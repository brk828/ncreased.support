# B. Kesner adapted from C. Ehlo 3 May 2023

# Post-Release contact analysis using logistic regression.  This script will take confirmed release records
# for a given species and reach and determine if they survived to a certain time after release based on 
# PIT scanning records.  The resultant survival data will be used to estimate the size apparent survival
# relationship differentiated across release seasons.  The resultant probabilities are a combination of 
# survival and detectability and are expected to be used as more to compare relationships across seasons, species
# and reaches and not to estimate actual survival. 

# Load useful lab functions
source("LabFunctions.R")

# remove unnecessary functions
rm(euclid, split_hourly, download_nfwg, download_backwater)

# Before running any script, declare the Reach you are interested in
MSCPReach <- 2

AdultTL <- 380 #cutoff for juveniles/adults not used in graphs yet
SurvivalDAL <- 60 # Day cutoff for post-stocking survival
ContactLimit <- 1000 # contact cutoff to remove likely dead tags
Sp <- "XYTE" # Species of interest GIEL bonytail, XYTE razorback
MinReleaseDate <- as.Date("2013-01-01") # Limit to fish release on or after this date
EarlySpringStartMonth <- 2 # Start of Early Spring month number (2 = February)
LateSpringStartMonth <- 4 # Start of Late Spring
SummerStartMonth <- 6 # Start of Summer/Fall usually few fish are released
WinterStartMonth <- 11 # Start of Winter month

# Load data workspace or downlod and load if more than 7 days old
if(file.exists("data/BasinScanningIndex.RData")){
  data_info <- file.info("data/BasinScanningIndex.RData")
  data_date <- as.Date(data_info$mtime)
  if(data_date>Sys.Date() - 7){
    load("data/BasinScanningIndex.RData")
    } else {
      download_basin("data")
      load("data/BasinScanningIndex.RData")
    }
} else {
  download_basin("data")
  load("data/BasinScanningIndex.RData")
}

# Load the R workspace file into the R session
# If this file is not already in the data subfolder, run DownloadWorkspace.R first

# packages function will install and load libraries when needed, if already installed it will just load
# packages function should be a part of the BasinScanningIndex workspace
packages(ggplot2)
packages(dplyr)     # data manipulation
packages(magrittr)  # allows use of %<>% assignment pipe
packages(glmmTMB) # General linear mixed model analysis built on TMB automatic differentiation engine
packages(lubridate)


#Create Bonytail release table adding size class, release month, and ensuring TL is present.
SpReachReleases <- BasinReleases %>% 
  select(PITIndex = PIT1, Reach, Species, ReleaseYear, ReleaseZone, ReleaseDate, ReleaseTL, TLCM, 
         ReleaseLocation, StockingID) %>%
  filter(Species == Sp, Reach == MSCPReach, ReleaseDate>=MinReleaseDate, !is.na(ReleaseTL)) %>%
  mutate(ReleaseMonth = month(ReleaseDate), 
         SizeClass = case_when(ReleaseTL < AdultTL ~ 'Juvenile', ReleaseTL >= AdultTL ~ 'Adult'),
         Season = ifelse(ReleaseMonth >= WinterStartMonth | ReleaseMonth < EarlySpringStartMonth, "Winter", 
                ifelse(ReleaseMonth >= EarlySpringStartMonth & ReleaseMonth < LateSpringStartMonth, "Early Spring",
                       ifelse(ReleaseMonth >= LateSpringStartMonth & 
                                ReleaseMonth < SummerStartMonth, "Late Spring", "Summer-Fall"))))
         
 #        Season = case_when(ReleaseMonth %in% c("9", "10", "11", '12') ~ "Winter", 
  #                                   ReleaseMonth %in% c("1", "2", "3") ~ "Early Spring",
   #                                  ReleaseMonth %in% c("4", "5") ~ "Late Spring"))

rm(BasinReleases)  

SpReachPITIndex <- BasinPITIndex %>%
  filter(Reach == MSCPReach, Species == Sp, ReleaseDate>=MinReleaseDate)

# Join contacts with Index dataframe to match contacts with release record of given species
# grouping by PITIndex ensures that fish with more than one PIT tag scanned will be counted as one fish
SpReachContacts <- BasinContacts %>%
  inner_join(SpReachPITIndex %>% 
  select(PIT, PITIndex, ReleaseDate, ReleaseTL, TLCM, ReleaseYear, ReleaseZone, Reach), 
  by = c("PIT" = "PIT", "Reach" = "Reach")) %>%
  mutate(DAL = as.integer(difftime(Date, ReleaseDate, unit = 'days'))) %>%
  group_by(PITIndex) %>%
  summarise(MaxDAL = max(DAL), Contacts = n()) %>%
  ungroup

  

SpeciesSurvival <- SpReachReleases %>% 
  select(ReleaseMonth, ReleaseYear, Season, SizeClass, PITIndex, ReleaseTL, TLCM) %>%
  left_join(SpReachContacts %>% select(MaxDAL, Contacts, PITIndex), 
            by = "PITIndex") %>% 
  mutate(MaxDAL = if_else(MaxDAL<0|is.na(MaxDAL),0,MaxDAL), 
         Alive = if_else(MaxDAL>=SurvivalDAL & Contacts < ContactLimit, 1, 0))

#zero inflated model by DAL and Season.  Could cluster by Zone and also add size at release
DALModel <- glmmTMB(Alive ~ ReleaseTL + Season,
                     family = binomial(link = 'logit'), 
                     data = SpeciesSurvival)

#creating a predictions data.frame from model for graphing
Predictions <- expand.grid(ReleaseTL = as.integer(seq(min(SpeciesSurvival$ReleaseTL), 
                                    max(SpeciesSurvival$ReleaseTL), length.out = 100)), 
                           Season = c("Winter", "Early Spring", "Late Spring"))
                           

Predictors <- predict(DALModel, newdata = Predictions, type = "response", se.fit = TRUE)
lowerCI <- Predictors$fit - (1.96 * Predictors$se.fit)
upperCI <- Predictors$fit + (1.96 * Predictors$se.fit)
Predicted <- cbind(Predictions, Predictors, lowerCI, upperCI)

DALGraph <- ggplot(Predicted, aes(x = ReleaseTL, y = fit, color = Season)) + 
  geom_line(linewidth = 1) + 
 labs(x = 'Total Length (mm)', y = 'Detection Probability', color = 'Season') + 
 theme(plot.margin = margin(.75,.75,.75,.75, unit = 'cm'), 
     axis.title.x = element_text(vjust = -2), axis.title.y = element_text(vjust = 5)) +
 geom_ribbon(aes(x = ReleaseTL, ymin = lowerCI, ymax = upperCI, fill = Season), alpha = 0.3)

DALGraph

Survived <- SpeciesSurvival %>% 
  filter(Alive == 1)

hist <- ggplot(SpeciesSurvival, aes(x = ReleaseTL, fill = Alive)) + 
  geom_histogram()  + facet_grid(Alive ~.)
hist


