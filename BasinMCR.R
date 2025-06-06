# B. Kesner 26 Feb 2024
# Script to produce basic MCR data for analysis, basin wide and indiscriminate

# Request Species
Sp <- "XYTE"
Rch <- 3


source("LabFunctions.R")
packages(dplyr)
packages(lubridate)
packages(zoo)
packages(tidyr)
packages(stringr)

# remove unnecessary functions
rm(euclid, split_hourly, download_nfwg, download_backwater)

# LabFunctions has a timout setting, but isn't retained
options(timeout = 400)

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

rm(download_basin, data_info, data_date, Unit, ReachTable)

# Reduce data to Species

SpReleases <- BasinReleases %>% 
  filter(Species == Sp, 
         ReleaseYear > 2012,
         ReleaseYear < year(Sys.Date())-8,
         Reach == Rch) 

# rm(BasinReleases)

SpContacts <- BasinContacts %>% 
  inner_join(SpReleases %>% select(PIT1, ReleaseDate, ReleaseYear, ReleaseTL), 
             by = c("PIT" = "PIT1")) %>%
  mutate(DAL = as.integer((Date - as.Date(ReleaseDate))),
         ReleaseMonth = as.integer(month(ReleaseDate)),
         ScanMonth = as.integer(month(Date)),
         ScanYear = as.integer(year(Date))) %>%
  select(ReleaseYear, ReleaseMonth, PIT, ScanYear, ScanMonth, ReleaseTL, DAL) %>%
  arrange(ReleaseYear, ReleaseMonth, ScanYear, ScanMonth)

#rm(BasinContacts)

SpContactHistory <- SpContacts %>%
  mutate(OneYear = ifelse(DAL >= 365 & DAL <= 700,1,0),
         TwoYear = ifelse(DAL >= 730 & DAL <= 1065,1,0),
         ThreeYear = ifelse(DAL >= 1095 & DAL <= 1429,1,0),
         FourYear = ifelse(DAL > 1459 & DAL <= 1794, 1,0),
         FiveYear = ifelse(DAL > 1824,1,0)) %>%
  group_by(PIT) %>%
  summarise(OneYear = max(OneYear),
            TwoYear = max(TwoYear),
            ThreeYear = max(ThreeYear),
            FourYear = max(FourYear),
            FiveYear = max(FiveYear)) %>%
  ungroup()

SpHistory <- SpReleases %>%
  mutate(Release = 1) %>% 
  select(PIT1, Release) %>%
  left_join(SpContactHistory, by = c("PIT1" = "PIT"))

rm(SpContactHistory)
# Replace all NAs with zeros. This will also zero NA TLs so must remove those
# if running TL as a covariate in MARK.
SpHistory[is.na(SpHistory)] <- 0    

# Create dataframe with relevant covariates for Rmark analysis
Spch <- unite(SpHistory, ch, -1, sep="", remove = TRUE) %>%
  inner_join(SpReleases %>% select(PIT1, Year = ReleaseYear, TL = ReleaseTL, Reach),
             by = c("PIT1" = "PIT1")) %>%
  mutate(ch = as.character(ch), Year = as.factor(Year), Reach = as.factor(Reach))


packages(RMark)

dp=RMark::process.data(Spch,model="CJS")
ddl=RMark::make.design.data(dp)

# Building a max 4 age structure
ddl$Phi$age2 <- ifelse(ddl$Phi$Age >4,5,ddl$Phi$age)
ddl$Phi$age2 <- as.factor(ddl$Phi$age2)

# Create two time sample structure as earlier ages have smaller scan windows
ddl$p$time2 <- ifelse(ddl$p$Age > 2, 3, 1)
ddl$p$time2 <- as.factor(ddl$p$time2)

Phi.age2 = list(formula=~age2) 
p.dot = list(formula=~1)
p.time2 = list(formula=~time2)

AgeModel <- mark(data=dp,ddl=ddl,model.parameters=list(Phi=Phi.age2,p=p.time2))

AgeResults <- AgeModel$results$real %>%
  tibble::rownames_to_column(var = "parameter")  # Store row names in a column

# Extract survival (S) estimates by age
AgeSurvival <- AgeResults %>%
  filter(str_starts(parameter, "Phi")) %>%
  mutate(age = row_number(),  # Assign sequential age numbers
         parameter = "Survival")  # Rename for clarity

# Extract recapture (p) estimates by time
TimeRecapture <- AgeResults %>%
  filter(str_starts(parameter, "p")) %>%
  mutate(time = row_number(),  # Assign sequential time numbers
         parameter = "Recapture")  # Rename for clarity


############################# marked package stuff, can't get to work yet.
# TimeIntervals <- c(1, 1, 1, 1, 1, 1)

# packages(marked)

# Quick run
# cjs_model <- crm(data = Spch, formula = list(phi = ~ AGE, p = ~ .), maxage = 5)

# SpProcessed <- process.data(Spch)

# SpDesign <- make.design.data(SpProcessed)

# Phi.fixed <- list(formula=~.)

# Phi.age <- list(formula=~age)
# p.fixed <- list(formula=~.)

# CJSModel <- crm(SpProcessed, SpDesign,
#                model.parameters = list(Phi = Phi.age,
#                                        p = p.fixed),
#                time.intervals = TimeIntervals)

#predict(CJSModel)


