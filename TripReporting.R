# B. Kesner 22 Feb 2024
# Trip reporting
# Request Study Reach
StudyReach <- as.integer(readline(prompt="Study Reach (1 through 4): "))

source("LabFunctions.R")
packages(dplyr)
packages(lubridate)
packages(zoo)

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

ReachTrips <- TripTable %>% 
  filter(TripReach == StudyReach, TripStart > (Sys.Date() - months(3))) %>%
  arrange(desc(TripStart)) %>% select(-TripReach, Isolated)
rm(TripTable)

TripContacts <- BasinContacts %>%
  inner_join(ReachTrips %>% select(TID), by = c("TripID" = "TID")) %>%
  left_join(BasinPITIndex %>% 
              select(PIT, PITIndex, Species, Sex, DateVerified,
                     ReleaseDate, ReleaseLocation), 
            by = c("PIT" = "PIT")) %>%
  group_by(Species, TripID, Location, PITIndex, PIT, DateVerified) %>%
  summarise(Contacts=n()) %>%
  ungroup()

TripLocationContactSummary <- TripContacts %>%
  filter(!is.na(PITIndex)) %>%
  group_by(TripID, Species, Location) %>%
  summarise(UniqueFish = n_distinct(PITIndex)) %>%
  ungroup()

TripSummary <- ReachTrips %>%
  left_join(TripContacts %>%
              group_by(TripID) %>%
              summarise(UniquePIT = n_distinct(PIT), 
                        UniqueFish = n_distinct(PITIndex)) %>%
              ungroup(), by = c("TID" = "TripID"))

