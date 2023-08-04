# B. Kesner 12 May 2023
# Script to produce Reach and zone population estimates based on PIT scanning
# Load useful lab functions

# Assign Study Reach
StudyReach = 4

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

# Reduce data to Study Reach, checking to ensure there are no orphans

ReachContacts <- BasinContacts %>% filter(Reach == StudyReach)
rm(BasinContacts)

ReachReleases <- BasinReleases %>% filter(Reach == StudyReach)
rm(BasinReleases)

ReachEffort <- BasinEffort %>% filter(Reach == StudyReach)

ContactsWithoutEffort <- ReachContacts %>% anti_join(ReachEffort, by = "EID")

if(nrow(ContactsWithoutEffort)==0){
  rm(ContactsWithoutEffort, BasinEffort)
} else {
  warning("There are contacts in the Study Reach without a corresponding effort, 
             please check ContactsWithoutEffort dataframe for more information")
}

ReachTrip <- TripTable %>% filter(TripReach == StudyReach)

EffortWithoutTrip <- ReachEffort %>% anti_join(ReachTrip, by = c("TripID" = "TID"))

if(nrow(EffortWithoutTrip)==0){
  rm(EffortWithoutTrip, TripTable)
} else {
  warning("There are deployments in the Study Reach without a corresponding trip, 
             please check EffortWithoutTrip dataframe for more information")
}

# Code for tag length checking
BadTags<- ReachContacts %>%
  filter(nchar(PIT)!=10)

# If there are no PIT tags that are too long or too short, remove dataframe
if(count(BadTags)==0) {remove(BadTags)
} else {
  warning("There are PIT tags in the Scanning table that are not 10 digits, 
          please check BadTags dataframe for more information")
}

# Check for deployments with no or midnight deploy and retrieval times
ReachEffortNoTime <- ReachEffort %>%
  filter(Issue != "Y") %>%
  filter(format(Deploy, "%H:%M:%S") == "00:00:00", 
         format(Retrieve, "%H:%M:%S") == "00:00:00")
if(nrow(ReachEffortNoTime)==0) {remove(ReachEffortNoTime)
} else {
  warning("There are scan deployments with no recorded issue and no start or retrieval time, 
        please check dataframe ReachEffortNoTime for details.")
}

# Check for contact records outside of deployment window
# These efforts have a contact that is at least 2 hours outside
# of the Deployment Retrieval window. They probably shouldn't be used for CPE analysis
ReachScanBadTimeCPE <- ReachEffort %>% 
  select(TripID, EID, Deploy, Retrieve, Issue, Comments, Crew, Location, ScanTimeHrs) %>%
  left_join(ReachContacts %>% select(EID, PIT, DateTime), by = "EID") %>%
  filter(DateTime < Deploy - hours(2) | DateTime > Retrieve + hours(2)) %>% 
  group_by(TripID, EID, Deploy, Retrieve, Issue, Comments, Crew, Location, ScanTimeHrs) %>%
  summarise(LateScan = max(DateTime), EarlyScan = min(DateTime)) %>%
  ungroup()

# Check for contact records outside of deployment window
# These efforts have a contact that is at least 120 days outside
# of the Deployment Retrieval window. They probably shouldn't be used for population analyses
ReachScanBadTimePop <- ReachEffort %>% 
  select(TripID, EID, Deploy, Retrieve, Issue, Comments, Crew, Location, ScanTimeHrs) %>%
  left_join(ReachContacts %>% select(EID, PIT, DateTime), by = "EID") %>%
  filter(DateTime < Deploy - days(120) | DateTime > Retrieve + days(120)) %>% 
  group_by(TripID, EID, Deploy, Retrieve, Issue, Comments, Crew, Location, ScanTimeHrs) %>%
  summarise(LateScan = max(DateTime), EarlyScan = min(DateTime)) %>%
  ungroup()

if(nrow(ReachScanBadTimePop)==0) {remove(ReachScanBadTimePop)
} else {
  warning("There are scan deployments with contacts occuring more than 120 days outside of 
        deployment time please check dataframe ReachScanBadTimePop for details.")
}
# Date verified cannot be before ReleaseDate when Release Date is available.
ReachScanIndexMixDates <- BasinPITIndex %>% 
  filter(Reach == StudyReach) %>%
filter(DateVerified < ReleaseDate)

if(nrow(ReachScanIndexMixDates)==0) {remove(ReachScanIndexMixDates)
} else {
  warning("There are records in BasinPITIndex within the study reach where the Date the PIT tag
  is verified is before the date of release.  Please refer to the dataframe ReachScanIndexMixDates
          for details")
}

MixedReach <- ReachContacts %>% 
  select(TripID, EID, PIT, DateTime, Location, Reach, DecimalZone, EID) %>%
  inner_join(BasinPITIndex %>%
               select(PIT, PITIndex, ReleaseDate, ReleaseLocation, ReleaseReach = Reach, ReleaseZone), 
             by = "PIT") %>%
  filter(ReleaseReach != Reach)

write.csv(MixedReach, file = "output/MixedReachContacts.csv")  

ReachContacts <- ReachContacts %>% anti_join(MixedReach, by = "PIT")

ReachContacts <- ReachContacts %>%
  select(-PITMFG, -PITPrefix, -Date, -Time) %>%
  inner_join(BasinPITIndex %>%
               select(Species, ReleaseTL, Sex, PIT, PITIndex, ReleaseDate, ReleaseLocation, 
                      ReleaseReach = Reach, ReleaseZone, FirstCensus), 
             by = "PIT")

rm(BasinPITIndex)
