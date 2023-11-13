# B. Kesner 12 May 2023
# Script to produce Reach and zone population estimates based on PIT scanning
# Load useful lab functions

# Assign Study Reach
StudyReach = 3
StartDate = "2021-10-01"
EndDate = "2023-05-31"

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

ReachContacts <- BasinContacts %>% filter(Reach == StudyReach) %>%
  filter(Date >= as.Date(StartDate), Date <= as.Date(EndDate))

rm(BasinContacts)

ReachReleases <- BasinReleases %>% filter(Reach == StudyReach)
rm(BasinReleases)

ReachEffort <- BasinEffort %>% filter(Reach == StudyReach) %>%
  filter(Retrieve >= as.Date(StartDate), Retrieve <= as.Date(EndDate))

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

if(nrow(MixedReach)==0) {remove(MixedReach)
} else {
  warning("There are records in BasinPITIndex within the study reach where the release reach
  is different than study reach.  Please refer to the dataframe MixedReach for details")
  write.csv(MixedReach, file = "output/MixedReachContacts.csv")  
  ReachContacts <- ReachContacts %>% anti_join(MixedReach, by = "PIT")
}

ReachContacts <- ReachContacts %>%
  select(-PITMFG, -PITPrefix, -DateTime, -Time) %>%
  inner_join(BasinPITIndex %>%
               select(Species, ReleaseTL, Sex, PIT, PITIndex, ReleaseDate, ReleaseLocation, 
                      ReleaseReach = Reach, ReleaseZone, FirstCensus), 
             by = "PIT")

ReachContactsNoMark <- ReachContacts %>% filter(is.na(Species))

ReachContactsNoRelease <-  ReachContacts %>% filter(is.na(ReleaseDate)) %>%
  group_by(Species, PITIndex, PIT) %>%
  summarise(Contacts = n(), FirstScan = min(Date), LastScan = max(Date), 
            UpstreamExtent = max(RiverKm), DownstreamExtent = min(RiverKm)) %>%
  ungroup()

ReachContactsMort <- ReachContacts %>% 
  group_by(TripID, EID, PIT) %>%
  summarise(Contacts = n(), MaxDate = max(Date)) %>%
  ungroup() %>%
  left_join(ReachEffort %>% select(EID, Location, Deploy, Retrieve, ScanTime), by = "EID") %>%
  filter(Contacts/ScanTime > 0.9 & ScanTime>0 & MaxDate <= as.Date(Retrieve))
  
ReachContactsDL <- ReachContacts %>% 
  group_by(TripID, EID, ScanDate = Date, ScanZone = DecimalZone, Species, PIT, PITIndex, Sex, ScanLocation = Location, 
           RiverKm, Easting, Northing, ReleaseTL, ReleaseDate, ReleaseLocation, ReleaseZone) %>%
  summarise(Contacts = n()) %>%
  ungroup()

# Marks are restricted to January or February of the census year (month < 3)
# The census year is equal to the year of scanning
ReachMarks <- ReachContacts %>% 
  select(Reach, DecimalZone, Date, PITIndex, FirstCensus, CensusYear = ScanFY, Species) %>%
  filter(month(Date) < 3, CensusYear >= FirstCensus) %>%
  group_by(CensusYear, DecimalZone, PITIndex, Species) %>%
  summarise(Contacts = n(), FirstScan = min(Date), LastScan = max(Date)) %>%
  ungroup() 

ReachCaptures <- ReachContacts %>%
  mutate(CensusYear = ScanFY - 1) %>%
  select(Reach, DecimalZone, Date, PITIndex, FirstCensus, CensusYear, Species) %>%
  filter(month(Date) > 9| month(Date) < 5, CensusYear >= FirstCensus) %>%
  group_by(CensusYear, DecimalZone, PITIndex, Species) %>%
  summarise(Contacts = n(), FirstScan = min(Date), LastScan = max(Date)) %>%
  ungroup() 

ReachRecaptures <- ReachMarks %>%
  inner_join(ReachCaptures %>%
               select(CensusYear, PITIndex, Species, DecimalZone), 
             by = c("CensusYear" = "CensusYear", 
                    "PITIndex" = "PITIndex",
                    "Species" = "Species",
                    "DecimalZone" = "DecimalZone"))

Mark <- ReachMarks %>%
  group_by(Species, DecimalZone, CensusYear) %>%
  summarise(M = n_distinct(PITIndex)) %>%
  ungroup()

Capture <- ReachCaptures %>%
  group_by(Species, DecimalZone, CensusYear) %>%
  summarise(C = n_distinct(PITIndex)) %>%
  ungroup()

Recapture <- ReachRecaptures %>%
  group_by(Species, DecimalZone, CensusYear) %>%
  summarise(R = n_distinct(PITIndex)) %>%
  ungroup()

Estimates <- Mark %>%
  inner_join(Capture, by = c("Species" = "Species",
                             "DecimalZone" = "DecimalZone",
                             "CensusYear" = "CensusYear")) %>%
  inner_join(Recapture, by = c("Species" = "Species",
                               "DecimalZone" = "DecimalZone",
                               "CensusYear" = "CensusYear")) %>%
  filter(R>3) %>%
  arrange(DecimalZone, Species, CensusYear) %>%
  mutate(LowerBoundR = qpois(0.025, R),
         UpperBoundR = qpois(0.975, R),
         Estimate = as.integer(((M + 1) * (C + 1))/(R + 1))) %>%
  mutate(LowerQP95CI = as.integer(((M + 1) * (C + 1))/(UpperBoundR + 1)),
         UpperQP95CI = as.integer(((M + 1) * (C + 1))/(LowerBoundR + 1)),
         SE = as.integer((sqrt(((M+1)^2*(C+1)*(C-R)/((R+2)*(R+1)^2))))),
         LowerN95CI = as.integer(Estimate-(1.96*SE)),
         UpperN95CI = as.integer(Estimate+(1.96*SE))) %>%
  na.omit()

packages(openxlsx) # package openxlsx is required
wb <- createWorkbook() # creates object to hold workbook sheets
addWorksheet(wb, "ReachContacts") # add worksheet
addWorksheet(wb, "ReachEffort") # add worksheet
addWorksheet(wb, "FieldTaggedContacts") # add worksheet
addWorksheet(wb, "PotentialMortalities") # add worksheet
addWorksheet(wb, "PopulationEstimates") # add worksheet

writeData(wb, "ReachContacts", ReachContactsDL) # write dataframe (second argument) to worksheet
writeData(wb, "ReachEffort", ReachEffort) # write dataframe (second argument) to worksheet
writeData(wb, "FieldTaggedContacts", ReachContactsNoRelease) # write dataframe 
writeData(wb, "PotentialMortalities", ReachContactsMort) # write dataframe 
writeData(wb, "PopulationEstimates", Estimates) # write dataframe 



# Last step is to save workbook. Give a useful name.  Adding date time ensures this step
# will not overwrite a previous version.  Location should be output folder
saveWorkbook(wb, paste0("output/Reach",StudyReach, "Data",
                        format(Sys.time(), "%Y%m%d"), ".xlsx"),
             overwrite = TRUE)
