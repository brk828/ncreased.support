# B. Kesner 12 May 2023
# Script to produce Reach and zone population estimates based on PIT scanning
# Load useful lab functions
source("LabFunctions.R")
packages(dplyr)
packages(lubridate)
# remove unnecessary functions
rm(euclid, split_hourly, download_nfwg, download_backwater)

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

rm(download_basin, data_info, data_date, Unit, TripTable, ReachTable, BasinEffort)

# Marks are restricted to January or February of the census year (month < 3)
# The census year is equal to the year of scanning
BasinMarksZone <- BasinContacts %>% 
  select(Reach, DecimalZone, Date, PIT) %>%
  filter(month(Date) < 3) %>%
  mutate(CensusYear = year(Date)) %>%
  group_by(CensusYear, Reach, DecimalZone, PIT) %>%
  summarise(Contacts = n(), FirstScan = min(Date), LastScan = max(Date)) %>%
  ungroup()

# Captures are restricted to October through April (month > 9 or month < 5)
# The census year is the year in which the October scanning begins but a year less
# than scans for January through April, Fiscal Year is a year ahead of this schedule 
BasinCapturesZone <- BasinContacts %>% 
  select(Reach, DecimalZone, Date, PIT, ScanFY) %>%
  filter(month(Date) > 9| month(Date) < 5) %>%
  mutate(CensusYear = ScanFY - 1) %>%
  group_by(CensusYear, Reach, DecimalZone, PIT) %>%
  summarise(Contacts = n(), FirstScan = min(Date), LastScan = max(Date)) %>%
  ungroup()

# Verified marks include a record in the BasinPITIndex where the FirstCensus field
# determines the first year the PIT tag meets the criteria to be included in an estimate
BasinMarksZoneV <- BasinMarksZone %>%
  inner_join(BasinPITIndex %>% 
               select(PIT, PITIndex, Species, Reach, FirstCensus), 
             by = c("PIT" = "PIT", "Reach" = "Reach")) %>%
  filter(CensusYear >= FirstCensus) %>%
  group_by(Reach, DecimalZone, CensusYear, Species, PITIndex) %>%
  summarise(PITContacted = n(), FirstPIT = min(PIT), SecondPIT = max(PIT)) %>%
  ungroup()

BasinCapturesZoneV <- BasinCapturesZone %>%
  inner_join(BasinPITIndex %>% 
               select(PIT, PITIndex, Species, Reach, FirstCensus), 
             by = c("PIT" = "PIT", "Reach" = "Reach")) %>%
  filter(CensusYear >= FirstCensus) %>%
  group_by(Reach, DecimalZone, CensusYear, Species, PITIndex) %>%
  summarise(PITContacted = n(), FirstPIT = min(PIT), SecondPIT = max(PIT)) %>%
  ungroup()

BasinRecapturesZoneV <- BasinMarksZoneV %>%
  select(Reach, DecimalZone, CensusYear, Species, PITIndex) %>%
  inner_join(BasinCapturesZoneV %>%
               select(DecimalZone, CensusYear, PITIndex), 
             by = c("DecimalZone" = "DecimalZone",
                    "CensusYear" = "CensusYear", 
                    "PITIndex" = "PITIndex"))

# Reach specific estimates require any contact that matches by Reach
BasinMarksReachV <- BasinMarksZoneV %>%
  group_by(Reach, CensusYear, Species, PITIndex) %>%
  summarise(ContactReaches = n(), 
            MinZone = min(DecimalZone), 
            MaxZone = max(DecimalZone)) %>%
  ungroup()

BasinCapturesReachV <- BasinCapturesZoneV %>%
  group_by(Reach, CensusYear, Species, PITIndex) %>%
  summarise(ContactReaches = n(), 
            MinZone = min(DecimalZone), 
            MaxZone = max(DecimalZone)) %>%
  ungroup()
  

BasinRecapturesReachV <- BasinMarksReachV %>%
  inner_join(BasinCapturesReachV %>%
               select(Reach, CensusYear, PITIndex), 
             by = c("Reach" = "Reach",
                    "CensusYear" = "CensusYear", 
                    "PITIndex" = "PITIndex"))

rm(BasinContacts, BasinCapturesZone, BasinMarksZone)

ZoneMark <- BasinMarksZoneV %>%
  group_by(Species, DecimalZone, CensusYear) %>%
  summarise(M = n_distinct(PITIndex)) %>%
  ungroup()

ZoneCapture <- BasinCapturesZoneV %>%
  group_by(Species, DecimalZone, CensusYear) %>%
  summarise(C = n_distinct(PITIndex)) %>%
  ungroup()

ZoneRecapture <- BasinRecapturesZoneV %>%
  group_by(Species, DecimalZone, CensusYear) %>%
  summarise(R = n_distinct(PITIndex)) %>%
  ungroup()

ReachMark <- BasinMarksReachV %>%
  group_by(Species, Reach, CensusYear) %>%
  summarise(M = n_distinct(PITIndex)) %>%
  ungroup()

ReachCapture <- BasinCapturesReachV %>%
  group_by(Species, Reach, CensusYear) %>%
  summarise(C = n_distinct(PITIndex)) %>%
  ungroup()

ReachRecapture <- BasinRecapturesReachV %>%
  group_by(Species, Reach, CensusYear) %>%
  summarise(R = n_distinct(PITIndex)) %>%
  ungroup()

rm(BasinCapturesZoneV, BasinMarksZoneV, BasinRecapturesZoneV, BasinCapturesReachV, BasinRecapturesReachV, 
   BasinMarksReachV)  

ZoneEstimates <- ZoneMark %>%
  inner_join(ZoneCapture, by = c("Species" = "Species",
                                 "DecimalZone" = "DecimalZone",
                                 "CensusYear" = "CensusYear")) %>%
  inner_join(ZoneRecapture, by = c("Species" = "Species",
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

# Cleanup data and use quasipoisson for confidence intervals when recaptures are 30 or fewer otherwise
# use normal distribution estimates.
ZoneEstimates <- ZoneEstimates %>%
  mutate(Year = as.integer(CensusYear),
         Estimate = as.numeric(Estimate),
         Zone = as.factor(DecimalZone),
         LowerCI = ifelse(R<=30, LowerQP95CI, LowerN95CI),
         UpperCI = ifelse(R<=30, UpperQP95CI, UpperN95CI)) %>%
  select(Species, Year, Zone, M, C, R, Estimate, SE, LowerCI, UpperCI)


ReachEstimates <- ReachMark %>%
  inner_join(ReachCapture, by = c("Species" = "Species",
                                 "Reach" = "Reach",
                                 "CensusYear" = "CensusYear")) %>%
  inner_join(ReachRecapture, by = c("Species" = "Species",
                                   "Reach" = "Reach",
                                   "CensusYear" = "CensusYear")) %>%
  filter(R>3) %>%
  arrange(Reach, Species, CensusYear) %>%
  mutate(LowerBoundR = qpois(0.025, R),
         UpperBoundR = qpois(0.975, R),
         Estimate = as.integer(((M + 1) * (C + 1))/(R + 1))) %>%
  mutate(LowerQP95CI = as.integer(((M + 1) * (C + 1))/(UpperBoundR + 1)),
         UpperQP95CI = as.integer(((M + 1) * (C + 1))/(LowerBoundR + 1)),
         SE = as.integer((sqrt(((M+1)^2*(C+1)*(C-R)/((R+2)*(R+1)^2))))),
         LowerN95CI = as.integer(Estimate-(1.96*SE)),
         UpperN95CI = as.integer(Estimate+(1.96*SE))) %>%
  na.omit()

# Cleanup data and use quasipoisson for confidence intervals when recaptures are 30 or fewer otherwise
# use normal distribution estimates.
ReachEstimates <- ReachEstimates %>%
  mutate(Year = as.integer(CensusYear),
         Estimate = as.numeric(Estimate),
         Reach = as.factor(Reach),
         LowerCI = ifelse(R<=30, LowerQP95CI, LowerN95CI),
         UpperCI = ifelse(R<=30, UpperQP95CI, UpperN95CI)) %>%
  select(Species, Year, Reach, M, C, R, Estimate, SE, LowerCI, UpperCI)


write.csv(ZoneEstimates, file="output/ZonePopulationEstimates.csv")
write.csv(ReachEstimates, file="output/ReachPopulationEstimates.csv")
write.csv(ReachEstimates, file="ReachEstimatesGraph/ReachPopulationEstimates.csv")
#
packages(googlesheets4)

# If Google sheet access has not previously been established on current computer
# the first time the following line is run it will require Google Authentication
# After initial run, a login token will be stored in the dependencies folder
gs4_auth(cache = "dependencies", email = "brk828@gmail.com")

# Create a new sheet with the following line, update as needed.  
# This only needs to be run to create a new sheet

# sheet <- gs4_create("PopulationEstimates", sheets = c("Zones", "Reaches"))

# writing data to the sheet requires the sheetID which can be obtained by viewing the information stored
# in the sheet object

# sheet

# Change sheetID to match the ID found in sheet
sheetID <- "1ub6stAPzrdNUR0K3L9sFj-5JETRgjBSfTIvhXVdIEpg"

# write sheet data
write_sheet(ReachEstimates, sheetID, "Reaches")
write_sheet(ZoneEstimates, sheetID, "Zones")
