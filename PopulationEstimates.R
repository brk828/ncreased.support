# B. Kesner 12 May 2023
# Script to analyze and improve PITMFG and PITPrefix fields in scan_data
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
  arrange(Species, DecimalZone, CensusYear) %>%
  mutate(LowerBoundR = qpois(0.05, R),
         UpperBoundR = qpois(0.95, R),
         Estimate = ((M + 1) * (C + 1))/(R + 1)) %>%
  mutate(Lower95CI = ((M + 1) * (C + 1))/(UpperBoundR + 1),
         Upper95CI = ((M + 1) * (C + 1))/(LowerBoundR + 1))

ReachEstimates <- ReachMark %>%
  inner_join(ReachCapture, by = c("Species" = "Species",
                                 "Reach" = "Reach",
                                 "CensusYear" = "CensusYear")) %>%
  inner_join(ReachRecapture, by = c("Species" = "Species",
                                   "Reach" = "Reach",
                                   "CensusYear" = "CensusYear")) %>%
  filter(R>3) %>%
  arrange(Species, Reach, CensusYear) %>%
  mutate(LowerBoundR = qpois(0.05, R),
         UpperBoundR = qpois(0.95, R),
         Estimate = ((M + 1) * (C + 1))/(R + 1)) %>%
  mutate(Lower95CI = ((M + 1) * (C + 1))/(UpperBoundR + 1),
         Upper95CI = ((M + 1) * (C + 1))/(LowerBoundR + 1))

rm(ReachMark, ReachRecapture, ReachCapture, ZoneCapture, ZoneMark, ZoneRecapture)