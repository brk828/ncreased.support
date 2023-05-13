# B. Kesner 12 May 2023
# Script to analyze and improve PITMFG and PITPrefix fields in scan_data
# Load useful lab functions
source("LabFunctions.R") # includes package loads for dplyr and lubridate

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

BasinMarksZone <- BasinContacts %>% 
  select(Reach, DecimalZone, Date, PIT) %>%
  filter(month(Date) < 3) %>%
  mutate(CensusYear = year(Date)) %>%
  group_by(CensusYear, Reach, DecimalZone, PIT) %>%
  summarise(Contacts = n(), FirstScan = min(Date), LastScan = max(Date)) %>%
  ungroup()

BasinCapturesZone <- BasinContacts %>% 
  select(Reach, DecimalZone, Date, PIT, ScanFY) %>%
  filter(month(Date) > 9| month(Date) < 5) %>%
  mutate(CensusYear = ScanFY - 1) %>%
  group_by(CensusYear, Reach, DecimalZone, PIT) %>%
  summarise(Contacts = n(), FirstScan = min(Date), LastScan = max(Date)) %>%
  ungroup()

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

rm(BasinCapturesZoneV, BasinMarksZoneV, BasinRecapturesZoneV)  

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
  