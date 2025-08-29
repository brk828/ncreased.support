# B. Kesner 12 May 2023
# Script to produce Reach and zone population estimates based on PIT scanning
# Load useful lab functions
source("LabFunctions.R")
packages(dplyr)
packages(lubridate)
packages(data.table) # speeding indexing
# remove unnecessary functions
rm(euclid, split_hourly, download_backwater)

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

rm(download_basin, BasinEffort)

# Constrained sites usually not included in estimates (e.g. Topock Marsh)
BasinContactsdt <- as.data.table(BasinContacts %>% filter(SurfaceConnection!= "constrained"))


BasinMarksZoneV <- (BasinContactsdt %>% 
                     filter(month(Date) > 9| month(Date) < 5) %>%
                     mutate(CensusYear = ScanFY))[order(Date), .SD[1], by = c("CensusYear", "Reach", "DecimalZone", "PITIndex")] %>%
  filter(available_date < as.Date(paste0(CensusYear-1, "-05-01")))

BasinCapturesZoneV <- (BasinContactsdt %>% 
                      filter(month(Date) < 3) %>%
                      mutate(CensusYear = ScanFY-1))[order(Date), .SD[1], by = c("CensusYear", "Reach", "DecimalZone", "PITIndex")] %>%
  filter(available_date < as.Date(paste0(CensusYear-1, "-05-01")))


BasinRecapturesZoneV <- BasinMarksZoneV %>%
  select(Reach, DecimalZone, CensusYear, species, PITIndex) %>%
  inner_join(BasinCapturesZoneV %>%
               select(DecimalZone, CensusYear, PITIndex), 
             by = c("DecimalZone" = "DecimalZone",
                    "CensusYear" = "CensusYear", 
                    "PITIndex" = "PITIndex"))

ZoneMark <- BasinMarksZoneV %>%
  group_by(species, DecimalZone, CensusYear) %>%
  summarise(M = n_distinct(PITIndex)) %>%
  ungroup()

ZoneCapture <- BasinCapturesZoneV %>%
  group_by(species, DecimalZone, CensusYear) %>%
  summarise(C = n_distinct(PITIndex)) %>%
  ungroup()

ZoneRecapture <- BasinRecapturesZoneV %>%
  group_by(species, DecimalZone, CensusYear) %>%
  summarise(R = n_distinct(PITIndex)) %>%
  ungroup()


rm(BasinMarksZoneV, BasinRecapturesZoneV, BasinCapturesReachV, BasinRecapturesReachV, 
   BasinMarksReachV)  

ZoneEstimates <- ZoneMark %>%
  inner_join(ZoneCapture, by = c("species" = "species",
                                 "DecimalZone" = "DecimalZone",
                                 "CensusYear" = "CensusYear")) %>%
  inner_join(ZoneRecapture, by = c("species" = "species",
                                 "DecimalZone" = "DecimalZone",
                                 "CensusYear" = "CensusYear")) %>%
  filter(R>3) %>%
  arrange(DecimalZone, species, CensusYear) %>%
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
ZoneEstimatesXYTE <- ZoneEstimates %>%
  filter(species == "XYTE") %>%
  inner_join(Zone %>%
               select(DecimalZone, ZoneName = Name, ZoneDescription = Description),
             by = "DecimalZone") %>%
  mutate(Year = as.integer(CensusYear),
         Estimate = as.numeric(Estimate),
         DecimalZone = as.factor(DecimalZone),
         LowerCI = ifelse(R<=30, LowerQP95CI, LowerN95CI),
         UpperCI = ifelse(R<=30, UpperQP95CI, UpperN95CI)) %>%
  select(Species, Year, DecimalZone, ZoneName, ZoneDescription, 
         M, C, R, Estimate, SE, LowerCI, UpperCI)

ZoneCleanXYTE <- BasinCapturesZoneV %>% 
  filter(!is.na(TLCM), !is.na(ReleaseKm), ReleaseKm > 0, CensusYear == MaxCensusYear,
         Species == 'XYTE') 

ZoneTLCMXYTE <- ZoneCleanXYTE %>%
  group_by(DecimalZone, CensusYear, ReleaseYear, TLCM) %>%
  summarise(Count = n_distinct(PITIndex)) %>%
  inner_join(Zone %>%
               select(DecimalZone, ZoneName = Name, ZoneDescription = Description),
             by = "DecimalZone") %>%
  ungroup()

ReleaseTLCMXYTE <- BasinReleases %>%
  filter(Species == "XYTE", !is.na(TLCM), ReleaseYear < MaxCensusYear) %>%
  group_by(ReleaseZone, ReleaseYear, TLCM) %>%
  summarise(Count = n_distinct(PIT1)) %>%
  inner_join(Zone %>%
               select(DecimalZone, ZoneName = Name, ZoneDescription = Description),
             by = c("ReleaseZone" = "DecimalZone")) %>%
  ungroup()

ZoneReleaseKmXYTE <- ZoneCleanXYTE %>%
  group_by(DecimalZone, CensusYear, ReleaseYear, ReleaseKm) %>%
  summarise(Count = n_distinct(PITIndex)) %>%
  inner_join(Zone %>%
               select(DecimalZone, ZoneName = Name, ZoneDescription = Description),
             by = "DecimalZone") %>%
  ungroup()

write.csv(ZoneEstimates, file="output/ZonePopulationEstimates.csv")
write.csv(ReachEstimates, file="output/ReachPopulationEstimates.csv")

# Write to Google Sheet for shiny app use.
packages(googlesheets4)

# If Google sheet access has not previously been established on current computer
# the first time the following line is run it will require Google Authentication
# After initial run, a login token will be stored in the dependencies folder
# If re-authorization is required, delete the cache file in dependencies subfolder and rerun gs4_auth
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
write_sheet(ReachEstimatesClean, sheetID, "Reaches")
write_sheet(ZoneEstimatesXYTE, sheetID, "Zones")
write_sheet(ZoneReleaseKmXYTE, sheetID, "ReleaseKmSummary")
write_sheet(ZoneTLCMXYTE, sheetID, "ReleaseTLSummary")
write_sheet(ReleaseTLCMXYTE, sheetID, "AllReleasesTL")

