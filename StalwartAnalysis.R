# B. Kesner 30 May 2023
# Script to produce Google Sheets for stawart analysis
# Load useful lab functions
source("LabFunctions.R")
packages(dplyr)
packages(lubridate)
# remove unnecessary functions
rm(euclid, download_nfwg, download_backwater)

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

# Moving window for scanning data to confirm still alive which will be the two most recent 
# scanning fiscal years.  Sticking to fiscal years as it follows the spawning season
CurrentMonth <- month(Sys.Date())
CurrentFY <- ifelse(CurrentMonth>9, year(Sys.Date())+1,year(Sys.Date()))

# Set ReleaseFY cutoff, fish must be released before this Fiscal year
CutoffFY <- 2011

# Going to look for fish released with a 134 kHz PIT tag, and have reliably had that tag 
# contacted in the most recent 2 years of PIT scanning.
# Fiscal year window controls release date (before Fiscal Year 2011)
BasinStalwarts <- BasinReleases %>%
  mutate(LastScanFY = ifelse(month(LastScanPIT1)>9, year(LastScanPIT1)+1,year(LastScanPIT1))) %>%
  filter(ReleaseDate == DatePIT1, LastScanFY > CurrentFY - 2, 
         !is.na(ReleaseTL), ReleaseFY < CutoffFY, ReleaseKm > 0) %>%
  select(Reach, Species, Sex, ReleaseFY, ReleaseDate, ReleaseTL, TLCM, ReleaseZone, ReleaseLocation, 
         ReleaseLID, ReleaseKm, StockingID, RearingLocation, PIT = PIT1, LastScan = LastScanPIT1,
         LastScanFY, MaxDAL) %>%
  mutate(ReleaseMonth = month(ReleaseDate)) %>%
  arrange(Species, Reach, ReleaseFY, ReleaseMonth, PIT)

StalwartContacts <- BasinContacts %>%
  inner_join(BasinStalwarts %>% select(PIT, Reach, Sex, Species, ReleaseZone, ReleaseKm), 
             by = c("PIT" = "PIT", "Reach" = "Reach")) %>%
  filter(ScanFY > CutoffFY + 1, ScanFY <= CurrentFY - 2) %>%
  select(Reach, ReleaseZone, ReleaseKm, Species, Sex, PIT, EID, Date, ScanHr, DateTime, LID, Location, Latitude, Longitude, 
         ScanZone = DecimalZone, RiverKm, UnitType, ScanFY) %>%
  mutate(ScanMonth = month(DateTime), DispersalKm = abs(ReleaseKm - RiverKm))

StalwartSummary <- StalwartContacts %>%
  group_by(Reach, ReleaseZone, ReleaseKm, Species, Sex, ScanFY, ScanMonth, Latitude, Longitude, Location, 
           LID, RiverKm, ScanZone, PIT) %>%
  summarise(Contacts = n(), Dispersal = as.integer(mean(DispersalKm))) %>%
  ungroup() %>%
  mutate(LatLong = paste0(Latitude,",",Longitude))

# Write to Google Sheet for shiny app use.
packages(googlesheets4)

# If Google sheet access has not previously been established on current computer
# the first time the following line is run it will require Google Authentication
# After initial run, a login token will be stored in the dependencies folder
gs4_auth(cache = "dependencies", email = "brk828@gmail.com")

# Create a new sheet with the following line, update as needed.  
# This only needs to be run to create a new sheet

# sheet <- gs4_create("Stalwart Analysis", sheets = c("Releases", "ContactSummary"))

# writing data to the sheet requires the sheetID which can be obtained by viewing the information stored
# in the sheet object

# sheet

# Change sheetID to match the ID found in sheet
sheetID <- "1je8OoV8i8Va_noe7EKcBrSUv3SG2N0b6_BljRridn4M"

# write sheet data
write_sheet(BasinStalwarts, sheetID, "Releases")
write_sheet(StalwartSummary, sheetID, "ContactSummary")
