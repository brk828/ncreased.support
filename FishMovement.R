# B. Kesner 31 May 2023
# Script to produce movement data on any PIT tag with scanning records
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

ReachContacts <- BasinContacts %>%
  filter(Reach > 1) %>%
  select(Reach, PIT, DateTime, Latitude, Longitude, RiverKm, Zone = DecimalZone, Location, LID, ScanFY) %>%
  mutate(LatLong = paste0(Latitude, ",", Longitude))


Movement <- ReachContacts %>%
  arrange(PIT, DateTime) %>%
  group_by(PIT) %>%
  mutate(Distance = abs(RiverKm - lag(RiverKm, default = first(RiverKm)))) %>%
  ungroup %>%
  filter(Distance > 2 | is.na(Distance), 
         Latitude > 0, 
         Longitude < 0, 
         Latitude < 38,
         !grepl("^00000", PIT))

MovementMohave <- Movement %>%
  filter(Reach == 2)

MovementHavasu <- Movement %>%
  filter(Reach == 3)

MovementLower <- Movement %>%
  filter(Reach == 4)

# Write to Google Sheet for shiny app use.
packages(googlesheets4)

# If Google sheet access has not previously been established on current computer
# the first time the following line is run it will require Google Authentication
# After initial run, a login token will be stored in the dependencies folder
gs4_auth(cache = "dependencies", email = "brk828@gmail.com")

# Create a new sheet with the following line, update as needed.  
# This only needs to be run to create a new sheet

# sheet <- gs4_create("Movement", sheets = c("Lake Mohave", "Lake Havasu", "Lower River"))

# writing data to the sheet requires the sheetID which can be obtained by viewing the information stored
# in the sheet object

# sheet

# Change sheetID to match the ID found in sheet
sheetID <- "1R2bk1-b2oMPECh5jRJ1iRtxHYQldFD43Nol_46J_-SU"

# write sheet data
write_sheet(MovementMohave, sheetID, "Lake Mohave")
write_sheet(MovementHavasu, sheetID, "Lake Havasu")
write_sheet(MovementLower, sheetID, "Lower River")
