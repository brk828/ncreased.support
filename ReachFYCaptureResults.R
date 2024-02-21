# B. Kesner 12 May 2023
# Script to produce Reach and zone population estimates based on PIT scanning
# Load useful lab functions

# Assign Study Reach
StudyReach = 3
StartDate = "2010-01-01"
EndDate = "2023-12-31"
Sp = "XYTE"
source("LabFunctions.R")

packages(dplyr)
packages(lubridate)

# remove unnecessary functions
rm(euclid, split_hourly, download_basin, download_backwater)

# LabFunctions has a timout setting, but isn't retained
options(timeout = 400)

# Load data workspace or downlod and load if more than 7 days old
if(file.exists("data/NFWGTable.RData")){
  data_info <- file.info("data/NFWGTable.RData")
  data_date <- as.Date(data_info$mtime)
  if(data_date>Sys.Date() - 7){
    load("data/NFWGTable.RData")
  } else {
    download_basin("data")
    load("data/NFWGTable.RData")
  }
} else {
  download_nfwg("data")
  load("data/NFWGTable.RData")
}

rm(download_nfwg)
# Reduce data to Study Reach, checking to ensure there are no orphans

ReachCaptures <- NFWGTable %>% filter(Status == "Capture", 
                                      Reach == StudyReach, 
                                      Species == Sp,
                                      CollectionDate >= as.Date(StartDate), 
                                      CollectionDate <= as.Date(EndDate)) %>%
  select(Species, CollectionDate, DecimalZone, Location, Latitude, Longitude,
         ReservoirKm, FirstPIT, First134PIT, Collector, Method) %>%
  arrange(CollectionDate)

ReachReleases <- NFWGTable %>% filter(Status == "Release", 
                                      Reach == StudyReach, 
                                      Species == Sp) %>%
  select(Species, CollectionDate, DecimalZone, Location, Latitude, Longitude,
         ReservoirKm, FirstPIT, First134PIT, Collector, Method)

packages(openxlsx) # package openxlsx is required
wb <- createWorkbook() # creates object to hold workbook sheets
addWorksheet(wb, "ReachCaptures") # add worksheet
addWorksheet(wb, "ReachReleases") # add worksheet

writeData(wb, "ReachCaptures", ReachCaptures) # write dataframe (second argument) to worksheet
writeData(wb, "ReachReleases", ReachReleases) # write dataframe (second argument) to worksheet

# Last step is to save workbook. Give a useful name.  Adding date time ensures this step
# will not overwrite a previous version.  Location should be output folder
saveWorkbook(wb, paste0("output/Reach",StudyReach, Sp, "HandlingRecords",
                        format(Sys.time(), "%Y%m%d"), ".xlsx"),
             overwrite = TRUE)
