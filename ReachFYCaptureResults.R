# B. Kesner 12 May 2023
# Script to produce Reach and zone population estimates based on PIT scanning
# Load useful lab functions

# Assign Study Reach
StudyReach = 3
StartDate = "2023-01-01"
EndDate = "2024-04-30"
source("LabFunctions.R")

packages(dplyr)
packages(lubridate)

# remove unnecessary functions
rm(euclid, split_hourly, download_backwater)

# LabFunctions has a timout setting, but isn't retained
options(timeout = 400)

# Load data workspace or downlod and load if more than 7 days old
if(file.exists("data/NFWGTable.RData")){
  data_info <- file.info("data/NFWGTable.RData")
  data_date <- as.Date(data_info$mtime)
  if(data_date>Sys.Date() - 7){
    load("data/NFWGTable.RData")
  } else {
    download_nfwg("data")
    load("data/NFWGTable.RData")
  }
} else {
  download_nfwg("data")
  load("data/NFWGTable.RData")
}

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


rm(download_nfwg, download_basin)
# Reduce data to Study Reach, checking to ensure there are no orphans

ReachTagging <- NFWGTable %>% filter(Status == "Capture"|Status == "Release", 
                                      Recapture == "N",
                                      Reach == StudyReach) %>%
select(TaggingEvent = Status, TaggingDate = CollectionDate, TaggingZone = DecimalZone, 
       TaggingLocation = Location, FirstPIT, TaggingMethod = Method, TaggingTL = TL) # %>%
#   inner_join(BasinPITIndex %>% select(PITIndex), by = c("FirstPIT" = "PITIndex"))

ReachCaptures <- NFWGTable %>% filter(Status == "Capture", 
                                      Reach == StudyReach, 
                                      CollectionDate >= as.Date(StartDate), 
                                      CollectionDate <= as.Date(EndDate)) %>%
  select(Reach, Zone = DecimalZone, PIT = FirstPIT, PIT134 = First134PIT, CollectionLocation = Location, 
         CollectionDate, Connection = SurfaceConnection, Species, Sex, TL, Method) %>%
  arrange(CollectionDate, PIT) %>% 
#  left_join(BasinPITIndex %>% select(PITIndex, DateVerified, ReleaseDate, ReleaseLocation, ReleaseTL, FirstCensus),
#            by = c("PIT" = "PITIndex")) %>%
  left_join(ReachTagging, by = c("PIT" = "FirstPIT"))

packages(openxlsx) # package openxlsx is required
wb <- createWorkbook() # creates object to hold workbook sheets
addWorksheet(wb, "ReachCaptures") # add worksheet
# addWorksheet(wb, "ReachReleases") # add worksheet

writeData(wb, "ReachCaptures", ReachCaptures) # write dataframe (second argument) to worksheet
# writeData(wb, "ReachReleases", ReachReleases) # write dataframe (second argument) to worksheet

# Last step is to save workbook. Give a useful name.  Adding date time ensures this step
# will not overwrite a previous version.  Location should be output folder
saveWorkbook(wb, paste0("output/Reach",StudyReach, "HandlingRecords",
                        format(Sys.time(), "%Y%m%d"), ".xlsx"),
             overwrite = TRUE)
