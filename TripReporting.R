# B. Kesner 22 Feb 2024
# Trip reporting
# Request Study Reach
StudyReach <- as.integer(readline(prompt="Study Reach (1 through 4): "))

source("LabFunctions.R")
packages(dplyr)
packages(lubridate)
packages(zoo)
packages(openxlsx) # package openxlsx is required
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

ReachTrips <- TripTable %>% 
  filter(TripReach == StudyReach, 
         Isolated == "No", 
         TripStart > (Sys.Date() - months(6))) %>%
  arrange(desc(TripStart)) %>% select(-TripReach, Isolated)
rm(TripTable)

TripContacts <- BasinContacts %>%
  inner_join(ReachTrips %>% select(TID, TripStart, TripEnd), 
             by = c("TripID" = "TID")) %>%
  left_join(BasinPITIndex %>% 
              select(PIT, PITIndex, Species, Sex, DateVerified,
                     ReleaseDate, ReleaseLocation), 
            by = c("PIT" = "PIT")) %>%
  group_by(Species, TripID, TripStart, TripEnd, Location, PITIndex, PIT, DateVerified, 
           ReleaseDate, ReleaseLocation) %>%
  summarise(Contacts=n()) %>%
  ungroup()

TripLocationContactSummary <- TripContacts %>%
  group_by(TripID, TripStart, TripEnd, Species, Location) %>%
  summarise(UniqueFish = n_distinct(PITIndex, na.rm = TRUE)) %>%
  ungroup()

TripSummary <- ReachTrips %>%
  left_join(TripContacts %>%
              group_by(TripID) %>%
              summarise(UniquePIT = n_distinct(PIT), 
                        UniqueFish = n_distinct(PITIndex, na.rm = TRUE)) %>%
              ungroup(), by = c("TID" = "TripID"))

TripSummary[is.na(TripSummary)] <- 0

wb <- createWorkbook() # creates object to hold workbook sheets

addWorksheet(wb, "TripsSummary") # add worksheet
addWorksheet(wb, "LocationSummary") # add worksheet
addWorksheet(wb, "LocationContacts") # add worksheet

writeData(wb, "TripsSummary", TripSummary) # write dataframe
writeData(wb, "LocationSummary", TripLocationContactSummary) # write dataframe 
writeData(wb, "LocationContacts", TripContacts) # write dataframe 

saveWorkbook(wb, paste0("output/TripReportingR", StudyReach, "_",
                        format(Sys.time(), "%Y%m%d%H%M"), ".xlsx"))
