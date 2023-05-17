# B. Kesner 12 May 2023
# Script to analyze and improve PITMFG and PITPrefix fields in scan_data
# Load useful lab functions
source("LabFunctions.R")

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

rm(download_basin, data_info, data_date)

BasinPrefixIndex <- BasinContacts %>% 
  filter(!is.na(PITPrefix)) %>%
  group_by(PIT, PITPrefix, PITMFG) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(PIT) %>%
  top_n(n = 1, wt = count) %>%
  ungroup()

# Identify PIT scans that have more than one prefix
BasinPrefixIndexDupes <- BasinPrefixIndex %>%
  group_by(PIT) %>%
  summarise(count = n()) %>%
  filter(count>1) %>%
  ungroup()

# These prefixes need to be fixed in the scanning database
BasinContactsBadPrefix <- BasinContacts %>% 
  filter(!is.na(PITPrefix)) %>%
  group_by(PIT, PITMFG, PITPrefix) %>%
  summarise(contacts = n()) %>%
  inner_join(BasinPrefixIndex %>% 
               select(PIT, IndexPrefix = PITPrefix, IndexMFG = PITMFG, IndexCount = count),
             by = "PIT") %>%
  filter(PITPrefix != IndexPrefix)

Prefixes <- BasinPrefixIndex %>%
  group_by(PITPrefix) %>%
  summarise(count = sum(count)) %>%
  ungroup()

