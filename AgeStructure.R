# B. Kesner 12 November 2023
# Script to develop age structure for any reach like Lake Mohave report

# Assign Study Reach if not established
if(exists("StudyReach") == FALSE) {StudyReach = 3}

# Assign first scanning FY if not established 
if(exists("FirstScanFY") == FALSE) {FirstScanFY = 2015}

if(exists("StudySpecies") == FALSE) {StudySpecies = "XYTE"}

TLCMCut = 40 # cutoff for release size classes
MinimumContacts = 200 # cutoff of total unique contacts per zone for inclusion in analysis

source("LabFunctions.R")
packages(dplyr)
packages(ggplot2)
packages(lubridate)
packages(lemon) # additional options for ggplot
packages(stringr)

CurrentFY <- if_else(month(Sys.Date())>9, year(Sys.Date())+1, year(Sys.Date()))
TLCutoffText <- paste0(TLCMCut, "0")

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

ReachReleaseSizes  <- BasinReleases %>%
  filter(Species == StudySpecies, Reach == StudyReach,
         ReleaseFY > 2007, ReleaseFY < CurrentFY - 2, !is.na(TLCM))%>%
  mutate(Size = factor(ifelse(TLCM >= TLCMCut, 
                       paste0(">=", TLCutoffText, " mm TL"), 
                       paste0("<", TLCutoffText, " mm TL")))) %>%
  dplyr::select( PIT1, ReleaseFY, ReleaseZone, TLCM, Size)

rm(BasinReleases)

SizePlot <- ggplot(ReachReleaseSizes, aes(ReleaseFY)) + 
  geom_bar(aes(fill = Size), colour="black") +
  scale_x_reverse(limits = c(CurrentFY-2, 2007), breaks = seq(CurrentFY-2, 2008, -2)) +
  scale_y_continuous(limits = c(0, 10000)) +
  scale_fill_manual(values = c('#FFFFFF','#000011')) +
  labs(x = "Release FY", y = "Number of Fish Released") +
  coord_capped_cart(bottom='both', left = 'both') +
  theme_bw(base_size = 15) + theme(panel.border = element_blank(), axis.line=element_line())
SizePlot <- SizePlot + facet_grid(ReleaseZone ~.)

png(paste0("output/Reach", StudyReach, "ReleaseCohorts.png"), width = 8, height = 6, units = 'in', res = 300)   
SizePlot
dev.off()

ReachYAL <- BasinContacts %>% 
  filter(Reach == StudyReach, ScanFY < CurrentFY)  %>%
  select(-PITMFG, -PITPrefix, -DateTime, -Time) %>%
  inner_join(BasinPITIndex %>%
               select(Species, TLCM, ReleaseFY, Sex, PIT, PITIndex,
                      ReleaseReach = Reach, ReleaseZone, FirstCensus), 
             by = "PIT") %>%
  mutate(ReleaseAge = ScanFY - ReleaseFY - 1) %>% 
  filter(Species == StudySpecies, !is.na(ReleaseFY), 
         ReleaseAge>0, ScanFY >= FirstScanFY, str_starts(ReleaseZone, paste0(StudyReach, "."))) %>%
  dplyr::select(PITIndex, ScanFY, ReleaseFY, Sex, TLCM, ReleaseZone, 
                ScanZone = DecimalZone, ReleaseAge) %>% 
  group_by(PITIndex, ScanFY, Sex, TLCM, ReleaseZone, ScanZone, ReleaseAge, ReleaseFY) %>%
  summarise(Contacts = n()) %>%
  ungroup()

rm(BasinContacts, BasinEffort)

Zones <- ReachYAL %>%
  group_by(ScanZone) %>%
  summarise(Count = n()) %>%
  filter(Count > MinimumContacts) 

Zone2 <- ifelse(StudyReach == 2, Zones$ScanZone[3], Zones$ScanZone[2])

ReachYALPlotData <- ReachYAL %>%
  inner_join(Zones %>% select(ScanZone), by = "ScanZone")

YearSplit <- median(unique(ReachYALPlotData$ScanFY))

ReachYALPlot1 <- ggplot(ReachYALPlotData %>% 
                         filter(ScanFY < YearSplit, ScanZone == Zones$ScanZone[1]), 
                       aes(ReleaseAge)) +
  geom_bar(aes(fill = ReleaseZone), colour="black") +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(0, 1000))  +
  labs(x = "Years at Large", y = "Number of Unique Fish Scanned") +
  coord_capped_cart(bottom='both', left = 'both') +
  theme_bw(base_size = 15) + theme(panel.border = element_blank(), axis.line=element_line())
ReachYALPlot1 <- ReachYALPlot1 + facet_grid(ScanFY ~.)

png(paste0("output/Reach", StudyReach, "YALPlot1.png"), width = 6, height = 4, units = 'in', res = 300)   
ReachYALPlot1
dev.off()

ReachYALPlot2 <- ggplot(ReachYALPlotData %>% 
                         filter(ScanFY >= YearSplit, ScanZone == Zones$ScanZone[1]), 
                       aes(ReleaseAge)) +
  geom_bar(aes(fill = ReleaseZone), colour="black") +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(0, 1000))  +
  labs(x = "Years at Large", y = "Number of Unique Fish Scanned") +
  coord_capped_cart(bottom='both', left = 'both') +
  theme_bw(base_size = 15) + theme(panel.border = element_blank(), axis.line=element_line())
ReachYALPlot2 <- ReachYALPlot2 + facet_grid(ScanFY ~.)

png(paste0("output/Reach", StudyReach, "YALPlot2.png"), width = 6, height = 4, units = 'in', res = 300)   
ReachYALPlot2
dev.off()

ReachYALPlot3 <- ggplot(ReachYALPlotData %>% 
                         filter(ScanFY < YearSplit, ScanZone == Zone2), 
                       aes(ReleaseAge)) +
  geom_bar(aes(fill = ReleaseZone), colour="black") +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(0, 1000))  +
  labs(x = "Years at Large", y = "Number of Unique Fish Scanned") +
  coord_capped_cart(bottom='both', left = 'both') +
  theme_bw(base_size = 15) + theme(panel.border = element_blank(), axis.line=element_line())
ReachYALPlot3 <- ReachYALPlot3 + facet_grid(ScanFY ~.)

png(paste0("output/Reach", StudyReach, "YALPlot3.png"), width = 6, height = 4, units = 'in', res = 300)   
ReachYALPlot3
dev.off()

ReachYALPlot4 <- ggplot(ReachYALPlotData %>% 
                         filter(ScanFY >= YearSplit, ScanZone == Zone2), 
                       aes(ReleaseAge)) +
  geom_bar(aes(fill = ReleaseZone), colour="black") +
  scale_x_continuous(limits = c(0, 26), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(0, 1000))  +
  labs(x = "Years at Large", y = "Number of Unique Fish Scanned") +
  coord_capped_cart(bottom='both', left = 'both') +
  theme_bw(base_size = 15) + theme(panel.border = element_blank(), axis.line=element_line())
ReachYALPlot4 <- ReachYALPlot4 + facet_grid(ScanFY ~.)

png(paste0("output/Reach", StudyReach, "YALPlot4.png"), width = 6, height = 4, units = 'in', res = 300)   
ReachYALPlot4
dev.off()

packages(openxlsx) # package openxlsx is required

wb <- createWorkbook() # creates object to hold workbook sheets
addWorksheet(wb, "AgeStructure") # add worksheet
addWorksheet(wb, "ReleaseCohorts") # add worksheet

writeData(wb, "AgeStructure", ReachYALPlotData %>% select(-Contacts)) # write dataframe (second argument) to worksheet
writeData(wb, "ReleaseCohorts", ReachReleaseSizes) # write dataframe (second argument) to worksheet


# Last step is to save workbook. Give a useful name.  Adding date time ensures this step
# will not overwrite a previous version.  Location should be output folder
saveWorkbook(wb, paste0("output/Reach",StudyReach, "AgeAnalysis",
                        format(Sys.time(), "%Y%m%d"), ".xlsx"),
             overwrite = TRUE)
