# load packages
library(dplyr)
library(RMariaDB)
library(lubridate)
library(dbplyr)

MySQLSettings <- "dependencies/brk828_nativef1_sensing.cnf"
con <- dbConnect(RMariaDB::MariaDB(),default.file=MySQLSettings, group="settings")

# Download source material
scan_db <- tbl(con, "scan_data")
effort_db <- tbl(con, "scan_effort")
location_db <- tbl(con, "location")
release_db <- tbl(con, "WebTable")

# Connecting tables to get scans from Topock Marsh with release history
ScanningTable <- scan_db %>%
  select(PIT, date, time, EID = effort_ID) %>%
  inner_join(effort_db %>% select(EID, LID = location_ID), by = "EID") %>%
  inner_join(location_db %>% select(LID = LOCATION_ID, Complex, LOCATION, SURFACE_CONNECTION) %>%
               filter(Complex == 'Topock Marsh'), by = "LID") %>%
  inner_join(release_db %>% select(Release_LID = LID, Release_Date = DISP_DATE, PIT = TAG_NUMBER, STATUS) %>%
               filter(STATUS == 'Release'), by = "PIT") %>%
  collect()
dbDisconnect(con)
#unique scans by year and filtering for fish present for longer than a year
UniqueScans <- ScanningTable %>% mutate(Release_Date = mdy(Release_Date), Year = year(date)) %>%
  group_by(PIT, Release_Date, Year) %>% summarize (date = max(date)) %>% mutate(DAL = date - Release_Date) %>% mutate(DAL = as.numeric(DAL)) %>%
  filter(DAL >= 365) %>% arrange(Year)

##for loop for estimating population
Loop_years <- unique(UniqueScans$Year) # Assumes your year variable is called 'Year'
Loop_results <- data.frame() # Create an empty data frame to store results

for (i in 2:length(Loop_years)) {
  prev_year <- Loop_years[i-1]
  curr_year <- Loop_years[i]
  
  # Extract relevant data for previous year
  prev_year_data <- UniqueScans %>% filter(Year == prev_year)
  
  # Extract relevant data for current year
  curr_year_data <- UniqueScans %>% filter(Year == curr_year)
  
  # Calculate marks and captures for previous and current year
  prev_year_marks <- prev_year_data %>% group_by(PIT) %>% summarise(n_marks = n())
  curr_year_captures <- curr_year_data %>% group_by(PIT) %>% summarise(n_captures = n())
  
  # Calculate recaps between previous and current year
  recaps <- inner_join(prev_year_marks, curr_year_captures, by = "PIT") %>% 
    summarise(n_recaps = n_distinct(PIT))
  
  # Compile results into a dataframe
  year_results <- data.frame(year = curr_year, marks = sum(prev_year_marks$n_marks), captures = sum(curr_year_captures$n_captures), recaps = sum(recaps$n_recaps))
  
  # Append results to the main dataframe
  Loop_results <- rbind(Loop_results, year_results)
}

#calculate population estimate and confidence intervals
Loop_results <- Loop_results %>% mutate(estimate = ((marks + 1) * (captures + 1))/(recaps + 1)) %>% 
  mutate(lower_bound = qpois(0.05, recaps)) %>% 
  mutate(upper_bound = qpois(0.95, recaps)) %>%
  mutate(lci = ((marks + 1) * (captures + 1))/(upper_bound + 1),
         uci = ((marks + 1) * (captures + 1))/(lower_bound + 1)) %>% arrange(year)


##lapply method instead of for loop
lapply_years <- unique(UniqueScans$Year) # Assumes your year variable is called 'Year'

# Function to calculate marks, captures, and recaps for each year
calc_yearly_stats <- function(i) {
  prev_year <- lapply_years[i-1]
  curr_year <- lapply_years[i]
  
  # Extract relevant data for previous year
  prev_year_data <- UniqueScans %>% filter(Year == prev_year)
  
  # Extract relevant data for current year
  curr_year_data <- UniqueScans %>% filter(Year == curr_year)
  
  # Calculate marks and captures for previous and current year
  prev_year_marks <- prev_year_data %>% group_by(PIT) %>% summarise(n_marks = n())
  curr_year_captures <- curr_year_data %>% group_by(PIT) %>% summarise(n_captures = n())
  
  # Calculate recaps between previous and current year
  recaps <- inner_join(prev_year_marks, curr_year_captures, by = "PIT") %>% 
    summarise(n_recaps = n_distinct(PIT))
  
  # Compile results into a dataframe
  year_results <- data.frame(year = curr_year, marks = sum(prev_year_marks$n_marks), captures = sum(curr_year_captures$n_captures), recaps = sum(recaps$n_recaps))
  
  return(year_results)
}

# Apply the function to each year using lapply()
lapply_results <- do.call(rbind, lapply(2:length(lapply_years), calc_yearly_stats))

#caluculate population estimates and CIs for the lapply method
lapply_results <- lapply_results %>% mutate(estimate = ((marks + 1) * (captures + 1))/(recaps + 1)) %>% 
  mutate(lower_bound = qpois(0.05, recaps)) %>% 
  mutate(upper_bound = qpois(0.95, recaps)) %>%
  mutate(lci = ((marks + 1) * (captures + 1))/(upper_bound + 1),
         uci = ((marks + 1) * (captures + 1))/(lower_bound + 1)) %>% arrange(year)

