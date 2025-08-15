# Caswell matrix population model with density dependence

packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)}}

packages(dplyr)
packages(lubridate)

# Define model parameters

years <- 20
pond_size <- 2.5 # Surface acres (adjustable)
initial_females <- 100
initial_males <- 100
initial_size_class <- 2
size_class_biomass <- c(0.350, 0.500, 1.2) # Biomass per individual for size classes 2-4
max_repro_output <- 5000

# Define stock-recruitment parameters
alpha <- 200  # Recruitment scaling factor
beta <- 0.02  # Adjust this to match density-dependent patterns
s_r_model <- "Ricker" # Define Ricker (decline at high abundance) or "B-V" Beverton-Holt (asymptotic)

# Survival Parameters (Density-dependent)
biomass_threshold <- 350 # kg per surface acre for size-0 survival reduction
survival_size1_low <- 0.10
survival_size1_high <- 0.01
survival_other_low <- 0.85
survival_other_high <- 0.7

# Transition Rates (Density-dependent)
transition_rates <- list(
  "female_2_to_3" = function(biomass) max(0.1, 0.5 - (0.4 * biomass / biomass_threshold)),
  "female_3_to_4" = function(biomass) max(0.1, 0.5 - (0.4 * biomass / biomass_threshold)),
  "male_2_to_3" = function(biomass) max(0.05, 0.3 - (0.25 * biomass / biomass_threshold)),
  "male_3_to_4" = function(biomass) max(0.1, 0.5 - (0.4 * biomass / biomass_threshold)),
  "female_1_to_2" = function(repro_output) max(0.1, 0.9 - (0.8 * repro_output / max_repro_output*pond_size)),
  "male_1_to_2" = function(repro_output) max(0.05, 0.6 - (0.55 * repro_output / max_repro_output*pond_size))
)

# S_R function

ifelse(s_r_model == "Ricker",
       recruitment <- function(B) {return(alpha * B * exp(-beta * B))}, 
       recruitment <- function(B) {return((alpha * B) / (1 + beta * B))})

# Initialize population structure
population <- list(
  females = matrix(0, nrow = years, ncol = 4),
  males = matrix(0, nrow = years, ncol = 4)
)
population$females[1, initial_size_class] <- initial_females
population$males[1, initial_size_class] <- initial_males

# Simulate population dynamics
for (t in 2:years) {
  # Calculate biomass of size classes 1-3
  total_female_biomass <- sum(population$females[t-1, 2:4] * size_class_biomass)
  total_male_biomass <- sum(population$males[t-1, 2:4] * size_class_biomass)
  total_biomass <- total_female_biomass + total_male_biomass
  
  female_biomass_sa <- total_female_biomass/pond_size
  total_biomass_sa <- total_biomass/pond_size
  
  # Determine reproductive output
  repro_output <- recruitment(female_biomass_sa)*pond_size
  
  # Apply survival rates (density-dependent)
  survival_size1 <- max(survival_size1_high, survival_size1_low - ((survival_size1_low - survival_size1_high) * (total_biomass_sa / biomass_threshold)))
  survival_other <- max(survival_other_high, survival_other_low - ((survival_other_low - survival_other_high) * (total_biomass_sa / biomass_threshold)))
  
  # Update population
  population$females[t, 1] <- as.integer(repro_output * survival_size1)
  population$males[t, 1] <- as.integer(repro_output * survival_size1)
  
  for (size in 1:4) {
    female_transition_key <- paste0("female_", size, "_to_", size+1)
    male_transition_key <- paste0("male_", size, "_to_", size+1)
    
    print(paste("Checking transition key:", female_transition_key, "Exists:", !is.null(transition_rates[[female_transition_key]])))
    print(paste("Checking transition key:", male_transition_key, "Exists:", !is.null(transition_rates[[male_transition_key]])))
    
    if (!is.null(transition_rates[[female_transition_key]]) && is.function(transition_rates[[female_transition_key]])) {
      female_transition_fn <- transition_rates[[female_transition_key]]
      print(paste("Calling function:", female_transition_key))
      transition_females <- population$females[t, size] * female_transition_fn(total_biomass_sa)
    } else {
      transition_females <- 0
      print(paste("Function missing:", female_transition_key))
    }
    
    if (!is.null(transition_rates[[male_transition_key]]) && is.function(transition_rates[[male_transition_key]])) {
      male_transition_fn <- transition_rates[[male_transition_key]]
      print(paste("Calling function:", male_transition_key))
      transition_males <- population$males[t, size] * male_transition_fn(total_biomass_sa)
    } else {
      transition_males <- 0
      print(paste("Function missing:", male_transition_key))
    }
    
    # Move transitioning individuals to the next size class
    if (size < 4) {
      population$females[t, size+1] <- transition_females
      population$males[t, size+1] <- transition_males
    }
    
    # Remaining individuals stay in current size class
    population$females[t, size] <- population$females[t, size] - transition_females
    population$males[t, size] <- population$males[t, size] - transition_males
  }
}

# Visualization
library(ggplot2)
# Convert matrix to dataframe for plotting
df_females <- data.frame(year = rep(1:years, times = 4), 
                         size_class = rep(1:4, each = years), 
                         count = as.vector(t(population$females)))  # Transpose the matrix to get proper structure

df_males <- data.frame(year = rep(1:years, times = 4), 
                       size_class = rep(1:4, each = years), 
                       count = as.vector(t(population$males))) 
df_sex_ratio <- df_females %>% 
  rename(females = count) %>% 
  inner_join(df_males 
             %>% rename(males = count), 
             by = c("year" = "year", "size_class" = "size_class")) %>%
  mutate(ratio = ifelse(males>0, females/(males+females), 0))

ggplot(df_females, aes(x = year, y = count, color = factor(size_class))) +
  geom_line() + labs(title = "Female Population Structure Over Time", x = "Year", y = "Count", color = "Size Class")

ggplot(df_males, aes(x = year, y = count, color = factor(size_class))) +
  geom_line() + labs(title = "Male Population Structure Over Time", x = "Year", y = "Count", color = "Size Class")

ggplot(df_sex_ratio, aes(x = year, y = ratio, color = factor(size_class))) +
  geom_line() + labs(title = "Female Proportion Over Time", x = "Year", y = "Prop", color = "Size Class")
