# B. Kesner 17 May 2023
# Script to plot population estimates
# Load useful lab functions
source("LabFunctions.R")

packages(ggplot2)
packages(dplyr)

data <- read.csv("output/ZonePopulationEstimates.csv", header = TRUE)

# Cleanup data and use quasipoisson for confidence intervals when recaptures are 30 or fewer otherwise
# use normal distribution estimates.
ZoneEstimates <- data %>%
  na.omit(data) %>% 
  mutate(Year = as.factor(CensusYear),
         Estimate = as.numeric(Estimate),
         Zone = as.factor(DecimalZone),
         LowerCI = ifelse(R<=30, LowerQP95CI, LowerN95CI),
         UpperCI = ifelse(R<=30, UpperQP95CI, UpperN95CI)) %>%
  select(-X,-LowerQP95CI, -LowerN95CI, -UpperQP95CI, -UpperN95CI, -DecimalZone, -CensusYear)

data <- read.csv("output/ReachPopulationEstimates.csv", header = TRUE)

# Cleanup data and use quasipoisson for confidence intervals when recaptures are 30 or fewer otherwise
# use normal distribution estimates.
ReachEstimates <- data %>%
  na.omit(data) %>% 
  mutate(Year = as.factor(CensusYear),
         Estimate = as.numeric(Estimate),
         Reach = as.factor(Reach),
         LowerCI = ifelse(R<=30, LowerQP95CI, LowerN95CI),
         UpperCI = ifelse(R<=30, UpperQP95CI, UpperN95CI)) %>%
  select(-X,-LowerQP95CI, -LowerN95CI, -UpperQP95CI, -UpperN95CI, -CensusYear)


#####increse number of tick marks 

number_ticks <- function(n) {function(limits) pretty(limits, n)}

XYTEZoneEstimates <- ZoneEstimates %>%
  filter(Species == "XYTE")

XYTEReachEstimates <- ReachEstimates %>%
  filter(Species == "XYTE")

plot = ggplot(data = XYTEZoneEstimates, aes(x=Year, y = Estimate, group = Zone)) +
  geom_line(aes(color = Zone)) +
  geom_point(aes(color = Zone), size = 2)+
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), linetype = 2, alpha = 0.1)+
  labs(x = "Year", y = "Population estimate", fill = "location")+
  theme_classic()+ 
  theme(axis.line.x = element_line(colour = 'black', size = 0.1, linetype = 'solid'), 
        axis.line.y = element_line(colour = 'black', size = 0.1, linetype = 'solid'))+
  theme(axis.text = element_text(size = 13))+
  theme(axis.title = element_text(size = 13))+
  theme(legend.text = element_text(size = 13))+
  scale_x_discrete(breaks=number_ticks(8)) +
  scale_y_continuous(breaks=number_ticks(8)) +
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(colour = "black", size = 13),axis.title = element_text(size = 15))+
  theme(axis.text.y = element_text(colour = "black", size = 13))+
  theme(legend.position = "top")

plot

jpeg("output/XYTEZoneEstimates.jpeg", units = "in", width = 11, height = 5, res = 300)
plot
dev.off()
############
#########write graph 
plot = ggplot(data = XYTEReachEstimates, aes(x = Year, y = Estimate, group = Reach)) +
  geom_line(aes(color = Reach)) +
  geom_point(aes(color = Reach), size = 2)+
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), linetype = 2, alpha = 0.1)+
  labs(x = "Year", y = "Population estimate", fill = "location")+
  theme_classic()+ 
  theme(axis.line.x = element_line(colour = 'black', size = 0.1, linetype = 'solid'), 
        axis.line.y = element_line(colour = 'black', size = 0.1, linetype = 'solid'))+
  theme(axis.text = element_text(size = 13))+
  theme(axis.title = element_text(size = 13))+
  theme(legend.text = element_text(size = 13))+
  scale_color_discrete(labels=c("Lake Mead", "Lake Mohave", "Lake Havasu", "LowerRiver"))+
  scale_x_discrete(breaks=number_ticks(8)) +
  scale_y_continuous(breaks=number_ticks(8)) +
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(colour = "black", size = 13),axis.title = element_text(size = 15))+
  theme(axis.text.y = element_text(colour = "black", size = 13))+
  theme(legend.position = "top")

plot

jpeg("output/XYTEReachEstimates.jpeg", units = "in", width = 11, height = 5, res = 300)
plot
dev.off()

