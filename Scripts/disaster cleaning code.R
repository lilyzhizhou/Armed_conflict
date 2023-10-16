#-----------------------------------------------------------------
# Author: Zhi Lin Zhou
# Last updated: 2023-09-18
# What: Read in raw data
#
#----------------------------------------------------------------

library(tidyverse)
library(here)
library(usethis)

here()

# read in raw file 
disaster.raw <- read.csv(here("Input","disaster.csv"),header = TRUE)

# Use the filter() function to subset the data set to only include years 2000–2019 and the disaster
# types “Earthquake” and “Drought”

disaster <- disaster.raw %>%
  filter(Disaster.Type %in% c("Earthquake","Drought") & Year >= 2000 & Year <= 2019) %>%

# b. Subset the data set to only include the following variables: Year, ISO, Disaster.type.
  
  select(Year, ISO, Disaster.Type) %>%
  
# c. Create a dummy variable drought and another dummy variable earthquake such that:
  
  mutate(drought = ifelse(Disaster.Type == "Drought",1,0),
         earthquake = ifelse(Disaster.Type == "Earthquake",1,0)) %>%

# Notice that some countries that had more than one earthquakes/droughts a year have multiple entries
# in some years. Use the group_by() and summarize() functions to create a data set where only one
# row of observation exists for each country and each year, such that:

 group_by(Year, ISO) %>%
 summarize(drought = sum(drought), earthquake = sum(earthquake))

