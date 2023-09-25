#-----------------------------------------------------------------
# Author: Zhi Lin Zhou
# Last updated: 2023-09-18
# What: Read in raw data
#
#----------------------------------------------------------------

library(tidyverse)
library(here)
library(usethis)
library(countrycode)

here() # sets working directory
#usethis::use_git_config(user.name = "Zhi Lin Zhou", user.email = "lilyzhi.zhou@mail.utoronto.ca")

# read in raw files 
maternal.raw <- read.csv(here("Input","infantmortality.csv"),header = TRUE)
infant.raw <- read.csv(here("Input","maternalmortality.csv"),header = TRUE)
neonatal.raw <- read.csv(here("Input","neonatalmortality.csv"),header = TRUE)
under5.raw <- read.csv(here("Input","under5mortality.csv"),header = TRUE)


# Make function that 
## 1. subset the data to have only the variables Country.Name, X2000 – X2019.
## 2. remove X from prefix years 
## 3. change name of variable to Year 
## 4. change values to "mortality" 
## 5. convert the dataset into a long format.

  clean.data <- function(data,x) {
  data <- data %>%
    select(Country.Name, X2000:X2019) %>%
  rename_all(~gsub("^X", "",.)) %>% # renaming all variables that starts with X to remove X
    pivot_longer(!Country.Name, names_to = "Year", values_to = x) %>% # creating 1 column for all mat mor values over all years 
    mutate(Year = as.numeric(Year)) %>% # Make year numeric 
    arrange(Country.Name, Year)
}
 

maternal.clean <- clean.data(maternal.raw, 'mat.mor') 
infant.clean <- clean.data(infant.raw, "infant.mor") 
neonatal.clean <- clean.data(infant.raw, "neo.mor")   
under5.clean <- clean.data(infant.raw, "under5.mor") 


# Join Data
list_data <- list(maternal.clean,infant.clean,neonatal.clean,under5.clean)
merge_data <- reduce(list_data, full_join)


# Add Country Code 
merge_data$ISO <- countrycode(merge_data$Country.Name, 
                            origin = "country.name", 
                            destination = "iso3c")

##### Distaster Cleaning data ###########

disaster.raw <- read.csv(here("Input","disaster.csv"),header = TRUE)

# Use the filter() function to subset the data set to only include years 2000–2019 and the disaster
# types “Earthquake” and “Drought”

disaster.clean <- disaster.raw %>%
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
