#-----------------------------------------------------------------
# Author: Zhi Lin Zhou
# Last updated: 2023-09-18
# What: Read in raw data
#
#----------------------------------------------------------------

here() # sets working directory

# Load data
covariates.clean <- read.csv(here('Input','covariates.csv')) %>%
  rename("Year" = year)
source(here('Scripts','Cleaning code.R'))
source(here("Scripts","armed conflict cleaning code.R"))

# merge all data sets created in the previous steps and create a new dataset 
# that can be used for primary analysis. 

merge_list <- list(covariates.clean, armedconf.clean, merge_data, disaster.clean) 
lapply(merge_list, FUN = summary)

final.data <- merge_list %>%
  reduce(left_join, by = c('ISO','Year')) %>% # join all the columns together 
  
  mutate(drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         sumdeath = replace_na(sumdeath, 0),
         conflict = replace_na(conflict,0)) %>%

  subset(select = -c(OECD2023,Country.Name)) # remove duplicate column 

# Check if you have 20 rows of data for each country 

print(final.data %>%
  group_by(ISO) %>%
  summarise(Count = n()), n = 186) 

# write csv

write.csv(final.data, file = here('Output','finaldata.csv'), row.names = FALSE)
