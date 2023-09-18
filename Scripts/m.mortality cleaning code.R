#-----------------------------------------------------------------
# Author: Zhi Lin Zhou
# Last updated: 2023-09-18
# What: Read in raw data
#
#----------------------------------------------------------------

library(tidyverse)
library(here)
library(usethis)

here() # sets working directory
#usethis::use_git_config(user.name = "Zhi Lin Zhou", user.email = "lilyzhi.zhou@mail.utoronto.ca")

# read in raw file 
m.mortality.raw <- read.csv(here("Input","maternalmortality.csv"),header = TRUE)

#Use the select() function in the dplyr package (which is one of the packages in the tidyverse 
# bundle) to subset the data to have only the variables Country.Name, X2000 – X2019.

m.mortality <- m.mortality.raw %>%
  select(Country.Name, X2000:X2019) %>%

#The data set is currently in a wide format. Use the pivot_longer() function to convert the data
#set into a long format. So that the first and last 20 rows of the resulting data set look like this.
#Hint: You need to select the columns X2000 to X2019, remove the prefix X from them, change
#the name of the variable to Year, change the values to MatMor. Finally, make sure the year
#variable is stored as numeric.

  rename_all(~gsub("^X", "",.)) %>% # renaming all variables that starts with X to remove X
  pivot_longer(!Country.Name, names_to = "Year", values_to = "MatMor") %>% # creating 1 column for all mat mor values over all years 
  mutate(Year = as.numeric(Year)) %>% # Make year numeric 
  arrange(Country.Name, Year)

#usethis::use_git()
#usethis::create_github_token()
#usethis::use_github()

# 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