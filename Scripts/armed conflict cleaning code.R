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

armedconf.raw <- read.csv(here("Input","conflictdata.csv"))

# create new viariable where < 25 = no, >= 25 = yes 

armedconf.clean <- armedconf.raw %>%
  group_by(ISO, year) %>%
  summarize(sumdeath = sum(best)) %>%
  ungroup() %>%
  mutate(conflict = ifelse(sumdeath < 25, 0,1)) %>%
  mutate(year = year +1) %>%
  rename('Year' = 'year')
 
  
