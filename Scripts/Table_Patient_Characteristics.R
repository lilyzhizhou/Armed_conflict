#-----------------------------------------------------------------
# Author: Zhi Lin Zhou
# Last updated: 2023-09-18
# What: Making Table 1
#
#----------------------------------------------------------------

library(table1)

here()
data <- read.csv(here('Output/finaldata.csv'))

# Subset data to only 2000
data2000 <- data %>%
  dplyr::filter(Year == 2000) %>%
  select(-c(country_name,ISO,region,Year))
  
# re-label conflict as ordinal 
catbinary <- function(x) {
  factor(x, levels = c(1,0),
                   labels = c("Yes","No"))
}

data2000$OECD <- catbinary(data2000$OECD)
data2000$drought <- catbinary(data2000$drought)
data2000$earthquake <- catbinary(data2000$earthquake)
data2000$conflict <- factor(data2000$conflict, levels = c(1,0),
          
                                              labels = c("Armed Conflict",'No Armed Conflict'))

# Functions that format categorical and continuous variables 
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

# Creating Table 1 
strata <- c(list(Total=data2000), split(data2000, data2000$conflict)) # selecting data to include 

labels <- list(
  variables = list(popdens = 'Population Density',
                   urban = 'Urban Residence',
                   agedep = 'Age Dependency',
                   male_edu = 'Male Education',
                   temp = 'Temperature',
                   sumdeath = 'Total Deaths',
                   mat.mor = 'Maternal Mortality',
                   infant.mor = 'Infant Mortality',
                   neo.mor = 'Neonatal Mortality',
                   under5.mor = 'Under 5 Mortality'), #creating labels for variables 
  groups = list("","Armed Conflict")) # creating headers for each column 


# Table 1 
table1(strata, labels, groupspan = c(1,2),
       caption = "Table 1. Country Characteristics",
       rander.categorical = my.render.cat)
