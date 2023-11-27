#-----------------------------------------------------------------
# Author: Zhi Lin Zhou
# Last updated: 2023-11-27
# What: Run Optimization 
#
#----------------------------------------------------------------

library(tidyverse)
library(here)
library(usethis)
library(optimx)

here() 

data <- read_csv('C:/Users/zhoul/OneDrive/Documents/School/UofT MPH - Epi/Fall 2023/Statistical Programming and Computation/Armed_conflict/Output/finaldata (1).csv')

# conflict 
data1 <- data %>%
  group_by(ISO) %>%
  mutate(conflict18 = ifelse(any(year == '2019' & armconf1 == '1'), 1,0)) %>%
  ungroup()


# Earthquake 
data1 <- data1 %>%
  group_by(ISO) %>%
  mutate(earthquake_ever = ifelse(any(year %in% c(2010:2017) & earthquake == '1'), 1,0)) %>%
  ungroup()


# Drought
data1 <- data1 %>%
  group_by(ISO) %>%
  mutate(drought_ever = ifelse(any(year %in% c(2010:2017) & drought == '1'), 1,0)) %>%
  ungroup()

# new dataset 
data1 <- data1 %>%
  select(ISO, year, armconf1, conflict18, earthquake, earthquake_ever, drought, drought_ever) %>%
  distinct(ISO, conflict18, earthquake_ever, drought_ever)

# model 
model.glm <- glm(conflict18 ~ drought_ever + earthquake_ever, family = binomial('logit'), data = data1)

summary(model.glm)


# Optimx
negll <- function(par){
  y <- data1$conflict18
  x1 <- data1$earthquake_ever
  x2 <- data1$drought_ever
  
  # 1. Calculate xbeta
  xbeta <- par[1] + par[2] * x1 + par[3] * x2
  
  # 2. Calculate p
  p <- exp(xbeta) / (1 + exp(xbeta))
  
  # 3. Calculate negative log-likelihood
  val <- -sum(y * log(p) + (1 - y) * log(1 - p))
  return(val)
}

opt <- optimx(
  par = c(0,0,0),
  fn = negll,
  control = list(trace = 0, all.methods = TRUE)
)

summary(opt, order = "convcode")
# intercept: -2.046141  
# earthquake: 0.9036027 
# drought: 0.9522648010
# very similar to model output 

# Extract hessian matrix for BFGS optimization
hessian_m <- attributes(opt)$details["BFGS", "nhatend"][[1]]
fisher_info <- solve(hessian_m)
prop_se <- sqrt(diag(fisher_info))
prop_se

### standard error 
# intercept: 0.3042700 
# earthquake: 0.3917786 
# drought: 0.3746297
# very similar to output data 