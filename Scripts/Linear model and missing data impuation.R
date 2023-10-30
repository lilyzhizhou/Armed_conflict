#-----------------------------------------------------------------
# Author: Zhi Lin Zhou
# Last updated: 2023-10-30
# What: 
# - Explore Missing Data 
# - Run Linear Model 
# - Impute missing data with MICE
#----------------------------------------------------------------
library(kableExtra)
library(finalfit)
library(texreg)
library(mice)

here() # sets working directory

# Load data
data <- read_csv(here('Output/finaldata.csv')) %>%
  select(-OECD2023)


# Visualize Missing Data
VIM::aggr(data, numbers = TRUE, prop = c(TRUE, FALSE))
  # mat.mor is missing around 10% 

naniar::vis_miss(data)
  # 0.9% missing 

######################
## Run Linear Model ##
######################

# rescale variables 
data1 <- data %>%
  mutate(GDP = GDP/1000,
         popdens = popdens/100)

preds <- as.formula("~ ISO + as.factor(Year) + GDP + popdens + OECD + 
                    urban + agedep + male_edu + temp + conflict + drought + earthquake")
                    
mat.mor.lm <- lm(update.formula(preds, mat.mor ~ .), data = data1)
infant.mor.lm <- lm(update.formula(preds, mat.mor ~.), data = data1)
neo.mor.lm <- lm(update.formula(preds, neo.mor ~.), data = data1)
under5.mor.lm <- lm(update.formula(preds, under5.mor ~.), data = data1)

screenreg(list(mat.mor.lm, under5.mor.lm, infant.mor.lm, neo.mor.lm),
       ci.force = TRUE,
       caption = 'Results from linear regression models')


###########################
## Imputing Missing Data ##
###########################

set.seed(777) # set seed for reproducibility 

data_imp <- data1 %>%
  mutate(ISO_num = as.numeric(factor(data1$ISO))) %>% # change ISO into numeric value 
  select(-c(country_name,ISO,region))
 

# Look at default imputation methods 
mi0 <- mice(data_imp, seed = 1, m = 1, maxit = 0, print = F)
meth <- mi0$method
meth

# 2l.pan level 1 variables 
pred <- mi0$predictorMatrix
pred

pred[c("GDP","popdens","urban","male_edu","temp","mat.mor",
       "infant.mor","neo.mor","under5.mor"),"ISO_num"] <- -2
pred
  
meth[c("GDP","popdens","urban","male_edu","temp","mat.mor",
       "infant.mor","neo.mor","under5.mor")] <- "2l.pan"

# run mice 
start.time <- Sys.time()
mice.multi.out  <- mice(data_imp, seed = 100, m = 10, maxit = 5,
                        method = meth,
                        predictorMatrix = pred, print = F)

save(mice.multi.out, file = "miceout.Rda")

load("miceout.Rda")
plot(mice.multi.out)

# Check imputed values 
complete.data.multi2 <- complete(mice.multi.out, "all")

head(complete.data.multi2$`1`, n=20)

sum(is.na(complete.data.multi2)) # no na data 
