######################
### sample dataset ###
######################

# Define the possible categories for each column
colors <- c("blue", "white", "brown")
countries <- c("japan", "canada", "peru")
diets <- c("fish", "bug", "fruit")
can_fly<- c(0, 1)
wingspan <- runif(30, min = 3, max = 150)

# Create a random selection of categories for each column
random_colors <- sample(colors, 30, replace = TRUE)
random_countries <- sample(countries, 30, replace = TRUE)
random_diets <- sample(diets, 30, replace = TRUE)
random_fly <- sample(can_fly, 30, replace = TRUE)

# Create the dataframe
birds <- data.frame(color = random_colors, country = random_countries, diet = random_diets, can_fly = random_fly, wingspan = wingspan) %>%
  mutate(color = ifelse(country == 'canada' & runif(1) < 0.5, 'white', color),
         diet = ifelse(can_fly == 'fly', sample(c("bug", "fish"), n(), replace = TRUE), diet))
  

# View the first few rows of the dataframe
head(birds)


#################
### Function ####
#################
library(corrplot) # to generate corrplot 
library(rcompanion) # to use cramers V
library(caret) # to create dummy variables 


# Calculate cramersV scores for overall variable correlation 
############################################################

calculate_cramers_v <- function(df) {
  ncol <- ncol(df)
  mat <- matrix(1, ncol, ncol) # create matrix to store cramerv values, dimensions are same a # of columns of df
  
  for (i in 1:(ncol - 1)) {                  # start at column 1 and go up till the 2nd last column
    for (j in (i + 1):ncol) {                # start at column 2 and go up till the last column 

      result <- cramerV(df[, i], df[, j])    # calculate craverV scores for each column iteration 
      mat[i, j] <- result                    # input the score into the indicated row/column in the matrix 
      mat[j, i] <- result                    # input the same score into the indicated row/column in the matrix 
    }
  }
  
  rownames(mat) <- colnames(df)             # set row names in matrix to match dataset names 
  colnames(mat) <- colnames(df)             # set column names in matrix to match dataset names 
  
  return(mat)
}

# Generate correlation matrix for categorical variables 
cramer_matrix <- calculate_cramers_v(birds)
cramer_matrix

corrplot(cramer_matrix, method = "color", addCoef.col = "black", is.corr = FALSE, tl.col = "black")



# Calculate pearson correlation for each categorical option 
############################################################
library(ggcorrplot)

#one hot encode categorical variables 
dummy_data <- function(data){
  
  dmy <- dummyVars(" ~ .", data = data)
  data_dmy <- data.frame(predict(dmy, newdata = data))
  data_dmy
}

birds_dmy <- dummy_data(birds)

head(birds_dmy)

# Chi square scores into 

# conduct chi square tests for each variable 
model.matrix(~0+., data=birds_dmy) %>% 
  ggcorrplot(show.diag=T,lab=TRUE, lab_size=2)



