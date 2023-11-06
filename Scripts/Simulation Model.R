#-----------------------------------------------------------------
# Author: Zhi Lin Zhou
# Last updated: 2023-11-06
# What: Creating Simulation Week 9
#
#----------------------------------------------------------------

library(SimDesign)
library(ggplot2)

here() # sets working directory
#usethis::use_git_config(user.name = "Zhi Lin Zhou", user.email = "lilyzhi.zhou@mail.utoronto.ca")

## set simulation parameters
Design <- expand.grid(sample_size = c(50,500),
                      pz = c(0.2,0.8),
                      alpha0 = c(-1,0,1),
                      alpha1 = c(0,0.5,1),
                      mean_difference = c(0, 0.5))

Generate <- function(condition, fixed_objects = NULL){
  alpha0 <- condition$alpha0
  alpha1 <- condition$alpha1
  pz <- condition$pz
  sample_size <- condition$sample_size
  beta0 <- -3
  beta1 <- 0
  beta2 <- 2
  
  ## generate confounder Z from a binomial distribution
  z <- rbinom(sample_size, size = 1, prob = pz)
  ## compute probability of observing X = 1 from the inverse logit function
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  ## randomly generate binary variable X from the above probability
  x <- rbinom(sample_size, size = 1, prob = px)
  ## repeat above to randomly generate binary variable Y
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(sample_size, size = 1, prob = py)
  
  ## combine three random variables into a data frame
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  
  dat
}
  
Analyse <- function (condition, dat, fixed_objects = NULL) {
  ## fit unadjusted logistic regression model
  unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
  unadj.coef <- summary(unadj.mod)$coef
  unadj.p <- unadj.coef[2,4] 
  ## fit adjusted logistic regression model
  adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  adj.coef <- summary(adj.mod)$coef
  adj.p <- adj.coef[2,4]
  
  ret <- c(unadj.p = unadj.p, adj.p = adj.p)
  ret 
}

Summarise <- function (condition , results , fixed_objects = NULL ){
  ret <- EDR(results, alpha = .05)
  ret
}

results <- runSimulation(design = Design,
                        replications = 1000,
                        parallel = TRUE,
                        generate = Generate,
                        analyse = Analyse,
                        summarise = Summarise)


# Type 1 Error Rate 
mean(ifelse(results$unadj.p < 0.05, 1, 0))
mean(ifelse(results$adj.p < 0.05, 1, 0))


# pivot long 
reslong <- results %>%
  pivot_longer(cols = c("unadj.p","adj.p"),
               names_to = "model",
               values_to = 'edr')


# Plot figures
plot <- ggplot(reslong, aes(x = alpha1, y = edr, color = factor(model))) +
  geom_point() +
  geom_line()+
  facet_grid(alpha0~sample_size)

plot
