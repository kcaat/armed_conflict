### week 12 in-class (optimx)

### preparation code from quiz10

library(tidyverse)
library(here)

data <- read.csv(here("original/finaldata.csv"))

#conflict variable
data <- data %>%
  group_by(ISO) |>
  mutate(conflict2018 = ifelse(any(binaryconflict == 1 & Year == 2019), 1, 0)) |>
  ungroup()

#create var: any earthquake between 2010 and 2017
data <- data %>%
  group_by(ISO) %>%
  mutate(any_earthq = ifelse(any(earthqbin == 1 & Year >= 2010 & Year <= 2017), 1, 0)) |>
  ungroup()

#create var: any drought between 2010 and 2017
data <- data %>%
  group_by(ISO) %>%
  mutate(any_drought = ifelse(any(droughtbin == 1 & Year >= 2010 & Year <= 2017), 1, 0)) |>
  ungroup()

#fit glm model
conflictmodel <- glm(conflict2018 ~ any_earthq + any_drought, family=binomial, data=data)

### edit the optimx code to include both ever_dr and ever_eq predictors

library(optimx)

#function for -ll using data
negll <- function(par){
  y <- data$conflict2018
  x1 <- data$any_earthq
  x2 <- data$any_drought
  # 1. Calculate xbeta
  xbeta <- par[1] + par[2]*x1 + par[3]*x2
  # 2. Calculate p
  p <- exp(xbeta) / (1 + exp(xbeta))
  # 3. Calculate negative log-likelihood
  val <- -sum(y * log(p) + (1 - y) * log(1 - p))
  return(val)
}

# optimize par1,2,3
opt <- optimx(
  par = c(0,0,0),
  fn = negll,
  control = list(trace = 0, all.methods = TRUE)
)

# get SE estimates
# Extract hessian matrix for BFGS optimization
hessian_m <- attributes(opt)$details["BFGS", "nhatend"][[1]]
fisher_info <- solve(hessian_m)
prop_se <- sqrt(diag(fisher_info))
prop_se

### Compare the parameter and standard error estimates from the glm() function to the BFGS method

summary(opt, order = "convcode")
#BFGS
#coefs -2.046325  0.9033308 0.9524118
#SEs 0.06804150 0.08760908 0.08377407
conflictmodel
glm_summary <- summary(conflictmodel)
glm_summary$coefficients[,"Std. Error"]
#coefs -2.0463       0.9035       0.9523
#SEs 0.06804074  0.08760830  0.08377376 

# the coefs and standard errors are very similar comparing GLM and optimx BFGS
# therefore these two methods both optimized and reached similar conclusions
# both methods agree