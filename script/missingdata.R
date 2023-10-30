### week 8 in-class

#Use one of the missing data visualization packages to describe the patterns of missing data

#install.packages("naniar")
#install.packages("VIM")
#install.packages("finalfit")
library(naniar)
library(VIM)
library(finalfit)

library(here)
data <- read.csv(here("original/finaldata.csv"))

#fix disaster variables to be binary
library(tidyverse)
data <- data %>%
  group_by(ISO, Year) |>
  mutate(earthqbin = case_when(earthquake <1 ~ 0,
                               earthquake >= 1 ~ 1))

data <- data %>%
  group_by(ISO, Year) |>
  mutate(droughtbin = case_when(drought <1 ~ 0,
                                drought >= 1 ~ 1))

write.csv(data, file = here("original/finaldata.csv"), row.names = FALSE)

data <- read.csv(here("original/finaldata.csv"))

naniar::vis_miss(data)
#total 0.8% missing
# MatMort has the most missing

VIM::aggr(data, numbers = TRUE, prop = c(TRUE, FALSE))

#linear model
colnames(data)

#prepare variables
data$GDP1000 <- data$GDP / 1000
data$popdens100 <- data$popdens / 100

# put covariates into object
preds <- as.formula("~GDP1000 +OECD +popdens100 +urban +agedep +male_edu +temp +binaryconflict +earthqbin +droughtbin +ISO + as.factor(Year)")

#create models
matmortmodel <- lm(update.formula(preds, MatMort ~ .), data=data) #replaces . with contents of preds
infmortmodel <- lm(update.formula(preds, InfMort ~ .), data=data)
under5mortmodel <- lm(update.formula(preds, Under5Mort ~ .), data=data)
neomortmodel <- lm(update.formula(preds, NeoMort ~ .), data=data)

library(texreg)
texreg(list(matmortmodel, under5mortmodel, infmortmodel, neomortmodel),
       ci.force=TRUE,
       caption="Results from lin reg models")
lmresults <- texreg(list(matmortmodel, under5mortmodel, infmortmodel, neomortmodel),
                    ci.force=TRUE,
                    caption="Results from lin reg models")

#same thing but output table directly to R
lmresults <- screenreg(list(matmortmodel, under5mortmodel, infmortmodel, neomortmodel),
          ci.force=TRUE,
          caption="Results from lin reg models")
lmresults
# same coefficients as aya's results

### multiple imputation

# Use the mice package to multiply impute the final data with 𝑚 = 10 imputations
#install.packages("mice")
library(mice)

#use 2l.pan for continuous level-1 (changing) variables
#need to convert ISO to numeric

MIdata <- data
MIdata$ISO <- as.numeric(as.factor(MIdata$ISO))

#see which variables have missing
summary(MIdata)
#GDP, popdens, urban, male_edu, temp, all Morts
#these are all continuous variables, should use 2l.pan for all

# initial run to get the method vector, treating as cross-sectional data
mi0 <- mice(MIdata, seed = 1, m = 1, maxit = 0, print = F)
# view default imputation methods
meth <- mi0$method
meth

#change imputation methods to 2l.pan
#define vector with continuous variables that have NA
MIvars <- c("GDP", "popdens", "urban", "male_edu", "temp", "MatMort", "NeoMort", "InfMort", "Under5Mort", "GDP1000", "popdens100")

meth[MIvars] <- "2l.pan"

# see predictor matrix
pred <- mi0$predictorMatrix
pred

#specify class variable b/c multilevel data
pred[MIvars, "ISO"] <- -2
pred

#run MI and get run time
start.time <- Sys.time()
mice.multi.out  <- mice(MIdata, seed = 100, m = 10, maxit = 10,
                        method = meth,
                        predictorMatrix = pred, print = F)
end.time <- Sys.time()
end.time - start.time #3 min

#save MI results as RDA file
save(mice.multi.out, file = "miceout.Rda")

load("miceout.Rda")
plot(mice.multi.out)

#put all imputations into one object
complete.data.multi <- complete(mice.multi.out, "all")
##check the first imputed dataset
head(complete.data.multi$`1`, n=20)

# Compare MI results to complete case data for each outcome
  # compare lin reg results with and without imputation

## fit analysis model and pool results

multi.matmort <- with(mice.multi.out,lm(update.formula(preds, MatMort ~ .)))
#getting error: object 'MatMort' not found

summary(pool(multi.matmort))