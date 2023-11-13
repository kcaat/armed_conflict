### week 10
# (scroll down for quiz code)

### in-class

#Create a table (in HTML or PDF) that shows
  #the differences in median (with the BCa boot- strap 95% confidence intervals)
  #for: maternal, infant, neonatal, and under-5 mortality
  #between the countries exposed to versus not   exposed to armed conflict 
  #for 2017.
#Be specific about the sample sizes used in each statistic (there may be missing).
#The table should be fully reproducible. Push the script that creates the table to your GitHub repository.

#use at least B=1000

library(tidyverse)
library(here)
library(boot)
data <- read.csv(here("original/finaldata.csv"))
data2017 <- subset(data, Year==2017)

#function to get med diff
getmeddiff <- function(data, indices, mort_stat) {
  sample_data <- data[complete.cases(data[,mort_stat]), ]
  sample_data <- sample_data[indices, ]
  group_meds <- tapply(sample_data[,mort_stat], sample_data$binaryconflict, FUN = median)
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

#MatMort
mat_boot <- boot(data=data2017, statistic = getmeddiff, strata = data2017$binaryconflict, R = 1000, mort_stat = 'MatMort')

mat_bootci <- boot.ci(boot.out = mat_boot, conf = 0.95, type = "bca")
mat_bootci

#NeoMort
neo_boot <- boot(data=data2017, statistic = getmeddiff, strata = data2017$binaryconflict, R = 1000, mort_stat = 'NeoMort')

neo_bootci <- boot.ci(boot.out = neo_boot, conf = 0.95, type = "bca")
neo_bootci

#InfMort
inf_boot <- boot(data=data2017, statistic = getmeddiff, strata = data2017$binaryconflict, R = 1000, mort_stat = 'InfMort')

inf_bootci <- boot.ci(boot.out = inf_boot, conf = 0.95, type = "bca")
inf_bootci

#Under5Mort
under5_boot <- boot(data=data2017, statistic = getmeddiff, strata = data2017$binaryconflict, R = 1000, mort_stat = 'Under5Mort')

under5_bootci <- boot.ci(boot.out = under5_boot, conf = 0.95, type = "bca")
under5_bootci

#store 95% CIs in vectors
variables <- c('Maternal Mortality', 'Neonatal Mortality', 'Infant Mortality', 'Under 5 Mortality')
lowerlims <- c(mat_bootci$bca[,4], neo_bootci$bca[,4], inf_bootci$bca[,4], under5_bootci$bca[,4])
upperlims <- c(mat_bootci$bca[,5], neo_bootci$bca[,5], inf_bootci$bca[,5], under5_bootci$bca[,5])
#store sample sizes for each variable
n <- c(
  nrow(data2017[complete.cases(data2017$MatMort),]),
  nrow(data2017[complete.cases(data2017$NeoMort),]),
  nrow(data2017[complete.cases(data2017$InfMort),]),
  nrow(data2017[complete.cases(data2017$Under5Mort),])
)

bcaCI <- data.frame(Variable=variables, LowerLim=lowerlims, UpperLim=upperlims, SampleSize=n)

### week 10 boot strap quiz

#Q1
#subset for 2017
data2017 <- subset(data, Year==2017)
print(sum(is.na(data2017$MatMort))) #3

#Q2
#subset for MatMort not missing
data2017a <- data2017[complete.cases(data2017$MatMort), ]
table(data2017a$binaryconflict)
#49 exposed to conflict, 134 unexposed

#Q3
#subset exposed
data_exposed <- subset(data2017a, binaryconflict==1)
median(data_exposed$MatMort) #164

#Q4
# B = 100
# sample _?_ observations w replacement from _49_ countries
# compute _standard deviation_

#Q5
#True