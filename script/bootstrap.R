### week 10 boot strap quiz

library(tidyverse)
library(here)
data <- read.csv(here("original/finaldata.csv"))

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