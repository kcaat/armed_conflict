### quiz 10

library(tidyverse)
library(here)

data <- read.csv(here("original/finaldata.csv"))

#create variable: any armed conflict in 2018
#recall: bin conflict is lagged by 1 year
#use 2017 conflict
#use 2019 conflict??

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

#check
head(data[, c("ISO", "Year", "conflict2018", "any_earthq", "any_drought")], 20)


#fit glm model
conflictmodel <- glm(conflict2018 ~ any_earthq + any_drought, family=binomial, data=data)
conflictmodel
summary(conflictmodel)
odds_ratios <- exp(coef(conflictmodel))
print(odds_ratios)


#Q1
table(data2017$any_earthq)
table(data2017$any_drought)

#Q2
data2017 <- data2017 %>%
  mutate(any_disaster = ifelse(any_earthq==1 & any_drought==1, 1, 0))
table(data2017$any_disaster)

#Q3
canada <- subset(data2017, country_name=="Canada")

#Q4
#estimated probability, of conflict in 2018, for 0 earthq and 0 drought

no_disaster <- subset(data, any_earthq==0 & any_drought==0)
predict(conflictmodel, newdata = no_disaster, type="response")
#0.1144

#using equation
probability <- exp(-2.0463)/(1+exp(-2.0463))
#gives same number