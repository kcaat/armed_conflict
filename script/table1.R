### week 6 in-class

### create a table 1

#install.packages('table1')
library(table1)
library(tidyverse)

library(here)
data <- read.csv(here("original/finaldata.csv"))

colnames(data)
#main exposure binaryconflict
#unit is ISO
#use data from Year=2000
#variables to include: MatMort, NeoMort, InfMort, Under5Mort, drought, earthquake

#subset Year=2000 data
data2000 <- subset(data, Year==2000)

#fix disaster variables to be binary
data2000 <- data2000 %>%
  group_by(ISO, Year) |>
  mutate(earthqbin = case_when(earthquake <1 ~ 0,
                               earthquake >= 1 ~ 1))

data2000 <- data2000 %>%
  group_by(ISO, Year) |>
  mutate(droughtbin = case_when(drought <1 ~ 0,
                                drought >= 1 ~ 1))

#turn binary conflict into factor
data2000$binaryconflict <- 
  factor(data2000$binaryconflict, 
         levels=c(0,1),
         labels=c("No armed conflict",
                  "Yes armed conflict"))

sum(is.na(data2000$binaryconflict))

#labels
title1 <- "Table 1. Baseline data from 2000 for armed conflict study"
label(data2000$MatMort)       <- "Maternal mortality ratio per 100,000 live births"
label(data2000$NeoMort)       <- "Neonatal mortality rate per 1,000 live births"
label(data2000$InfMort)       <- "Infant mortality rate per 1,000 live births"
label(data2000$Under5Mort)       <- "Under-5 mortality rate per 1,000 live births"
label(data2000$GDP)       <- "GDP per capita"
label(data2000$popdens)       <- "Population density(a)"
label(data2000$urban)       <- "Urban residence(b)"
label(data2000$agedep)       <- "Age dependency ratio(c)"
label(data2000$male_edu)       <- "Male education(d)"
label(data2000$temp)       <- "Temperature"

#can also use list method to label all variables

#factor and label categorical variables
data2000$earthqbin <- 
  factor(data2000$earthqbin, 
         levels=c(0,1),
         labels=c("No",
                  "Yes"))
label(data2000$earthqbin)       <- "Earthquake"
data2000$droughtbin <- 
  factor(data2000$droughtbin, 
         levels=c(0,1),
         labels=c("No",
                  "Yes"))
label(data2000$droughtbin)       <- "Drought"
data2000$OECD <- 
  factor(data2000$OECD, 
         levels=c(0,1),
         labels=c("No",
                  "Yes"))
label(data2000$OECD)       <- "OECD member"

#add foodnote for definition of armed conflict
footnote1 <- c("Armed conflict is defined as >=25 battle-related deaths/year",
               "(a) Population density: percentage of population living in density of >1,000 people/km2",
               "(b) Urban residence: percentage of the population living in urban areas",
               "(c) Age dependency: percentage of working-age population")

#render function to display mean or median for diff variables
#note: this removes missing from table
rndr <- function(x, name, ...) {
  if (!is.numeric(x)) return(render.categorical.default(x))
  what <- switch(name,
                 MatMort = "Median [Min, Max]",
                 InfMort = "Median [Min, Max]",
                 NeoMort = "Median [Min, Max]",
                 Under5Mort = "Median [Min, Max]",
                 GDP = "Median [Min, Max]",
                 popdens = "Median [Min, Max]",
                 urban = "Median [Min, Max]",
                 agedep = "Median [Min, Max]",
                 male_edu = "Median [Min, Max]",
                 temp = "Median [Min, Max]")
  parse.abbrev.render.code(c("", what))(x)
}

conflicttable1 <- table1(~ GDP + OECD +popdens + urban + agedep + male_edu+ temp+MatMort + NeoMort + InfMort + Under5Mort + droughtbin + earthqbin | binaryconflict, data=data2000, caption=title1, footnote=footnote1, render=rndr)

conflicttable1
