### week 3 

###merge all datasets

#Write a new R script that merges all the data sets created in the previous steps and create a new data set that can be used for the primary analysis. Use the source() function to call the R scripts that create the individual data sets. What variables would you use as keys to join the data? Why do you need to be careful when merging the armed conflict data and disaster data?

#merge merged mortality, disaster, conflict, covariates

#not working lol
library(here)
here()
source(here("script/cleandisaster.R"))


covariates <- read.csv(here("original/covariates.csv"))
mortality <- read.csv(here("original/mergedmortality.csv"))
disaster <- read.csv(here("original/disasternew.csv"))
conflict <- read.csv(here("original/binaryconflict.csv"))

head(mortality)
head(disaster)
head(conflict)

mortality$Year
disaster$Year
conflict$year
covariates$year

# Change year to Year
names(conflict)[names(conflict) == "year"] <- "Year"
names(covariates)[names(covariates) == "year"] <- "Year"

#sum conflict data by year
library(tidyverse)
conflict <- conflict %>% 
  group_by(Year,ISO) %>% 
  summarise(best = sum(best))

conflict <- conflict %>%
  mutate(binaryconflict = case_when(best < 25 ~ 0,
                                    best >= 25 ~ 1))

#merge
#use left join
#code is on her github

alllist <- list(covariates, mortality, disaster, conflict)

finaldata <- alllist |> reduce(left_join, by = c('ISO', 'Year'))

# need to fill in NAs with 0's for armconf1, drought, earthquake
finaldata <- finaldata |>
  mutate(binaryconflict = replace_na(binaryconflict, 0),
         drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         best = replace_na(best, 0))

write.csv(finaldata, file = here("original/finaldata.csv"), row.names = FALSE)
