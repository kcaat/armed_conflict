### in class week 2 cleaning disaster

### Disaster

disaster <- read.csv("disaster.csv")
library(tidyverse)
disaster <- filter(disaster,between(Year,2000,2019))

disaster <- disaster %>%
  filter(Disaster.Type %in% c("Drought", "Earthquake"))

#b. Subset the data set to only include the following variables: Year, ISO, Disaster.type.

disaster <- disaster %>% select(Year, Disaster.Type, ISO)

#c. Create a dummy variable drought and another dummy variable earthquake such that:
disaster <- disaster %>%
  mutate(drought = case_when(Disaster.Type=="Earthquake" ~ 0,
                             Disaster.Type=="Drought" ~ 1),
         earthquake = case_when(Disaster.Type=="Earthquake" ~ 1,
                                Disaster.Type=="Drought" ~ 0))

#d. Notice that some countries that had more than one earthquakes/droughts a year have multiple entries in some years. Use the group_by() and summarize() functions to create a data set where only one row of observation exists for each country and each year, such that:

disastergrouped <- disaster %>%
  group_by(Year,ISO) %>% 
  summarise(drought=sum(drought),earthquake=sum(earthquake))

#The new data set should contain the following variables:
#  i) Year ii) ISO
#iii) Dummy variable for earthquake iv) Dummy variable for drought

write.csv(disastergrouped, ("disasternew.csv"), row.names = FALSE)