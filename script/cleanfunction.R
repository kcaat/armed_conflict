### in class

### week 3

#covariates data only includes countries in list provided to aya by author
  #use this list

#her RPub
#has raw data documentation
#can use RPub country code package to convert ISO to country names
  #use ISO3 codes


#a. Create a function that performs the same procedure on the maternal mortality data so that you can apply the same steps on infant mortality, neonatal mortality, and under 5 mortality.

library(tidyverse)

cleandata <- function(dataset, statistic){
  #Use the select() function in the dplyr package (part of tidyverse) subset the data to have   only the variables Country.Name, X2000 – X2019.
  dataset <- dataset %>% select(Country.Name, X2000:X2019)
  
  #remove prefix X from column names
  colnames(dataset)<-gsub("X","",colnames(dataset))
  
  #Use the pivot_longer() function to convert the data set from wide to long format.
  dataset <- pivot_longer(dataset,
                               cols=c('2000':'2019'),
                               names_to = "Year", values_to = statistic, 
                               values_drop_na = FALSE,
                               names_transform = list(Year = as.numeric))
}

#b. Apply the function to each of the four data sets and create four “new” data sets.

maternal <- read.csv("neonatalmortality.csv")
cleanmaternal <- cleandata(maternal, 'MatMort')

neonatal <- read.csv("neonatalmortality.csv")
cleanneonatal <- cleandata(neonatal, 'NeoMort')

infant <- read.csv("infantmortality.csv")
cleaninfant <- cleandata(infant, 'InfMort')

under5 <- read.csv("under5mortality.csv")
cleanunder5 <- cleandata(under5, 'Under5Mort')

#c. Use the reduce() and full_join() functions to merge the four data sets to create one data set that contains the following variables (pick your own variable names):
  #  i) Country name ii) Year
  #iii) Maternal mortality rate iv) Infant mortality rate
  #v) Neonatal mortality rate vi) Under 5 mortality rate

list_data <- list(cleanmaternal, cleanneonatal, cleaninfant, cleanunder5)
merged <- reduce(list_data, full_join)
head(merged)

#d. Use the countrycode() function in the countrycode package to add the ISO-3 country code variable to the new data set created in Step c. Call the new variable ISO. Then remove the Country name variable.

library(here)
library(tidyverse)
library(data.table)
install.packages("states")
install.packages("countrycode")
library(states)
library(countrycode)

#create new column ISO in merged dataset, populate using countrycode function
merged$ISO <- countrycode(merged$Country.Name, 
                             origin = "country.name", 
                             destination = "iso3c")
head(countries)

#Then remove the Country name variable.
merged <- merged[,-1]

#e. Save the new data set.
write.csv(merged, ("mergedmortality.csv"), row.names = FALSE)
