#directed reading week 2 in-class assignment

#a.
#Use the select() function in the dplyr package (part of tidyverse) subset the data to have only the variables Country.Name, X2000 – X2019.

library(readr)
maternalmortality <- read_csv("original/maternalmortality.csv")

# want columns 1 and 45-64
library(tidyverse)
maternalsubset <- maternalmortality %>% select(1, 45:64)
#this didn't work for some reason

#used txt file instead
maternalmort <- read_table("original/maternalmortality.txt", col_names = TRUE)
maternalmort <- read.csv("original/maternalmortality.txt", sep="")
maternalsubset <- maternalmort %>% select(Country.Name, X2000:X2019)

#b.
#The data set is currently in a wide format. Use the pivot_longer() function to convert the data set into a long format. (See PDF for example)

#Hint: You need to select the columns X2000 to X2019, remove the prefix X from them, change the name of the variable to Year, change the values to MatMor. Finally, make sure the year variable is stored as numeric.

#remove prefix X from column names
colnames(maternalsubset)<-gsub("X","",colnames(maternalsubset))

maternallong <- pivot_longer(maternalsubset,
                             cols=2:ncol(maternalsubset),
                             names_to = "Year", values_to = "MatMor", 
                             values_drop_na = FALSE,
                             names_transform = list(Year = as.numeric))

is.numeric(maternallong$Year)

#push to github
install.packages('usethis')
library(usethis)
usethis::use_git_config(user.name="kcaat",user.email="kat.lu@mail.utoronto.ca")
usethis::use_git()
usethis::create_github_token()
#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
gitcreds::gitcreds_set()
usethis::use_github()
