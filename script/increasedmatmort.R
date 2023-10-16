### week 6 part 2

### Write an R script that creates a figure that shows the trend in maternal mortality for countries that had an increase from 2000 to 2017

library(table1)
library(tidyverse)
library(here)
library(dplyr)
data <- read.csv(here("original/finaldata.csv"))

#create a new variable diffmatmor that shows the difference between maternal mortality of each year and maternal mortality in 2000
data <- data |>
  dplyr::select(country_name, ISO, Year, MatMort) |>
  dplyr::filter(Year < 2018) |>
  arrange(ISO, Year) |>
  group_by(ISO) |>
  mutate(diffmatmor = MatMort - MatMort[1L])

#now, need to select countries where diffmatmor for 2017>0
data2017 <- subset(data, Year==2017)
increased <- subset(data2017, diffmatmor>0)
#create vector of countries in subset
increased_countries <- c(increased$ISO)

#subset og dataset for countries that increased
increaseddata <- filter(data, ISO %in% increased_countries)

#trend plots
matmortplot <- increaseddata |>
  ggplot(aes(x=Year,y=MatMort,group = country_name, colour = country_name)) +
  geom_line() +
  xlim(c(2000,2017)) +
  labs(y="Maternal mortality ratio per 100,000 live births",x="Year") +
  theme_bw()

print(matmortplot + ggtitle("Countries with increased maternal mortality from 2000-2017"))
