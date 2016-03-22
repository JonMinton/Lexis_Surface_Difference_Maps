# Script for looking at differences in infant mortality in all available countries for each half decade


# Load packages -----------------------------------------------------------

rm(list=ls())


require(readxl)
require(xlsx)
require(readr)

require(plyr)

require(tidyr)
require(dplyr)

require(stringr)
require(car)

require(ggplot2)

require(xtable)

# Infant mortality trends 

dta <- read_csv("data/tidy/counts.csv")


imr_data <- dta  %>% 
  filter(age == 0)  %>% 
  filter(year %in% c(seq(1950, 2005, 5), 2009))  %>% 
  filter(sex == "total")  %>% 
  mutate(imr = 100000 * death_count / population_count)  %>% 
  select(country, year, imr)


countries <- unique(imr_data$country)
countries <- countries[!countries %in% c("DEUTNP", "FRACNP", "GBRCENW", "NZL_MA", "NZL_NM")]

years <- unique(imr_data$year)

imr_value_matrix <- matrix(NA, nrow = length(countries), ncol = length(years), dimnames = list(countries, years))

for(this_year in years){
  for(this_country in countries){
    this_value <- try(
      imr_data %>% 
        filter(year == this_year) %>% 
        filter(country == this_country) %>% 
        .$imr,
      silent = TRUE
    )
    cat(this_year, "\t", this_country, "\t", this_value, "\n")
    if(length(this_value) == 1){
      imr_value_matrix[as.character(this_country), as.character(this_year)] <- this_value}
  }
}

# Now rank by year



