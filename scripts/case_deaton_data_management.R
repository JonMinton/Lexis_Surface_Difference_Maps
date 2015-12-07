
#4/12/2015
# case deaton data munging


# Focus on - Non-hispanic Whites
# 1) Non-Hispanic Whites
# 2) Black Non-Hispanic
# 3) Combined Hispanic



rm(list=ls())

# data management
require(readr)

require(plyr)
require(tidyr)
require(stringr)
require(dplyr)


#graphics
require(lattice)
require(latticeExtra)
require(ggplot2) 
require(RColorBrewer)
require(grid)

# for smoothing
require(fields) 
require(spatstat)

#tables
require(xtable)


vehicle_transport <- read_csv(
  "data/usa_multiple_cause/vehicle_transport_1999_2013.csv",
  col_types = "iicccii"
  )

names(vehicle_transport) <- 
  c(
      "year", "age", "sex", "race", "hispanic", 
      "vehicle_transport",
      "population_1"
  )



suicides <- read_csv("data/usa_multiple_cause/suicides_1999_2013.csv", col_types = "iicccii")
names(suicides) <- 
  c(
    "year", "age", "sex", "race", "hispanic", 
    "suicides",
    "population_2"
  )


poisonings <- read_csv("data/usa_multiple_cause/poisonings_1999_2013.csv", col_types = "iicccii")
names(poisonings) <- 
  c(
    "year", "age", "sex", "race", "hispanic", 
    "poisonings",
    "population_3"
  )


chronic_liver_disease <- read_csv("data/usa_multiple_cause/chronic_liver_diseases_1999_2013.csv", col_types = "iicccii")
names(chronic_liver_disease) <- 
  c(
    "year", "age", "sex", "race", "hispanic", 
    "chronic_liver_disease",
    "population_4"
  )


all_cause <- read_csv("data/usa_multiple_cause/all_cause_mortality_1999_2013.csv", col_types = "iicccii")
names(all_cause) <- 
  c(
    "year", "age", "sex", "race", "hispanic", 
    "all_cause",
    "population_5"
  )


merged <- all_cause %>% 
  full_join(chronic_liver_disease) %>% 
  full_join(poisonings) %>% 
  full_join(suicides) %>% 
  full_join(vehicle_transport)

merged <-  merged %>% mutate(
  population = ifelse(!is.na(population_1), population_1, NA),
  population = ifelse(is.na(population), population_2, population),
  population = ifelse(is.na(population), population_3, population),
  population = ifelse(is.na(population), population_4, population),
  population = ifelse(is.na(population), population_5, population)
)

merged <- merged %>% filter(!is.na(population)) %>% 
  select(
    year, age, sex, race, hispanic, 
    chronic_liver_disease, poisonings, suicides, vehicle_transport,
    all_cause, population
  )

tmp <- merged
tmp[,c(6:10)] <- apply(tmp[,c(6:10)], 2, function(x) {y <- x; y[is.na(y)] <- 0; return(y)})
    
merged <- tmp

# Focus on - Non-hispanic Whites
# 1) Non-Hispanic Whites
# 2) Black Non-Hispanic
# 3) Combined Hispanic

merged$group <- NA

tmp <- merged %>% mutate(
  group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "White Non-Hispanic", group),
  group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "Black Non-Hispanic", group),
  group = ifelse(hispanic == "Hispanic or Latino", "Hispanic", group)
  )

merged <- tmp

merged <- merged %>% 
  select(-race, -hispanic) %>% 
  filter(!is.na(group)) %>%
  group_by(year, age, sex, group) %>% 
  summarise_each(funs(sum)) 


# now to save

write_csv(x = merged, path = "data/usa_multiple_cause/tidied_and_simplified.csv")



# Exploration of raw data  ------------------------------------------------


# Data by single age from CDC wonder database itself 

dta <- read_delim("data/usa_multiple_cause/external causes of death.txt", delim = "\t")

dta %>% 
  select(-Notes) %>% 
  rename(
    age = `Single-Year Ages Code`,
    sex = Gender,
    race = Race,
    hispanic = `Hispanic Origin`
    ) %>% 
  select(
    sex, 
    race, 
    hispanic, 
    year = Year, 
    age, 
    deaths = Deaths, population = Population
    ) %>% 
  group_by(race, hispanic) %>% 
  tally



simple_dta <- dta %>% 
  select(-Notes) %>% 
  rename(
    age = `Single-Year Ages Code`,
    sex = Gender,
    race = Race,
    hispanic = `Hispanic Origin`
  ) %>% 
  select(
    sex, 
    race, 
    hispanic, 
    year = Year, 
    age, 
    deaths = Deaths, population = Population
  ) 


simple_dta %>% 
  mutate(
    group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "white", NA),
    group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "black", group), 
    group = ifelse(hispanic =="Hispanic or Latino", "hispanic", group)
  ) 

