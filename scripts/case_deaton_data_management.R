
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


# full range is now: 
# Alcohol induced [done]
# all cause mortality [done]
# assault mortality [done]
# chronic liver disease [done]
# drug induced [done]
# legal intervention [done]
# motor vehicle accidents 
# poisonings [done]
# suicides [done]
# vehicle transport [done]


alcohol_induced <- read_csv(
  "data/usa_multiple_cause/alcohol_induced_1999_2014.csv",
  col_types = "iciccii"
)

names(alcohol_induced) <- 
  c(
    "year", "sex", "age", "race", "hispanic", 
    "alcohol_induced",
    "population_1"
  )


assault_homicide <- read_csv(
  "data/usa_multiple_cause/assault_homicide_1999_2014.csv",
  col_types = "iciccii"
)

names(assault_homicide) <- 
  c(
    "year", "sex", "age", "race", "hispanic", 
    "assault_homicide", 
    "population_2"
  )
  

drug_induced <- read_csv(
  "data/usa_multiple_cause/drug_induced_1999_2014.csv",
  col_types = "iciccii"
)

names(drug_induced) <- 
  c(
    "year", "sex", "age", "race", "hispanic",
    "drug_induced", 
    "population_3"
    )
  
legal_intervention <- read_csv(
  "data/usa_multiple_cause/legal_intervention_1999_2014.csv",
  col_types = "iciccii"
)

names(legal_intervention) <- 
  c(
    "year", "sex", "age", "race", "hispanic",
    "legal_intervention", 
    "population_4"
  )

motor_accidents <- read_csv(
  "data/usa_multiple_cause/motor_vehicle_accidents_1999_2014.csv",
  col_types = "iciccii"
)

names(motor_accidents) <- 
  c(
    "year", "sex", "age", "race", "hispanic", 
    "motor_accidents",
    "population_5"
  )


vehicle_transport <- read_csv(
  "data/usa_multiple_cause/vehicle_transport_1999_2013.csv",
  col_types = "iicccii"
  )

names(vehicle_transport) <- 
  c(
      "year", "age", "sex", "race", "hispanic", 
      "vehicle_transport",
      "population_6"
  )



suicides <- read_csv("data/usa_multiple_cause/suicides_1999_2013.csv", col_types = "iicccii")
names(suicides) <- 
  c(
    "year", "age", "sex", "race", "hispanic", 
    "suicides",
    "population_7"
  )


poisonings <- read_csv("data/usa_multiple_cause/poisonings_1999_2013.csv", col_types = "iicccii")
names(poisonings) <- 
  c(
    "year", "age", "sex", "race", "hispanic", 
    "poisonings",
    "population_8"
  )


chronic_liver_disease <- read_csv("data/usa_multiple_cause/chronic_liver_diseases_1999_2013.csv", col_types = "iicccii")
names(chronic_liver_disease) <- 
  c(
    "year", "age", "sex", "race", "hispanic", 
    "chronic_liver_disease",
    "population_9"
  )





all_cause <- read_csv("data/usa_multiple_cause/all_cause_mortality_1999_2013.csv", col_types = "iicccii")
names(all_cause) <- 
  c(
    "year", "age", "sex", "race", "hispanic", 
    "all_cause",
    "population"
  )





merged <- all_cause %>% 
  full_join(alcohol_induced) %>% 
  full_join(assault_homicide) %>%
  full_join(chronic_liver_disease) %>%
  full_join(drug_induced) %>% 
  full_join(legal_intervention) %>% 
  full_join(poisonings) %>% 
  full_join(suicides) %>% 
  full_join(motor_accidents) %>% 
  full_join(vehicle_transport)

# merged <-  merged %>% mutate(
#   population = ifelse(!is.na(population_1), population_1, NA),
#   population = ifelse(is.na(population), population_2, population),
#   population = ifelse(is.na(population), population_3, population),
#   population = ifelse(is.na(population), population_4, population),
#   population = ifelse(is.na(population), population_5, population)
# )

merged <- merged %>% filter(!is.na(population)) %>% 
  select(
    year, age, sex, race, hispanic, 
    chronic_liver_disease, poisonings, suicides, vehicle_transport,
    alcohol_induced, assault_homicide, drug_induced, legal_intervention,
    motor_accidents, 
    all_cause, population
  )

tmp <- merged
tmp[,c(6:16)] <- apply(tmp[,c(6:16)], 2, function(x) {y <- x; y[is.na(y)] <- 0; return(y)})
    
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

