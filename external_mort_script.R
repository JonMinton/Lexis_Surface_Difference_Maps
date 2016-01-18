# USA Fall in violence paper

# The aim of this session is to produce various figures which show how mortality rates due to assault/homicide 
# have varied with age, sex, and ethnicity over time in the USA. THe aim of doing this is to produce some a series 
# of figures which show the trends for as long a period as possible. In particular it is important to see if the trends 
#  follow those observed more generally about convergence between Blacks and Non-Blacks in age specific mortality rates 


 
# To do:  -----------------------------------------------------------------

# 1) look on CDC for longer term data on assault homicide
# 2) Look for existing data relevant to this and bring the code into this file only 



# Load packages -----------------------------------------------------------

rm(list=ls())

require(readr)
require(plyr)
require(tidyr)
require(dplyr)
require(stringr)

require(ggplot2)
require(lattice)
require(latticeExtra)
require(RColorBrewer)
require(grid)

require(xtable)

# for smoothing
require(fields) 
require(spatstat)



# Manage data (base and derived) ------------------------------------------



# Progress : 
#   
#   Found and extracted data on legal intervention from 1968-1974
#     - located in data/long_term_external_causes


# Broader category - 1968 to 1978- all external causes of death 
 # Males and females extracted separately because of file size limitations 
#E800-899 - external causes of death

# To do - same for females using ICD 8
# to do - same for males and females using ICD 9 
# To do - same for males and females using ICD 10



#Let's try to access this data

dta <- read_delim("data/long_term_external_causes/Compressed Mortality, 1968-1978.txt", delim = "\t", na = "Not Applicable", col_types = paste(rep("c", 14), collapse = ""))


# "Notes"	
# "Age Group"	
# "Age Group Code"	
# "Gender"	
# "Gender Code"	
# "Race"	
# "Race Code"	
# "Year"	
# "Year Code"	
# "Cause of death"	
# "Cause of death Code"	
# Deaths	
# Population	
# Crude Rate
legal_intervention_1968_1978 <- dta %>% filter(Notes == "Total") %>% 
  select(race = Race, sex = Gender, year = Year, age = `Age Group`, death_count = Deaths, population_count = Population) %>%
  mutate(death_count = as.integer(death_count), population_count = as.integer(population_count)) %>%
  filter(race != "", sex != "", age != "", year != "", !is.na(death_count), !is.na(population_count)) %>%
  mutate(age = str_trim(str_replace(str_replace(age, "year", ""), "s", ""))) 


#Death rate by year 
legal_intervention_1968_1978 %>% group_by(race, sex, year) %>%
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(death_rate = 100000 * death_count / population_count ) %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = death_rate, group = race, colour = race)) + 
              facet_wrap(~ sex, scales = "free_y")
  


dta <- read_delim("data/long_term_external_causes/external_males_Compressed Mortality, 1968-1978.txt", delim = "\t", na = "Not Applicable", col_types = paste(rep("c", 14), collapse = ""))


all_external_male_1968_1978 <- dta %>%  
  select(race = Race, sex = Gender, year = Year, age = `Age Group`, death_count = Deaths, population_count = Population) %>%
  mutate(death_count = as.numeric(death_count), population_count = as.numeric(population_count)) %>%
  filter(race != "", sex != "", age != "", year != "", !is.na(death_count), !is.na(population_count)) %>%
  mutate(age = str_trim(str_replace(str_replace(age, "year", ""), "s", ""))) %>%
  group_by(age, sex, race, year) %>%
  summarise(death_count = sum(death_count), population_count = sum(population_count))

all_external_male_1968_1978 %>% 
  group_by(year, sex, race) %>%
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  ggplot()  + 
  geom_line(aes(y = death_rate, x = year, group = race, colour = race))


dta <- read_delim("data/long_term_external_causes/external_Compressed Mortality, 1979-1998.txt", delim = "\t")
source("scripts/manage_data.R")


# Figures and tables -----------------