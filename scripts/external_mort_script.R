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
require(car)

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

dta <- read_delim("data/long_term_external_causes/legal_intervention_Compressed Mortality, 1968-1978.txt", delim = "\t", na = "Not Applicable", col_types = paste(rep("c", 14), collapse = ""))


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


# Homicide using ICD categories - homicide ICD 8 

dta_homicide_icd08 <- read_delim("data/long_term_external_causes/homicide_icd_8_Compressed Mortality, 1968-1978.txt", delim = "\t")

dta_hom_icd08_rate <- dta_homicide_icd08 %>% 
  filter(Notes == "Total") %>% 
  select(age = `Age Group`, sex = Gender, race = Race, year = Year, death_count = Deaths, population_count = Population) %>%
  select(age, year, race, sex, death_count, population_count) %>% 
  filter(!is.na(race), !is.na(sex), !is.na(year), !is.na(age), age != "Not Stated") %>%
  mutate(year = as.integer(year)) %>% 
  group_by(race, sex, year) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>%
  mutate(death_rate = 100000 * death_count / population_count) 

dta_hom_icd08_rate %>% 
  ggplot(.) +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  scale_y_log10(breaks = c(1, 5, 10, 50, 100))





# Homicide- ICD 9 

dta_homicide_icd09 <- read_delim("data/long_term_external_causes/homicide_icd9.txt", delim = "\t",
                                 col_types = "ccccciicccciid")

dta_hom_icd09_rate <- dta_homicide_icd09 %>% 
  filter(Notes == "Total") %>% 
  select(age = `Age Group`, sex = Gender, race = Race, year = Year, death_count = Deaths, population_count = Population) %>%
  select(age, year, race, sex, death_count, population_count) %>% 
  filter(!is.na(race), !is.na(sex), !is.na(year), !is.na(age), age != "Not Stated") %>%
  filter(race != "") %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(race, sex, year) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>%
  mutate(death_rate = 100000 * death_count / population_count)

dta_hom_icd09_rate %>% 
  ggplot(.) +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  scale_y_log10(breaks= c(1, 5, 10, 50, 100))




# Homicide - ICD 10 

#  Can't find a homicide category so looking at assault only 

dta_assault_icd10 <- read_delim("data/long_term_external_causes/assault_icd10_hisprace.txt", delim = "\t")

dta_assault_icd10_rate <- dta_assault_icd10 %>% 
  select(age = `Age Group`, year = Year, sex = Gender, race = Race, hispanic = `Hispanic Origin`, death_count = Deaths, population_count = Population) %>%
  filter(race = !is.na(race), age = !is.na(age), year = !is.na(year), sex = !is.na(sex)) %>%
  mutate(
    group = NA,
    group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "White Non-Hispanic", group),
    group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "Black Non-Hispanic", group),
    group = ifelse(hispanic == "Hispanic or Latino", "Hispanic", group)
  ) %>% 
  filter(!is.na(group)) %>% 
  select(sex, race = group, age, year, death_count, population_count) %>% 
  mutate(population_count = as.numeric(population_count)) %>% 
  filter(population_count = !is.na(population_count)) %>% 
  group_by(sex, race, year) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(death_rate = 100000 * death_count / population_count) 

dta_assault_icd10_rate %>% 
  ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  scale_y_log10(breaks = c(1, 5, 10, 50, 100))



# Group the above together 

dta_hom_icd08_rate
dta_hom_icd09_rate

dta_hom_icd08_09_rate <- rbind(dta_hom_icd08_rate, dta_hom_icd09_rate)

dta_hom_icd08_09_rate %>% 
  ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y")


tmp_10 <- dta_assault_icd10_rate %>% 
  select(r1 = race, sex, year, death_count, population_count, death_rate) %>% 
  mutate(
    r2 = recode(
      r1, 
      "
      'Black Non-Hispanic' = 'Black';
      'White Non-Hispanic' = 'White';
      'Hispanic' = 'Hispanic/Other'
      "
    )
  ) %>% ungroup() %>% 
  select(race = r2, sex, year, death_count, population_count, death_rate) 




tmp_08_09 <- dta_hom_icd08_09_rate %>%
  select(r1 = race, sex, year, death_count, population_count, death_rate) %>% 
  mutate(
    r2 = recode(
      r1, 
      "
      'Black or African American' = 'Black';
      'White' = 'White';
      'Other Race' = 'Hispanic/Other'
      "
    )
    ) %>% ungroup() %>%
  select(race = r2, sex, year, death_count, population_count, death_rate) 
  

homassault_icd_08_09_10_rates <- rbind(tmp_08_09, tmp_10)


homassault_icd_08_09_10_rates %>% 
  ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  geom_vline(xintercept = c(1979, 1999)) +
  scale_y_log10(breaks= c(1, 2, 5, 10, 20, 50, 100)) + 
  labs(y = "Death rate from assault/homicide per 100 000")


# Grouping together hispanic/other and non-black for more comparability with ICD-08 and ICD-09 groups
homassault_icd_08_09_10_rates %>% 
  select(race, sex, year, death_rate) %>%
  spread(key = race, value = death_rate) %>%
  mutate(`Non-Black` = (White * `Hispanic/Other`)^0.5) %>%
  gather(key = race, value = death_rate, -sex, -year) %>%
  mutate(
    death_rate = ifelse(race == "White" & year >= 1999, NA, death_rate),
    death_rate = ifelse(race == "Hispanic/Other" & year >= 1999, NA, death_rate),
    death_rate = ifelse(race == "Non-Black" & year < 1999, NA, death_rate)
    ) %>% 
  ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  geom_vline(xintercept = c(1979, 1999)) +
  scale_y_log10(breaks= c(1, 2, 5, 10, 20, 50, 100)) + 
  labs(y = "Death rate from assault/homicide per 100 000")



# Now to do look at data on death from external causes  (The supercategory)

dta_ext_icd08 <- read_delim("data/long_term_external_causes/external_icd08.txt", delim = "\t")

dta_ext_icd09 <- read_delim("data/long_term_external_causes/external_icd09.txt", delim = "\t")

dta_ext_icd10 <- read_delim("data/long_term_external_causes/external_icd10.txt", delim = "\t")


ext_icd08_rate <- dta_ext_icd08 %>% 
  select(
    age = `Age Group`, sex = Gender, 
    race = Race, year = Year, 
    death_count = Deaths, population_count = Population) %>%
  mutate(population_count = as.numeric(population_count)) %>% 
  filter(!is.na(race), !is.na(sex), !is.na(year), !is.na(age), age != "Not Stated") %>%
  filter(race != "") %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(race, sex, year) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>%
  mutate(death_rate = 100000 * death_count / population_count)

ext_icd08_rate %>%
  ggplot() + 
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex) + 
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200))


ext_icd09_rate <- dta_ext_icd09 %>% 
select(
  age = `Age Group`, sex = Gender, 
  race = Race, year = Year, 
  death_count = Deaths, population_count = Population) %>%
mutate(population_count = as.numeric(population_count)) %>% 
filter(!is.na(race), !is.na(sex), !is.na(year), !is.na(age), age != "Not Stated") %>%
filter(race != "") %>% 
mutate(year = as.integer(year)) %>% 
group_by(race, sex, year) %>% 
summarise(death_count = sum(death_count), population_count = sum(population_count)) %>%
mutate(death_rate = 100000 * death_count / population_count)

ext_icd09_rate %>%
  ggplot() + 
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex) + 
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200))


ext_icd08_09_rate <- rbind(ext_icd08_rate, ext_icd09_rate)


ext_icd10_rate <- dta_ext_icd10 %>% 
select(age = `Age Group`, year = Year, sex = Gender, race = Race, hispanic = `Hispanic Origin`, death_count = Deaths, population_count = Population) %>%
filter(race = !is.na(race), age = !is.na(age), year = !is.na(year), sex = !is.na(sex)) %>%
mutate(
  group = NA,
  group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "White Non-Hispanic", group),
  group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "Black Non-Hispanic", group),
  group = ifelse(hispanic == "Hispanic or Latino", "Hispanic", group)
) %>% 
filter(!is.na(group)) %>% 
select(sex, race = group, age, year, death_count, population_count) %>% 
mutate(population_count = as.numeric(population_count)) %>% 
filter(population_count = !is.na(population_count)) %>% 
group_by(sex, race, year) %>% 
summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
mutate(death_rate = 100000 * death_count / population_count) 

ext_icd10_rate %>% 
  ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200))



tmp_10 <- ext_icd10_rate %>% 
  select(r1 = race, sex, year, death_count, population_count, death_rate) %>% 
  mutate(
    r2 = recode(
    r1, 
    "
    'Black Non-Hispanic' = 'Black';
    'White Non-Hispanic' = 'White';
    'Hispanic' = 'Hispanic/Other'
    "
    )
  ) %>% ungroup() %>% 
  select(race = r2, sex, year, death_count, population_count, death_rate) 




tmp_08_09 <- ext_icd08_09_rate %>%
  select(r1 = race, sex, year, death_count, population_count, death_rate) %>% 
  mutate(
    r2 = recode(
      r1, 
    "
    'Black or African American' = 'Black';
    'White' = 'White';
    'Other Race' = 'Hispanic/Other'
    "
  )
  ) %>% ungroup() %>%
select(race = r2, sex, year, death_count, population_count, death_rate) 

ext_icd_08_09_10_rates <- rbind(tmp_08_09, tmp_10)


ext_icd_08_09_10_rates %>% 
ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  geom_vline(xintercept = c(1979, 1999)) +
  scale_y_log10(breaks= c(1, 2, 5, 10, 20, 50, 100, 200)) + 
  labs(y = "Death rate from all external causes per 100 000")




# Now to do the above but with suicides and undetermined 




