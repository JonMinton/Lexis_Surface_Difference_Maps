

# Synthetic Cohort Simulations - Scotland in Context ----------------------


# The purpose of this script is to produce a series of models which illustrate the effect that the 
#  different all cause mortality rates at different ages in Scotland, and in the UK, compared with 
#  the UK and the rest of Western Europe, has on different numbers of deaths by different ages. 
#  
#  This is done by imagining cohorts of 100 000 people, 50 000 males and 50 000 females, aging
# in both the comparator and the reference region , and experiencing either the gender and age-specific 
#  mortality rates observed either in particular periods (synthetic cohorts) or for particular birth cohorts
#  
#  Lifetables based on these different age/sex/country/region specific schedules are produced and cohorts 
#  are 'aged' and 'die' according to these lifetables. The cumulative numbers per 100 000 population still alive 
#  by particular ages are estimated for both comparator and reference region, and the differences between these 
#  two values are calculated to produce estimates of the numbers of 'excess deaths' by age X associated with 
#  living in the comparator compared with the reference region. 
#  


# Definitions of regions --------------------------------------------------


#  
# UK
# - Northern Ireland
# - England & Wales
# - Scotland

# Western Europe:
# - Northern Ireland 
# - England & Wales 
# - Scotland 
# - Ireland
# - France
# - Belgium
# - Luxembourg
# - Netherlands
# - Germany
# - Switzerland
# - Austria

# Prereqs -----------------------------------------------------------------

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
require(xlsx)



# functions ---------------------------------------------------------------
source("scripts/smoother_function.R")
source("scripts/scotland_in_context__helper_functions.R")


# base data  --------------------------------------------------------------


dta <- read_csv("data/tidy/counts_germany_combined.csv")

# country group definitions
source("scripts/scotland_in_context__country_group_definitions.R")


# country_group_selections ------------------------------------------------

dta_uk <- dta %>% filter(country %in% uk_codes)
dta_we <- dta %>% filter(country %in% w_europe_codes)
dta_europe <- dta %>% filter(country %in% europe_codes)
dta_all <- dta %>% filter(country %in% all_codes)
dta_ne <- dta %>% filter(country %in% europe_northern)
dta_se <- dta %>% filter(country %in% europe_southern)
dta_ee <- dta %>% filter(country %in% europe_eastern)
dta_na <- dta %>% filter(country %in% north_america)
dta_anglo <- dta %>% filter(country %in% anglophone)

dta_uk_noscot <- dta_uk %>% filter(country !="GBR_SCO")
dta_we_noscot <- dta_we %>% filter(country !="GBR_SCO")
dta_europe_noscot <- dta_europe %>% filter(country !="GBR_SCO")
dta_all_noscot <- dta_all %>% filter(country !="GBR_SCO")
dta_anglo_noscot <- dta_anglo %>% filter(country !="GBR_SCO")


# country group selections - smoothed -------------------------------------

fn <- function(DTA) {
  out <- DTA %>%   
    mutate(
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base=10)     
    ) %>% smooth_var(
      dta =.,
      group_vars= c("country", "sex"),
      smooth_var = "lg_cmr",
      smooth_par = 1.3
    ) 
  return(out)
}


dta_uk_smoothed <- fn(dta_uk)
dta_we_smoothed <- fn(dta_we)
dta_europe_smoothed <- fn(dta_europe)
dta_all_smoothed <- fn(dta_all)
dta_ne_smoothed <- fn(dta_ne)
dta_se_smoothed <- fn(dta_se)
dta_ee_smoothed <- fn(dta_ee)
dta_na_smoothed <- fn(dta_na)
dta_anglo_smoothed <- fn(dta_anglo)


dta_uk_noscot_smoothed <- fn(dta_uk_noscot) 
dta_we_noscot_smoothed <- fn(dta_we_noscot) 
dta_europe_noscot_smoothed <- fn(dta_europe_noscot)
dta_all_noscot_smoothed <- fn(dta_all_noscot)
dta_anglo_noscot_smoothed <- fn(dta_anglo_noscot)



rm(fn)
# country group selections - combined  ---------------------------------------


dta_uk_overall <- grouper(dta_uk)
dta_we_overall <- grouper(dta_we)
dta_europe_overall <- grouper(dta_europe)
dta_all_overall <- grouper(dta_all)
dta_ne_overall <- grouper(dta_ne)
dta_se_overall <- grouper(dta_se)
dta_ee_overall <- grouper(dta_ee)
dta_na_overall <- grouper(dta_na)
dta_anglo_overall <- grouper(dta_anglo)


dta_uk_noscot_overall <- grouper(dta_uk_noscot)
dta_we_noscot_overall <- grouper(dta_we_noscot)
dta_europe_noscot_overall <- grouper(dta_europe_noscot)
dta_all_noscot_overall <- grouper(dta_all_noscot)
dta_anglo_noscot_overall <- grouper(dta_anglo_noscot)





# country group selections - combined and smoothed -------------------------------




# Tables showing excess deaths by particular ages per 100 000 popu --------


ages <- c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80)

init_cohort_size <- 50000 # 50 000 as males and females 
# need to be combined
calculate_survivors <- function(x, init_size = init_cohort_size){
    k <- dim(x)[1]
    cohort_size <- rep(NA, k)
    cohort_size[1] <- init_size
    for (i in 2:k){
      cohort_size[i] <- cohort_size[i - 1] * (1 - x$death_rate[i - 1])
    }
    output <- data.frame(x, cohort_size = cohort_size)
}

# Period-based, Scotland against rest of UK -------------------------------

dta_scot <- dta %>% 
  filter(country == "GBR_SCO") %>% 
  filter(year >= 1950) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland")

dta_ruk <- dta_uk_noscot_overall %>% 
  mutate(country = "ruk") %>% 
  filter(sex != "total") %>% 
  select(country, year, age, sex, death_count, population_count) %>% 
  filter(year >= 1950)

dta_both <- bind_rows(dta_ruk, dta_scot)

# for each region, year, and sex, produce cumulative mortality by 
# different ages 



dta_synth <- dta_both %>% 
  arrange(country, year, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, year, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)

# Write out this for an appendix
#write_csv(x = dta_synth, path = "tables/period_based_lifetables_scotland_ruk.csv" )


# this calculates and presents the excess deaths per 100 000 population 
# for each decade by certain ages 
dta_synth %>% 
  select(country, year, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = scotland - ruk) %>% 
  filter(age %in% ages) %>% 
  mutate(
    decade = cut(
      year, 
      seq(1950, 2010, by = 10), 
      include.lowest = T,
      labels = c("1950s", "1960S", "1970S", "1980S", "1990s", "2000s")
    )) %>% 
  group_by(decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total) %>% 
  filter(!is.na(decade)) -> excess_deaths_scot_ruk



# Now to compare Scotland with rest of Western Europe

dta_scot <- dta %>% 
  filter(country == "GBR_SCO") %>% 
  filter(year >= 1950) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland")

dta_rwe <- dta_we_noscot_overall %>% 
  mutate(country = "rwe") %>% 
  filter(sex != "total") %>% 
  select(country, year, age, sex, death_count, population_count) %>% 
  filter(year >= 1950)

dta_both <- bind_rows(dta_rwe, dta_scot)


dta_synth <- dta_both %>% 
  arrange(country, year, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, year, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)

# Save this for an appendix
#write_csv(dta_synth, path = "tables/period_based_lifetables_scotland_rwe.csv")

# this calculates and presents the excess deaths per 100 000 population 
# for each decade by certain ages 

dta_synth %>% 
  select(country, year, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = scotland - rwe) %>% 
  filter(age %in% ages) %>% 
  mutate(
    decade = cut(
      year, 
      seq(1950, 2010, by = 10), 
      include.lowest = T,
      labels = c("1950s", "1960S", "1970S", "1980S", "1990s", "2000s")
    )) %>% 
  group_by(decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total) %>% 
  filter(!is.na(decade)) -> excess_deaths_scot_rwe


# Now to compare UK with rest of Western Europe

dta_rwe <- dta_we %>% 
  filter(!(country %in% c("GRBCENW", "GBR_NIR", "GBR_SCO"))) %>% 
  filter(year >= 1950) %>%
  filter(sex != "total") %>% 
  group_by(year, age, sex) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(country = "rwe") %>% 
  select(country, year, age, sex, death_count, population_count)

dta_uk <- dta_uk_overall %>% 
  mutate(country = "uk") %>% 
  filter(sex != "total") %>% 
  select(country, year, age, sex, death_count, population_count) %>% 
  filter(year >= 1950)

dta_both <- bind_rows(dta_rwe, dta_uk)


dta_synth <- dta_both %>% 
  arrange(country, year, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, year, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)

write_csv(dta_synth, path = "tables/period_based_lifetables_uk_rwe.csv")

# this calculates and presents the excess deaths per 100 000 population 
# for each decade by certain ages 

dta_synth %>% 
  select(country, year, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = uk - rwe) %>% 
  filter(age %in% ages) %>% 
  mutate(
    decade = cut(
      year, 
      seq(1950, 2010, by = 10), 
      include.lowest = T,
      labels = c("1950s", "1960S", "1970S", "1980S", "1990s", "2000s")
    )) %>% 
  group_by(decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total) %>% 
  filter(!is.na(decade)) -> excess_deaths_uk_rwe


# Something similar by cohort 


# cohort-based, Scotland against rest of UK -------------------------------

dta_scot <- dta %>%
  mutate(birth_cohort = year - age) %>% 
  filter(country == "GBR_SCO") %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland") %>% 
  select(country, birth_cohort, age, sex, death_count, population_count)


dta_ruk <- dta_uk_noscot_overall %>%
  mutate(birth_cohort = year - age) %>% 
  mutate(country = "ruk") %>% 
  filter(sex != "total") %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  select(country, birth_cohort, age, sex, death_count, population_count) 
  
dta_both <- bind_rows(dta_ruk, dta_scot)

dta_synth <- dta_both %>% 
  arrange(country, birth_cohort, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, birth_cohort, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)

# Write out 
write_csv(dta_synth, "tables/cohort_based_lifetables_scotland_ruk.csv")

dta_synth %>% 
  select(country, birth_cohort, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = scotland - ruk) %>% 
  filter(age %in% ages) %>% 
  mutate(
    cohort_decade = cut(
      birth_cohort, 
      seq(1930, 1980, by = 10), 
      include.lowest = T,
      labels = c("1930s", "1940S", "1950S", "1960S", "1970s")
    )) %>% 
  group_by(cohort_decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(cohort_decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total)  -> cohort_excess_deaths_scot_ruk


# Cohort-based, Scotland compared with rest of WE

dta_scot <- dta %>%
  mutate(birth_cohort = year - age) %>% 
  filter(country == "GBR_SCO") %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland") %>% 
  select(country, birth_cohort, age, sex, death_count, population_count)


dta_rwe <- dta_we_noscot_overall %>% 
  mutate(birth_cohort = year - age) %>% 
  mutate(country = "rwe") %>% 
  filter(sex != "total") %>%   
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  select(country, birth_cohort, age, sex, death_count, population_count)  


dta_both <- bind_rows(dta_rwe, dta_scot)


dta_synth <- dta_both %>% 
  arrange(country, birth_cohort, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, birth_cohort, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)

# write out 
#write_csv(dta_both, path = "tables/cohort_based_lifetables_scotland_rwe.csv")

dta_synth %>% 
  select(country, birth_cohort, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = scotland - rwe) %>% 
  filter(age %in% ages ) %>% 
  mutate(
    cohort_decade = cut(
      birth_cohort, 
      seq(1930, 1980, by = 10), 
      include.lowest = T,
      labels = c("1930s", "1940S", "1950S", "1960S", "1970s")
    )) %>% 
  group_by(cohort_decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(cohort_decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total)  -> cohort_excess_deaths_scot_rwe


# Now to compare UK with rest of Western Europe

dta_rwe <- dta_we %>% 
  filter(!(country %in% c("GRBCENW", "GBR_NIR", "GBR_SCO"))) %>% 
  mutate(birth_cohort = year - age) %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  filter(sex != "total") %>% 
  group_by(birth_cohort, age, sex) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(country = "rwe") %>% 
  select(country, birth_cohort, age, sex, death_count, population_count)


dta_uk <- dta_uk_overall %>% 
  mutate(country = "uk") %>% 
  mutate(birth_cohort = year - age) %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  filter(sex != "total") %>% 
  select(country, birth_cohort, age, sex, death_count, population_count)  

dta_both <- bind_rows(dta_rwe, dta_uk)


dta_synth <- dta_both %>% 
  arrange(country, birth_cohort, sex, age) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  group_by(country, birth_cohort, sex) %>% 
  do(calculate_survivors(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)

#write_csv(dta_synth, path = "tables/cohort_based_lifetables_uk_rwe.csv") 

dta_synth %>% 
  select(country, birth_cohort, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = uk - rwe) %>% 
  filter(age %in% ages ) %>% 
  mutate(
    cohort_decade = cut(
      birth_cohort, 
      seq(1930, 1980, by = 10), 
      include.lowest = T,
      labels = c("1930s", "1940S", "1950S", "1960S", "1970s")
    )) %>% 
  group_by(cohort_decade, sex, age) %>% 
  summarise(excess_deaths = sum(excess_deaths) / length(excess_deaths)) %>% 
  spread(sex, excess_deaths) %>% 
  mutate(total = female + male) %>% 
  select(cohort_decade, age, total) %>%
  mutate(total = round(total, 0)) %>% 
  spread(age, total)  -> cohort_excess_deaths_uk_rwe



# produce excel table of these period excess charts 

wb <- createWorkbook()

addDataFrame(
  x = excess_deaths_scot_ruk, 
  sheet = createSheet(wb, sheetName = "period_Scotland_less_rUK")
  )

addDataFrame(
  x = excess_deaths_scot_rwe, 
  sheet = createSheet(wb, sheetName = "period_Scotland_less_rWE")
)

addDataFrame(
  x = excess_deaths_uk_rwe, 
  sheet = createSheet(wb, sheetName = "period_UK_less_rWE")
)

addDataFrame(
  x = cohort_excess_deaths_scot_ruk, 
  sheet = createSheet(wb, sheetName = "cohort_Scotland_less_rUK")
)

addDataFrame(
  x = cohort_excess_deaths_scot_rwe, 
  sheet = createSheet(wb, sheetName = "cohort_Scotland_less_rWE")
)

addDataFrame(
  x = cohort_excess_deaths_uk_rwe, 
  sheet = createSheet(wb, sheetName = "cohort_UK_less_rWE")
)

saveWorkbook(wb, 
             file = "tables/scotland_excess_deaths.xlsx"
             )

# Now to do the above but for each of the components


