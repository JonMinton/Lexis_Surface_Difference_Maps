
# Prereqs -----------------------------------------------------------------

rm(list=ls())

pacman::p_load(
  readr, stringr, tidyr, 
  dplyr, purrr,
  xlsx
)

# base data  --------------------------------------------------------------

# country group definitions
source("scripts/scotland_in_context__country_group_definitions.R")


#########################################################
#########################################################
# Now to use exposures rather than populations, and 
# to use the formulae specified around p 38 of the following:
# http://www.mortality.org/Public/Docs/MethodsProtocol.pdf

proper_counts <- read_csv("data/tidy/new_counts.csv")


# First to combine East and West Germany 
counts_deut <- proper_counts %>% 
  filter(country_code %in% c("DEUTE", "DEUTW"))  %>% 
  group_by(year, age, sex)  %>% 
  summarise(
    deaths = sum(deaths), 
    population = sum(population), 
    exposure = sum(exposure)
  )  %>% 
  ungroup() %>% 
  mutate(country_code = "DEUT") %>% 
  select(country_code, year, age, sex, deaths, population, exposure)

proper_counts %>% filter(!country_code %in% c("DEUTE", "DEUTW")) %>% 
  bind_rows(counts_deut) -> proper_counts

proper_counts <- proper_counts %>% rename(country = country_code)

# Produce function for producing q_x, probability of death within year 

calc_qx <- function(age, sex, deaths, exposure){
  m_x <-  deaths / exposure
  a_x <-  if(age == 0)
  {
    if(m_x > 0.107) 
    {
      if (sex == "female")
      {
        0.350
      } else 
      {
        0.330  
      }
      
    } else 
    {
      if (sex == "female") 
      {
        0.053 + 2.800 * m_x
      } else 
      {
        0.045 + 2.684 * m_x
      }
    }
  } else 
  {
    0.5
  }
  
  q_x = m_x / (1 + (1 - a_x) * m_x)
  
}



proper_counts <- proper_counts %>% 
  mutate(
    q_x = pmap_dbl(
      .l = list(
        age = age,
        sex = sex,
        deaths = deaths, 
        exposure = exposure
      ), 
      .f = possibly(calc_qx, otherwise = NA)
    ),
    p_x = 1 - q_x
  )


# country_group_selections ------------------------------------------------

dta_uk <- proper_counts %>% filter(country %in% uk_codes)
dta_we <- proper_counts %>% filter(country %in% w_europe_codes)
dta_europe <- proper_counts %>% filter(country %in% europe_codes)
dta_all <- proper_counts %>% filter(country %in% all_codes)
dta_ne <- proper_counts %>% filter(country %in% europe_northern)
dta_se <- proper_counts %>% filter(country %in% europe_southern)
dta_ee <- proper_counts %>% filter(country %in% europe_eastern)
dta_na <- proper_counts %>% filter(country %in% north_america)
dta_anglo <- proper_counts %>% filter(country %in% anglophone)

dta_uk_noscot <- dta_uk %>% filter(country !="GBR_SCO")
dta_we_noscot <- dta_we %>% filter(country !="GBR_SCO")
dta_europe_noscot <- dta_europe %>% filter(country !="GBR_SCO")
dta_all_noscot <- dta_all %>% filter(country !="GBR_SCO")
dta_anglo_noscot <- dta_anglo %>% filter(country !="GBR_SCO")


# country group selections - combined  ---------------------------------------

grouper <- function(x){
  x %>% 
    select(-country) %>% 
    group_by(year, age, sex) %>% 
    summarise(
      deaths = sum(deaths),
      population = sum(population),
      exposure = sum(exposure)
  ) %>% 
    ungroup()
}

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




 ages <- c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80)
# 
init_cohort_size <- 50000 # 50 000 as males and females
# need to be combined
calculate_survivors_properly <- function(x, init_size = init_cohort_size){
  k <- dim(x)[1]
  cohort_size <- rep(NA, k)
  cohort_size[1] <- init_size * x$p_x[1]
  for (i in 2:k){
    cohort_size[i] <- cohort_size[i - 1] * x$p_x[i]
  }
  output <- data.frame(x, cohort_size = cohort_size)
}

# Period-based, Scotland against rest of UK -------------------------------

dta_scot <- proper_counts %>% 
  filter(country == "GBR_SCO") %>% 
  filter(year >= 1950) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland")

dta_ruk <- dta_uk_noscot_overall %>% 
  mutate(country = "ruk") %>% 
  filter(sex != "total") %>% 
  select(country, year, age, sex, deaths, population, exposure) %>% 
  filter(year >= 1950)

dta_both <- bind_rows(dta_ruk, dta_scot)

# for each region, year, and sex, produce cumulative mortality by 
# different ages 

dta_synth <- dta_both %>% 
  mutate(
    q_x = pmap_dbl(
      .l = list(
        age = age,
        sex = sex,
        deaths = deaths, 
        exposure = exposure
      ), 
      .f = possibly(calc_qx, otherwise = NA)
    ),
    p_x = 1 - q_x
  ) %>% 
  arrange(country, year, sex, age) %>%
  group_by(country, year, sex) %>% 
  do(calculate_survivors_properly(.)) %>% 
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

dta_scot <- proper_counts %>% 
  filter(country == "GBR_SCO") %>% 
  filter(year >= 1950) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland")

dta_rwe <- dta_we_noscot_overall %>% 
  mutate(country = "rwe") %>% 
  filter(sex != "total") %>% 
  select(country, year, age, sex, deaths, population, exposure) %>% 
  filter(year >= 1950)

dta_both <- bind_rows(dta_rwe, dta_scot)

# for each region, year, and sex, produce cumulative mortality by 
# different ages 

dta_synth <- dta_both %>% 
  mutate(
    q_x = pmap_dbl(
      .l = list(
        age = age,
        sex = sex,
        deaths = deaths, 
        exposure = exposure
      ), 
      .f = possibly(calc_qx, otherwise = NA)
    ),
    p_x = 1 - q_x
  ) %>% 
  arrange(country, year, sex, age) %>%
  group_by(country, year, sex) %>% 
  do(calculate_survivors_properly(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)


# Write out this for an appendix
#write_csv(x = dta_synth, path = "tables/period_based_lifetables_scotland_ruk.csv" )


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
  summarise(deaths = sum(deaths), population = sum(population), exposure = sum(exposure)) %>% 
  mutate(country = "rwe") %>% 
  select(country, year, age, sex, deaths, population, exposure)

dta_uk <- dta_uk_overall %>% 
  mutate(country = "uk") %>% 
  filter(sex != "total") %>% 
  select(country, year, age, sex, deaths, population, exposure) %>% 
  filter(year >= 1950)

dta_both <- bind_rows(dta_rwe, dta_uk)


dta_synth <- dta_both %>% 
  mutate(
    q_x = pmap_dbl(
      .l = list(
        age = age,
        sex = sex,
        deaths = deaths, 
        exposure = exposure
      ), 
      .f = possibly(calc_qx, otherwise = NA)
    ),
    p_x = 1 - q_x
  ) %>% 
  arrange(country, year, sex, age) %>%
  group_by(country, year, sex) %>% 
  do(calculate_survivors_properly(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)


# Write out this for an appendix
#write_csv(x = dta_synth, path = "tables/period_based_lifetables_scotland_ruk.csv" )


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

dta_scot <- proper_counts %>%
  mutate(birth_cohort = year - age) %>% 
  filter(country == "GBR_SCO") %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland") %>% 
  select(country, birth_cohort, age, sex, deaths, population, exposure)


dta_ruk <- dta_uk_noscot_overall %>%
  mutate(birth_cohort = year - age) %>% 
  mutate(country = "ruk") %>% 
  filter(sex != "total") %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  select(country, birth_cohort, age, sex, deaths, population, exposure) 

dta_both <- bind_rows(dta_ruk, dta_scot)

dta_synth <- dta_both %>% 
  mutate(
    q_x = pmap_dbl(
      .l = list(
        age = age,
        sex = sex,
        deaths = deaths, 
        exposure = exposure
      ), 
      .f = possibly(calc_qx, otherwise = NA)
    ),
    p_x = 1 - q_x
  ) %>% 
  arrange(country, birth_cohort, sex, age) %>%
  group_by(country, birth_cohort, sex) %>% 
  do(calculate_survivors_properly(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)


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

dta_scot <- proper_counts %>%
  mutate(birth_cohort = year - age) %>% 
  filter(country == "GBR_SCO") %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>%
  filter(sex != "total") %>% 
  mutate(country = "scotland") %>% 
  select(country, birth_cohort, age, sex, deaths, population, exposure)


dta_rwe <- dta_we_noscot_overall %>% 
  mutate(birth_cohort = year - age) %>% 
  mutate(country = "rwe") %>% 
  filter(sex != "total") %>%   
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  select(country, birth_cohort, age, sex, deaths, population, exposure)  


dta_both <- bind_rows(dta_rwe, dta_scot)

dta_synth <- dta_both %>% 
  mutate(
    q_x = pmap_dbl(
      .l = list(
        age = age,
        sex = sex,
        deaths = deaths, 
        exposure = exposure
      ), 
      .f = possibly(calc_qx, otherwise = NA)
    ),
    p_x = 1 - q_x
  ) %>% 
  arrange(country, birth_cohort, sex, age) %>%
  group_by(country, birth_cohort, sex) %>% 
  do(calculate_survivors_properly(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)


dta_synth %>% 
  select(country, birth_cohort, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = scotland - rwe) %>% 
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
  spread(age, total)  -> cohort_excess_deaths_scot_rwe



# Now to compare UK with rest of Western Europe

dta_rwe <- dta_we %>% 
  filter(!(country %in% c("GRBCENW", "GBR_NIR", "GBR_SCO"))) %>% 
  mutate(birth_cohort = year - age) %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  filter(sex != "total") %>% 
  group_by(birth_cohort, age, sex) %>% 
  summarise(deaths = sum(deaths), population = sum(population), exposure = sum(exposure)) %>% 
  mutate(country = "rwe") %>% 
  select(country, birth_cohort, age, sex, deaths, population, exposure)


dta_uk <- dta_uk_overall %>% 
  mutate(country = "uk") %>% 
  mutate(birth_cohort = year - age) %>% 
  filter(birth_cohort >= 1930 & birth_cohort < 1980) %>% 
  filter(sex != "total") %>% 
  select(country, birth_cohort, age, sex, deaths, population, exposure)  

dta_both <- bind_rows(dta_rwe, dta_uk)


dta_synth <- dta_both %>% 
  mutate(
    q_x = pmap_dbl(
      .l = list(
        age = age,
        sex = sex,
        deaths = deaths, 
        exposure = exposure
      ), 
      .f = possibly(calc_qx, otherwise = NA)
    ),
    p_x = 1 - q_x
  ) %>% 
  arrange(country, birth_cohort, sex, age) %>%
  group_by(country, birth_cohort, sex) %>% 
  do(calculate_survivors_properly(.)) %>% 
  mutate(cumulative_deaths = init_cohort_size - cohort_size)


dta_synth %>% 
  select(country, birth_cohort, age, sex, cumulative_deaths) %>% 
  spread(country, cumulative_deaths) %>% 
  mutate(excess_deaths = uk - rwe) %>% 
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
             file = "tables/proper_method_scotland_excess_deaths.xlsx"
)

