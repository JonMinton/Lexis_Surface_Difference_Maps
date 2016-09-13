# New script to re-harvest data from HMD
#12 / 7/ 2016
# Extracting population, exposure, deaths 

rm(list = ls())
require(pacman)

pacman::p_load(
  readr, tidyr, dplyr,
  purrr, stringr,
  ggplot2
)

source_dir <- "E:/Dropbox/Data/HMD/hmd_countries_12_07_2016"

country_codes <- list.files(source_dir)

grab_and_reshape <- function(CODE){
  location = paste(source_dir, CODE, "STATS", sep = "/")
  
  load_something <- function(WHAT){
    read.table(paste(location, WHAT, sep = "/"), # changed to read.table as read_table
               # assumes fixed width positions between columns based on first 
               # few observations, then fails when ages > 99 years
    skip = 2, header = T, stringsAsFactors = F
    ) %>% tbl_df()
  }
  
  this_population = load_something("Population.txt")
  this_exposure = load_something("Exposures_1x1.txt")
  this_deaths = load_something("Deaths_1x1.txt")
  
  reshape_something <- function(input){
    output <- input
    names(output) = tolower(names(output))
    output %>% 
      select(-total) %>% 
      filter(age != "110+") %>% 
      mutate(age = as.integer(age)) %>% 
      mutate(year = as.integer(year)) %>% 
      gather(key = sex, value = COUNT, -year, -age) %>% 
      mutate(COUNT = as.numeric(COUNT)) 
  }
  
  this_population %>% reshape_something() %>% rename(population = COUNT) -> this_population
  this_exposure %>% reshape_something() %>% rename(exposure = COUNT) -> this_exposure
  this_deaths %>% reshape_something() %>% rename(deaths = COUNT) -> this_deaths
  
  this_population %>% 
    full_join(this_exposure, by = c("year", "age", "sex")) %>% 
    full_join(this_deaths, by = c("year", "age", "sex")) %>% 
    mutate(country_code = CODE) %>% 
    select(country_code, year, age, sex, deaths, population, exposure) %>% 
    arrange(year, age, sex)
}


data_frame(code = country_codes) %>% 
  mutate(df = map(.x = code, .f = grab_and_reshape)) %>% 
  unnest() %>% 
  select(-code) -> all_data

write_csv(x = all_data, path = "data/tidy/new_counts.csv")



