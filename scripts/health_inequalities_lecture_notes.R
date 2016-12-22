rm(list = ls())


pacman::p_load(
  tidyverse
)

dta <- read_csv("data/tidy/new_counts.csv")


dta %>% 
  filter(country_code == "GBR_SCO") %>% 
  filter(age == 0) %>% 
  mutate(infant_death_rate = 1000 * deaths/ exposure) %>% 
  ggplot(., aes(x = year, y = infant_death_rate)) + 
  geom_line() + 
  facet_wrap(~sex) + 
  labs(x = "Year", y = "Infant deaths per 1000 births", title = "Infant death rates in Scotland")

dta %>% 
  filter(country_code == "GBR_SCO") %>% 
  filter(age == 0) %>% 
  mutate(infant_death_rate = 1000 * deaths/ exposure) %>% 
  ggplot(., aes(x = year, y = infant_death_rate)) + 
  geom_line() + 
  facet_wrap(~sex) + 
  labs(x = "Year", y = "Infant deaths per 1000 births", title = "Infant death rates in Scotland") + 
  scale_y_log10(breaks = c(1, 5, 10, 20, 50, 100, 200, 500, 1000), 
                labels = c(1, 5, 10, 20, 50, 100, 200, 500, 1000))

# Mortality at age 15, 30, 45, 60 and 75

dta %>% 
  filter(country_code == "GBR_SCO") %>% 
  filter(age %in% c(15, 30, 45, 60, 70, 80)) %>% 
  mutate(infant_death_rate = 1000 * deaths/ exposure) %>% 
  ggplot(., aes(x = year, y = infant_death_rate)) + 
  geom_line() + 
  facet_grid(sex ~ age) + 
  labs(x = "Year", y = "Deaths per thousand", title = "Death rate trends at specific ages") + 
  scale_y_log10(breaks = c(1, 5, 10, 20, 50, 100, 200, 500, 1000), 
                labels = c(1, 5, 10, 20, 50, 100, 200, 500, 1000))

dta %>% 
  filter(country_code == "GBR_SCO") %>% 
  filter(age %in% c(15, 30, 45, 60, 70, 80)) %>% 
  mutate(infant_death_rate = 1000 * deaths/ exposure) %>% 
  ggplot(., aes(x = year, y = infant_death_rate)) + 
  geom_line() + 
  facet_grid(sex ~ age) + 
  labs(x = "Year", y = "Deaths per thousand", title = "Death rate trends at specific ages") 



# Mortality risk in 2010 by age for males and females

dta %>% 
  filter(country_code == "GBR_SCO") %>%
  filter(year == 2010) %>% 
  filter(age <= 90) %>% 
  mutate(death_rate = 1000 * deaths / exposure) %>% 
  ggplot(. , aes(x = age, y = death_rate, group = sex, linetype = sex)) +
  geom_line() + 
  labs(x = "Age in years", y = "Deaths per thousand", title = "Deaths by age and sex in Scotland, 2010")

dta %>% 
  filter(country_code == "GBR_SCO") %>%
  filter(year == 2010) %>% 
  filter(age <= 90) %>% 
  mutate(death_rate = 1000 * deaths / exposure) %>% 
  ggplot(. , aes(x = age, y = death_rate, group = sex, linetype = sex)) +
  geom_line() + 
  labs(x = "Age in years", y = "Deaths per thousand", title = "Deaths by age and sex in Scotland, 2010") + 
  scale_y_log10(breaks = c(1, 5, 10, 20, 50, 100, 200, 500, 1000), 
                labels = c(1, 5, 10, 20, 50, 100, 200, 500, 1000))


# Difference shown 
dta %>% 
  filter(country_code == "GBR_SCO") %>%
  filter(year == 2010) %>% 
  filter(age >= 20) %>% 
  filter(age <= 90) %>% 
  mutate(death_rate = 1000 * deaths / exposure) %>%
  select(age, sex, death_rate) %>% 
  spread(sex, death_rate) %>%
  mutate(abs_difference = male - female, rel_difference = male / female) %>%
  ggplot(. , aes(x = age)) +
  geom_ribbon(
    aes(
      ymin = ifelse(female < male, female, NA), ymax = ifelse(female < male, male, NA)),
    fill = "blue") + 
  labs(x = "Age in years", y = "Deaths per thousand", title = "Male excess deaths per thousand, from age 20, in Scotland, 2010") + 
  guides(fill = "none")  


dta %>% 
  filter(country_code == "GBR_SCO") %>%
  filter(year == 2010) %>% 
  filter(age >= 20) %>% 
  filter(age <= 90) %>% 
  mutate(death_rate = 1000 * deaths / exposure) %>%
  select(age, sex, death_rate) %>% 
  spread(sex, death_rate) %>%
  mutate(abs_difference = male - female, rel_difference = male / female) %>%
  ggplot(. , aes(x = age)) +
  geom_ribbon(
    aes(
      ymin = ifelse(female < male, female, NA), ymax = ifelse(female < male, male, NA)),
    fill = "blue") + 
  labs(x = "Age in years", y = "Deaths per thousand", title = "Male excess deaths per thousand, from age 20, in Scotland, 2010") + 
  guides(fill = "none") + 
  scale_y_log10(breaks = c(1, 5, 10, 20, 50, 100, 200, 500, 1000), 
                labels = c(1, 5, 10, 20, 50, 100, 200, 500, 1000))



# Size of the risk by different ages 

dta %>% 
  filter(country_code == "GBR_SCO") %>%
  filter(year == 2010) %>% 
  filter(age <= 90) %>% 
  mutate(death_rate = 1000 * deaths / exposure) %>%
  select(age, sex, death_rate) %>% 
  spread(sex, death_rate) %>%
  mutate(abs_difference = male - female, rel_difference = male / female) %>%
  ggplot(. , aes(x = age, y = abs_difference)) +
  geom_line() + 
  labs(x = "Age in years", y = "Excess Deaths per thousand", title = "Male excess deaths per thousand in Scotland, 2010") 

  
dta %>% 
  filter(country_code == "GBR_SCO") %>%
  filter(year == 2010) %>% 
  filter(age <= 90) %>% 
  mutate(death_rate = 1000 * deaths / exposure) %>%
  select(age, sex, death_rate) %>% 
  spread(sex, death_rate) %>%
  mutate(abs_difference = male - female, rel_difference = male / female) %>%
  ggplot(. , aes(x = age, y = abs_difference)) +
  geom_line() + 
  labs(x = "Age in years", y = "Excess Deaths per thousand", title = "Male excess deaths per thousand, in Scotland, 2010") + 
  scale_y_log10(breaks = c(1, 5, 10, 20, 50, 100, 200, 500, 1000), 
                labels = c(1, 5, 10, 20, 50, 100, 200, 500, 1000))

dta %>% 
  filter(country_code == "GBR_SCO") %>%
  filter(year == 2010) %>% 
  filter(age <= 90) %>% 
  mutate(death_rate = 1000 * deaths / exposure) %>%
  select(age, sex, death_rate) %>% 
  spread(sex, death_rate) %>%
  mutate(abs_difference = male - female, rel_difference = male / female) %>%
  ggplot(. , aes(x = age, y = rel_difference)) +
  geom_line() + 
  labs(x = "Age in years", y = "Ratio of male/female deaths", title = "Sex mortality ratio in Scotland, 2010") 



dta %>% 
  filter(country_code == "GBR_SCO") %>%
  filter(year == 2010) %>% 
  filter(age <= 90) %>% 
  mutate(death_rate = 1000 * deaths / exposure) %>%
  select(age, sex, death_rate) %>% 
  spread(sex, death_rate) %>%
  mutate(abs_difference = male - female, rel_difference = male / female) %>%
  ggplot(. , aes(x = age, y = rel_difference)) +
  geom_point() + stat_smooth(span = 0.1) + 
  labs(x = "Age in years", y = "Ratio of male/female deaths", title = "Sex mortality ratio in Scotland, 2010") 

