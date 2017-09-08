
# Infant mortality trends in USA and Canada

rm(list = ls())

pacman::p_load(
  tidyverse,
  stringr,
  scales
)


dta_pop <- read_csv("data/tidy/new_counts.csv")


dta_pop %>% 
  filter(country_code %in% c("CAN", "USA")) %>% 
  filter(age == 0) %>% 
  group_by(year, country_code) %>% 
  summarise(deaths = sum(deaths), exposure = sum(exposure)) %>%
  ungroup() %>% 
  mutate(mr = deaths / exposure) %>% 
  mutate(lmr = log(mr, 10)) -> dta_sub

dta_sub %>% 
  ggplot(aes(x = year, y = lmr, colour = country_code)) + 
  geom_line()
  
dta_sub %>% 
  filter(country_code == "USA") %>% 
  ggplot(aes(x = year, y = lmr)) + 
  geom_point() + 
  geom_vline(xintercept = 1996) +
  stat_smooth(method = "lm", se = F)

dta_sub %>% 
  filter(country_code == "USA") %>% 
  mutate(after_tanf = year > 1996) %>% 
  ggplot(aes(x = year, y = lmr)) + 
  geom_point() + 
  geom_vline(xintercept = 1996) +
  stat_smooth(method = "lm", se = F)


dta_sub %>% 
  filter(country_code == "USA") %>% 
  mutate(after_tanf = year > 1996) %>% 
  ggplot(aes(x = year, y = lmr)) + 
  geom_point() + 
  geom_vline(xintercept = 1996) +
  stat_smooth(method = "lm", se = F)


dta_sub %>% 
  filter(country_code == "CAN") %>% 
  mutate(after_tanf = year > 1996) %>% 
  ggplot(aes(x = year, y = lmr)) + 
  geom_point() + 
  geom_vline(xintercept = 1996) +
  stat_smooth(method = "lm", se = F)
