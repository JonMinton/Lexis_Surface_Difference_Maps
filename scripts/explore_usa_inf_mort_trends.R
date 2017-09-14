
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
  ggplot(aes(x = year, y = mr, colour = country_code)) + 
  geom_line() + 
  scale_y_log10(breaks = c(0.1, 0.05, 0.02, 0.01, 0.005, 0.001))

dta_pop %>% 
  filter(country_code %in% c("CAN", "USA")) %>% 
  filter(age == 0) %>% 
  group_by(year, country_code) %>% 
  summarise(deaths = sum(deaths), exposure = sum(exposure)) %>%
  ungroup() %>% 
  mutate(mr = deaths / exposure) %>% 
  mutate(lmr = log(mr, 10)) -> dta_sub

dta_pop %>% 
  filter(age == 0) %>% 
  group_by(year, country_code) %>% 
  summarise(deaths = sum(deaths), exposure = sum(exposure)) %>%
  ungroup() %>% 
  mutate(mr = deaths / exposure) %>% 
  mutate(lmr = log(mr, 10)) %>% 
  ggplot(aes(x = year, y = mr, group = country_code)) + 
    geom_line(colour = "grey") + 
    scale_y_log10(breaks = c(0.1, 0.05, 0.02, 0.01, 0.005, 0.001)) + 
    geom_line(colour = "red", size = 1.5, data = . %>% filter(country_code == "USA"))

dta_pop %>% 
  filter(age == 0) %>% 
  group_by(year, country_code) %>% 
  summarise(deaths = sum(deaths), exposure = sum(exposure)) %>%
  ungroup() %>% 
  mutate(mr = deaths / exposure) %>% 
  mutate(lmr = log(mr, 10)) %>% 
  filter(year >= 1900) %>% 
  ggplot(aes(x = year, y = mr, group = country_code)) + 
  geom_line(colour = "grey", alpha = 0.5) + 
  scale_y_log10(breaks = c(0.1, 0.05, 0.02, 0.01, 0.005, 0.001)) + 
  geom_line(colour = "red", size = 1.5, data = . %>% filter(country_code == "USA")) + 
#  geom_vline(xintercept = 1996, linetype = "dashed") + 
  labs(
    y = "Infant Mortality Rate", 
    x = "Year", 
    title = "Infant mortality rate in the USA (Red) and other countries",
    caption = "Data source: Human Mortality Database")

ggsave("figures/infant_mort_usa.png", height = 12, width = 20, units = "cm", dpi = 300)

dta_pop %>% 
  filter(age == 0) %>% 
  group_by(year, country_code) %>% 
  summarise(deaths = sum(deaths), exposure = sum(exposure)) %>%
  ungroup() %>% 
  mutate(mr = deaths / exposure) %>% 
  mutate(lmr = log(mr, 10)) %>% 
  filter(mr <= 0.001)


  
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
