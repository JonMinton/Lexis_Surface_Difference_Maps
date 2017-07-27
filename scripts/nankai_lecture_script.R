rm(list = ls())

pacman::p_load(
  tidyverse,
  stringr,
  scales
)


dta_pop <- read_csv("data/tidy/new_counts.csv")

places_in_europe <- c(
  "Austria", "Belgium", "Switzerland", "Germany (East & West Combined)",
  "Denmark", "Spain", "Finland", "France (Total)", "United Kingdom", 
  "Ireland", "Iceland", "Italy", "Luxembourg", "Netherlands", "Norway", 
  "Sweden"
)

places <- read_csv(
  "data/tidy/country_codes__new.csv", 
  col_names = c("country_code", "full_name", "in_europe") ,
  skip = 1
) %>% 
  mutate(
    full_name = str_replace_all(full_name, "\"", "")
  ) %>% 
  filter(full_name %in% places_in_europe) 

dta_pop_ss <- dta_pop %>% 
  right_join(places) %>% 
  select(country_code, full_name, year, age, sex, deaths, population, exposure) 

# Population structure by decade for Western Europe

dta_pop_ss %>% 
  filter(year %in% seq(1960, 2010, by = 10)) %>% 
  group_by(year, age, sex) %>% 
  summarise(population = sum(population)) %>% 
  ggplot(., aes(x = age, y = population, fill = sex)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~year) + 
  theme_minimal() + 
  annotate("rect", xmin=0, xmax=18, ymin=0, ymax=Inf, alpha=0.2) +
  annotate("rect", xmin=60, xmax=100, ymin=0, ymax=Inf, alpha=0.2) +
  scale_y_continuous(labels=comma) 

ggsave("figures/nankai_lecture/western_europe_population_structure.png",
       width = 25, height = 20, units = "cm", dpi = 300)


# Population change over time 

dta_pop_ss %>% 
  filter(year >= 1950, year <= 2010) %>% 
  filter(year != 1981) %>% 
  filter(year != 1975) %>% 
  filter(year != 1954) %>% 
  group_by(year) %>% 
  summarise(population = sum(population, na.rm = T) / 1E6) %>% 
  ggplot(., aes(x = year, y = population)) + 
  geom_line() + 
  theme_minimal() + 
  scale_y_continuous(labels = comma, limits = c(0, 350)) + 
  labs(y = "Population (Millions)")

ggsave("figures/nankai_lecture/western_europe_population_size.png",
       width = 10, height = 10, units = "cm", dpi = 300)


# Age groups in Western Europe by year 

dta_pop_ss %>% 
  filter(year >= 1950, year <= 2010) %>% 
  filter(year != 1981) %>% 
  filter(year != 1975) %>% 
  filter(year != 1954) %>% 
  mutate(age_type = case_when(
    age == 0 ~ "babies",
    age > 0 & age < 18 ~ "children",
    age >= 18 & age < 60 ~ "working age",
    age >= 60  & age <= 80 ~ "retired",
    age > 80 ~ "elderly"
  )) %>% 
  group_by(year) %>% 
  mutate(total_population = sum(population, na.rm = T)) %>% 
  group_by(year, age_type) %>% 
  summarise(
    total_population = total_population[1],
    pop_by_group = sum(population, na.rm = T)) %>% 
  mutate(proportion = pop_by_group / total_population) %>% 
  mutate(region = "Western Europe") %>% 
  select(year, region, age_type, proportion) -> props_we

dta_pop_ss %>% 
  filter(full_name == "United Kingdom") %>% 
  filter(year >= 1950, year <= 2010) %>% 
  filter(year != 1981) %>% 
  filter(year != 1975) %>% 
  filter(year != 1954) %>% 
  mutate(age_type = case_when(
    age == 0 ~ "babies",
    age > 0 & age < 18 ~ "children",
    age >= 18 & age < 60 ~ "working age",
    age >= 60  & age <= 80 ~ "retired",
    age > 80 ~ "elderly"
  )) %>% 
  group_by(year) %>% 
  mutate(total_population = sum(population, na.rm = T)) %>% 
  group_by(year, age_type) %>% 
  summarise(
    total_population = total_population[1],
    pop_by_group = sum(population, na.rm = T)) %>% 
  mutate(proportion = pop_by_group / total_population) %>% 
  mutate(region = "United Kingdom") %>% 
  select(year, region, age_type, proportion) -> props_uk

dta_pop %>% 
  filter(country_code == "GBR_SCO") %>% 
  filter(year >= 1950, year <= 2010) %>% 
  filter(year != 1981) %>% 
  filter(year != 1975) %>% 
  filter(year != 1954) %>% 
  mutate(age_type = case_when(
    age == 0 ~ "babies",
    age > 0 & age < 18 ~ "children",
    age >= 18 & age < 60 ~ "working age",
    age >= 60  & age <= 80 ~ "retired",
    age > 80 ~ "elderly"
  )) %>% 
  group_by(year) %>% 
  mutate(total_population = sum(population, na.rm = T)) %>% 
  group_by(year, age_type) %>% 
  summarise(
    total_population = total_population[1],
    pop_by_group = sum(population, na.rm = T)) %>% 
  mutate(proportion = pop_by_group / total_population) %>% 
  mutate(region = "Scotland") %>% 
  select(year, region, age_type, proportion) -> props_scot


props <- bind_rows(props_scot, props_we, props_uk)

props %>% 
  ggplot(., aes(x = year, y = proportion, size = region, colour = region, linetype = region)) + 
  geom_line() + 
  theme_minimal() + 
  facet_wrap(~age_type, scale = "free_y") + 
  scale_colour_manual(values = c("blue", "grey", "black")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dashed")) + 
  scale_size_manual(values = c(1.2, 1, 1))

ggsave("figures/nankai_lecture/population_proportion_types.png",
       width = 25, height = 20, units = "cm", dpi = 300)


ggsave("figures/nankai_lecture/western_europe_workers.png",
       width = 10, height = 10, units = "cm", dpi = 300)

dta_pop_ss %>% 
  filter(year >= 1950, year <= 2010) %>% 
  filter(year != 1981) %>% 
  filter(year != 1975) %>% 
  filter(year != 1954) %>% 
  filter(age >= 60) %>% 
  group_by(year) %>% 
  summarise(population = sum(population, na.rm = T) / 1E6) %>% 
  ggplot(., aes(x = year, y = population)) + 
  geom_line() + 
  theme_minimal() + 
  scale_y_continuous(labels = comma, limits = c(0, 200)) + 
  labs(y = "Elderly (Millions)")

ggsave("figures/nankai_lecture/western_europe_workers.png",
       width = 10, height = 10, units = "cm", dpi = 300)

