rm(list = ls())

pacman::p_load(
  tidyverse,
  stringr,
  scales
)


dta_pop <- read_csv("data/tidy/new_counts.csv")

places_in_europe <- c(
  "Austria", "Belgium", "Switzerland", "Germany (East & West Combined)",
  "Denmark", "Spain", "Finland", "France (Total)", 
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


categorise_to_age_groups <- function(data, region_name){
  region_name <- enquo(region_name)
  
  data %>% 
    filter(year >= 1950, year <= 2010) %>% 
    filter(year != 1981) %>% 
    filter(year != 1975) %>% 
    filter(year != 1954) %>% 
    mutate(age_type = case_when(
      age == 0 ~ "babies",
      age > 0 & age < 18 ~ "children",
      age >= 18 & age <= 25 ~ "young adults",
      age >= 26 & age < 60 ~ "working age",
      age >= 60  & age <= 80 ~ "retired",
      age > 80 ~ "elderly"
    )) %>% 
    mutate(age_type = factor(
      age_type, 
      levels = c("babies", "children", "young adults", 
                 "working age", "retired", "elderly")
      )
    ) %>% 
    group_by(year) %>% 
    mutate(total_population = sum(population, na.rm = T)) %>% 
    group_by(year, age_type) %>% 
    summarise(
      total_population = total_population[1],
      pop_by_group = sum(population, na.rm = T)) %>% 
    mutate(proportion = pop_by_group / total_population) %>% 
    mutate(region = !!region_name) %>% 
    select(year, region, age_type, proportion)  -> output
  
  output
}

age_grp_we <- categorise_to_age_groups(dta_pop_ss, "Western Europe")
age_grp_uk <- categorise_to_age_groups(dta_pop %>% filter(country_code == "GBRTENW"), "England & Wales")
age_grp_scot <- categorise_to_age_groups(dta_pop %>% filter(country_code == "GBR_SCO"), "Scotland")

age_grp_joined <- bind_rows(age_grp_we, age_grp_uk, age_grp_scot) %>% 
  mutate(region = factor(region, levels = c("Western Europe", "England & Wales", "Scotland"), ordered = T))

age_grp_joined %>% 
  ggplot(., aes(x = year, y = proportion, size = region, colour = region, linetype = region)) + 
  geom_line() + 
  theme_minimal() + 
  facet_wrap(~age_type, scale = "free_y") + 
  scale_colour_manual(values = c("grey", "black", "blue")) + 
  scale_linetype_manual(values = c("longdash", "twodash", "solid")) + 
  scale_size_manual(values = c(1.2, 1, 1))

ggsave("figures/nankai_lecture/population_proportion_types.png",
       width = 25, height = 20, units = "cm", dpi = 300)


# Now to present dependency ratios 

calculate_dependency_ratio <- function(data, region_name, male_dep_age = 65, female_dep_age = 60){
  region_name <- enquo(region_name)
  male_dep_age <- enquo(male_dep_age)
  female_dep_age <- enquo(female_dep_age)
  data %>% 
    filter(year >= 1950, year <= 2010) %>% 
    filter(year != 1981) %>% 
    filter(year != 1975) %>% 
    filter(year != 1954) %>% 
    mutate(dep_status = case_when(
      age < 18  ~ "young", 
      sex == "female" & age >= !!female_dep_age ~ "female_older",
      sex == "male" & age >= !!male_dep_age ~ "male_older",
      TRUE ~ "independent"
      )
    ) %>% 
    group_by(year, dep_status) %>% 
    summarise(total_population = sum(population, na.rm = T)) %>% 
    group_by(year) %>% 
    mutate(young_dep = total_population[dep_status == "young"] / total_population[dep_status == "independent"]) %>% 
    mutate(old_dep = sum(total_population[dep_status %in% c("female_older", "male_older")]) / total_population[dep_status == "independent"]) %>% 
    mutate(dep_ratio = sum(total_population[!(dep_status == "independent")]) / total_population[dep_status == "independent"]) %>% 
    mutate(region = !!region_name) %>% 
    select(year, region, young_dep, old_dep, dep_ratio) %>% 
    distinct() -> output
  
  output
}

dep_we <- calculate_dependency_ratio(dta_pop_ss, "Western Europe")
dep_uk <- calculate_dependency_ratio(dta_pop %>% filter(country_code == "GBRTENW"), "England & Wales")
dep_scot <- calculate_dependency_ratio(dta_pop %>% filter(country_code == "GBR_SCO"), "Scotland")

dep_joined <- bind_rows(dep_we, dep_uk, dep_scot) %>% 
  mutate(region = factor(region, levels = c("Western Europe", "England & Wales", "Scotland"), ordered = T)) %>% 
  ungroup() %>% 
  gather(key = "dep_type", value = "ratio", young_dep:dep_ratio) %>% 
  mutate(dep_type = recode_factor(dep_type, young_dep = "Youth", old_dep = "Elderly", dep_ratio = "Overall")) %>% 
  mutate(ratio = 1000 * ratio)

dep_joined %>% 
  ggplot(., aes(x = year, y = ratio, size = region, colour = region, linetype = region)) + 
  geom_line() + 
  theme_minimal() + 
  facet_wrap(~dep_type, scale = "free_y") +
  scale_colour_manual(values = c("grey", "black", "blue")) + 
  scale_linetype_manual(values = c("longdash", "twodash", "solid")) + 
  scale_size_manual(values = c(1.2, 1, 1))

ggsave("figures/nankai_lecture/dependency_per_1000.png",
       width = 25, height = 20, units = "cm", dpi = 300)

# Different retirement ages 

dep_ratio_w_diff_retirement <- function(ret_age){

  male_dep_age <- enquo(ret_age)
  female_dep_age <- enquo(ret_age)
  
  dep_we <- calculate_dependency_ratio(dta_pop_ss, "Western Europe", male_dep_age = !!male_dep_age, female_dep_age = !!female_dep_age)
  dep_uk <- calculate_dependency_ratio(dta_pop %>% filter(country_code == "GBRTENW"), "England & Wales",
                                       male_dep_age = !!male_dep_age, female_dep_age = !!female_dep_age)
  dep_scot <- calculate_dependency_ratio(dta_pop %>% filter(country_code == "GBR_SCO"), "Scotland", 
                                         male_dep_age = !!male_dep_age, female_dep_age = !!female_dep_age)
  
  
  output <- bind_rows(dep_we, dep_uk, dep_scot) %>% 
    mutate(region = factor(region, levels = c("Western Europe", "England & Wales", "Scotland"), ordered = T)) %>% 
    mutate(retire_at = !!ret_age)
  
  output
}


dep_at_various <- data_frame(ret_age = c(65, 67, 70, 75)) %>% 
  mutate(dep_data = map(ret_age, .f = dep_ratio_w_diff_retirement)) %>% 
  unnest()

dep_at_various %>% 
  ggplot(., aes(x = year, y = old_dep, group = region, colour = region, linetype = region)) + 
  geom_line() + 
  theme_minimal() + 
  scale_colour_manual(values = c("grey", "black", "blue")) + 
  scale_linetype_manual(values = c("longdash", "twodash", "solid")) + 
  scale_size_manual(values = c(1.2, 1, 1)) +
  facet_wrap(~retire_at) + 
  labs(y = "Old Age Dependency Ratio")

ggsave("figures/nankai_lecture/old_age_dependency_by_retirement_age.png",
       width = 25, height = 20, units = "cm", dpi = 300)


