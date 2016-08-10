# Quick exploration of whether derivative of change in mortality with time is approximately linear 


rm(list = ls())

# Quick script which shows how mortality contour hurdles have changed
# by cohort year. 

# England & Wales?

pacman::p_load(
  readr, tidyr, stringr, dplyr,
  car,
  lattice, latticeExtra, 
  ggplot2, RColorBrewer,
  grid,
  fields, spatstat
)

dta <- read_csv("data/tidy/counts.csv")

this_dta <- dta %>% 
  filter(country == "GBRTENW" & sex !="total") %>% 
  mutate(birth_year = year - age) %>% 
  filter(birth_year >= 1850 & age >= 40 & age <=90) %>% 
  arrange(sex, birth_year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, birth_year, age, lg_cmr)



# png(filename="figures/shifting_hurdles/shifting_hurdles_spectral.png", 
#     width=30, height=20, res=300, units="cm"
# )


contourplot(
  lg_cmr ~ birth_year * age | sex, 
  data=this_dta, 
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab=list(label="Birth year", cex=1.4),
  xlab=list(label="Age in years", cex=1.4),
  par.settings=list(strip.background=list(col="lightgrey")),
  scales=list(
    y=list(cex=1.2, at = seq(40, 90, by = 10)), 
    x=list(cex=1.2, at = seq(1850, 1960, by = 10), rot = 90),
    alternating=T
  ),
  col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
  cuts = 20
)

# dev.off()


dta %>% 
  filter(country == "GBRTENW" & sex !="total") %>% 
  filter(year >= 1900 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, year, age, lg_cmr) %>% 
  contourplot(
    lg_cmr ~ year * age | sex, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(40, 90, by = 10)), 
      x=list(cex=1.2, at = seq(1900, 2010, by = 10), rot = 90),
      alternating=T
    ),
    aspect = "iso",
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    cuts = 20
  )


# Gradient wrt period 

dta %>% 
  filter(country == "GBRTENW" & sex !="total") %>% 
  filter(year >= 1900 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, year, age, lg_cmr) %>% 
  group_by(sex, age) %>% 
  arrange(year) %>% 
  mutate(lagger = lag(lg_cmr)) %>% 
  mutate(change_lg_cmr = lg_cmr - lagger) %>%
  mutate(
    change_lg_cmr = ifelse(change_lg_cmr > 0.1, 0.1, change_lg_cmr),
    change_lg_cmr = ifelse(change_lg_cmr < -0.1, -0.1, change_lg_cmr)
  ) %>%      
  ungroup() %>% 
  levelplot(
    change_lg_cmr ~ year * age | sex, 
    data=., 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Year", cex=1.4),
    xlab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(40, 90, by = 10)), 
      x=list(cex=1.2, at = seq(1900, 2010, by = 10), rot = 90),
      alternating=T
    ),
    aspect = "iso",
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    at = seq(-0.1, 0.1, by = 0.025)
  )


#ggplot2


dta %>% 
  filter(country == "GBRTENW" & sex !="total") %>% 
  filter(year >= 1899 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, year, age, lg_cmr) %>% 
  group_by(sex, age) %>% 
  arrange(year) %>% 
  mutate(lagger = lag(lg_cmr)) %>% 
  mutate(change_lg_cmr = lg_cmr - lagger) %>%
  ungroup() %>% 
  select(sex, year, age, change_lg_cmr) %>% 
  filter(!is.na(change_lg_cmr)) %>% 
  group_by(sex, year) %>% 
  summarise(
    mean_change = mean(change_lg_cmr),
    rms_change = (mean(change_lg_cmr ^ 2))^0.5
    ) %>% 
  ggplot(., aes(x = year, y = mean_change)) + 
  geom_point() + stat_smooth( se = F, linetype = "dashed", colour = "black") + 
  facet_wrap( ~ sex) + 
  theme_minimal() + 
  geom_hline(aes(yintercept = 0)) + 
  labs(x = "Year", y = "Mean change in age-specific log mort", title = "Mean log mort change")



dta %>% 
  filter(country == "GBRTENW" & sex !="total") %>% 
  filter(year >= 1899 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, year, age, lg_cmr) %>% 
  group_by(sex, age) %>% 
  arrange(year) %>% 
  mutate(lagger = lag(lg_cmr)) %>% 
  mutate(change_lg_cmr = lg_cmr - lagger) %>%
  ungroup() %>% 
  select(sex, year, age, change_lg_cmr) %>% 
  filter(!is.na(change_lg_cmr)) %>% 
  group_by(sex, year) %>% 
  summarise(
    mean_change = mean(change_lg_cmr),
    rms_change = (mean(change_lg_cmr ^ 2))^0.5
  ) %>% 
  ggplot(., aes(x = year, y = rms_change)) + 
  geom_point() + stat_smooth( se = F, linetype = "dashed", colour = "black") + 
  facet_wrap( ~ sex) + 
  theme_minimal() + 
  geom_hline(aes(yintercept = 0)) + 
  labs(x = "Year", y = "RMS of change in age-specific log mort", title = "RMS of log mort change")






dta %>% 
  filter(country == "GBRTENW" & sex !="total") %>% 
  filter(year >= 1899 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, year, age, lg_cmr) %>% 
  group_by(sex, age) %>% 
  arrange(year) %>% 
  mutate(lagger = lag(lg_cmr)) %>% 
  mutate(change_lg_cmr = lg_cmr - lagger) %>%
  ungroup() %>% 
  select(sex, year, age, change_lg_cmr) %>% 
  filter(!is.na(change_lg_cmr)) %>% 
  group_by(sex, year) %>% 
  summarise(
    mean_change = mean(change_lg_cmr),
    rms_change = (mean(change_lg_cmr ^ 2))^0.5
  ) %>% 
  ggplot(., aes(x = year, y = mean_change)) + 
  geom_point() + stat_smooth( se = T, linetype = "dashed", colour = "darkgreen", method = "lm") + 
  stat_smooth( se = F, linetype = "solid", colour = "blue") + 
  facet_wrap( ~ sex) + 
  theme_minimal() + 
  geom_hline(aes(yintercept = 0)) + 
  labs(x = "Year", y = "Mean change in age-specific log mort", title = "Mean log mort change")




dta %>% 
  filter(country == "GBRTENW" & sex !="total") %>% 
  filter(year >= 1899 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, year, age, lg_cmr) %>% 
  group_by(sex, age) %>% 
  arrange(year) %>% 
  mutate(lagger = lag(lg_cmr)) %>% 
  mutate(change_lg_cmr = lg_cmr - lagger) %>%
  ungroup() %>% 
  select(sex, year, age, change_lg_cmr) %>% 
  filter(!is.na(change_lg_cmr)) %>% 
  group_by(sex, year) %>% 
  summarise(
    mean_change = mean(change_lg_cmr),
    rms_change = (mean(change_lg_cmr ^ 2))^0.5
  ) %>% 
  ggplot(., aes(x = year, y = mean_change)) + 
  geom_point() + 
  stat_smooth( se = T, linetype = "dashed", colour = "darkgreen", method = "lm", formula = "y ~ 1") + # Fixed gradient
  stat_smooth( se = F, linetype = "solid", colour = "blue") + 
  facet_wrap( ~ sex) + 
  theme_minimal() + 
  geom_hline(aes(yintercept = 0)) + 
  labs(x = "Year", y = "Mean change in age-specific log mort", title = "Mean log mort change")



# Look for cohort effect of improvement in other countries  ---------------



dta %>% 
  filter(country == "FRATNP" & sex !="total") %>% 
  filter(year >= 1900 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, year, age, lg_cmr) %>% 
  group_by(sex, age) %>% 
  arrange(year) %>% 
  mutate(lagger = lag(lg_cmr)) %>% 
  mutate(change_lg_cmr = lg_cmr - lagger) %>%
  mutate(
    change_lg_cmr = ifelse(change_lg_cmr > 0.1, 0.1, change_lg_cmr),
    change_lg_cmr = ifelse(change_lg_cmr < -0.1, -0.1, change_lg_cmr)
  ) %>%      
  ungroup() %>% 
  levelplot(
    change_lg_cmr ~ year * age | sex, 
    data=., 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Year", cex=1.4),
    xlab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(40, 90, by = 10)), 
      x=list(cex=1.2, at = seq(1900, 2010, by = 10), rot = 90),
      alternating=T
    ),
    aspect = "iso",
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    at = seq(-0.1, 0.1, by = 0.025)
  )



dta %>% 
  filter(country == "NOR" & sex !="total") %>% 
  filter(year >= 1900 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, year, age, lg_cmr) %>% 
  group_by(sex, age) %>% 
  arrange(year) %>% 
  mutate(lagger = lag(lg_cmr)) %>% 
  mutate(change_lg_cmr = lg_cmr - lagger) %>%
  mutate(
    change_lg_cmr = ifelse(change_lg_cmr > 0.1, 0.1, change_lg_cmr),
    change_lg_cmr = ifelse(change_lg_cmr < -0.1, -0.1, change_lg_cmr)
  ) %>%      
  ungroup() %>% 
  levelplot(
    change_lg_cmr ~ year * age | sex, 
    data=., 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Year", cex=1.4),
    xlab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(40, 90, by = 10)), 
      x=list(cex=1.2, at = seq(1900, 2010, by = 10), rot = 90),
      alternating=T
    ),
    aspect = "iso",
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    at = seq(-0.1, 0.1, by = 0.025)
  )


dta %>% 
  filter(country == "ESP" & sex !="total") %>% 
  filter(year >= 1900 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, year, age, lg_cmr) %>% 
  group_by(sex, age) %>% 
  arrange(year) %>% 
  mutate(lagger = lag(lg_cmr)) %>% 
  mutate(change_lg_cmr = lg_cmr - lagger) %>%
  mutate(
    change_lg_cmr = ifelse(change_lg_cmr > 0.1, 0.1, change_lg_cmr),
    change_lg_cmr = ifelse(change_lg_cmr < -0.1, -0.1, change_lg_cmr)
  ) %>%      
  ungroup() %>% 
  levelplot(
    change_lg_cmr ~ year * age | sex, 
    data=., 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Year", cex=1.4),
    xlab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(40, 90, by = 10)), 
      x=list(cex=1.2, at = seq(1900, 2010, by = 10), rot = 90),
      alternating=T
    ),
    aspect = "iso",
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    at = seq(-0.1, 0.1, by = 0.025)
  )


dta %>% 
  filter(country == "FIN" & sex !="total") %>% 
  filter(year >= 1900 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, year, age, lg_cmr) %>% 
  group_by(sex, age) %>% 
  arrange(year) %>% 
  mutate(lagger = lag(lg_cmr)) %>% 
  mutate(change_lg_cmr = lg_cmr - lagger) %>%
  mutate(
    change_lg_cmr = ifelse(change_lg_cmr > 0.1, 0.1, change_lg_cmr),
    change_lg_cmr = ifelse(change_lg_cmr < -0.1, -0.1, change_lg_cmr)
  ) %>%      
  ungroup() %>% 
  levelplot(
    change_lg_cmr ~ year * age | sex, 
    data=., 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Year", cex=1.4),
    xlab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(40, 90, by = 10)), 
      x=list(cex=1.2, at = seq(1900, 2010, by = 10), rot = 90),
      alternating=T
    ),
    aspect = "iso",
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    at = seq(-0.1, 0.1, by = 0.025)
  )

dta %>% 
  filter(country == "AUS" & sex !="total") %>% 
  filter(year >= 1900 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, year, age, lg_cmr) %>% 
  group_by(sex, age) %>% 
  arrange(year) %>% 
  mutate(lagger = lag(lg_cmr)) %>% 
  mutate(change_lg_cmr = lg_cmr - lagger) %>%
  mutate(
    change_lg_cmr = ifelse(change_lg_cmr > 0.1, 0.1, change_lg_cmr),
    change_lg_cmr = ifelse(change_lg_cmr < -0.1, -0.1, change_lg_cmr)
  ) %>%      
  ungroup() %>% 
  levelplot(
    change_lg_cmr ~ year * age | sex, 
    data=., 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Year", cex=1.4),
    xlab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(40, 90, by = 10)), 
      x=list(cex=1.2, at = seq(1900, 2010, by = 10), rot = 90),
      alternating=T
    ),
    aspect = "iso",
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    at = seq(-0.1, 0.1, by = 0.025)
  )


dta %>% 
  filter(country %in% c("DEUTE", "DEUTW", "GBRCENW", "GBR_SCO", "FRACNP", "USA", "CAN") & sex !="total") %>% 
  filter(year >= 1900 & age >= 40 & age <=90) %>% 
  arrange(sex, year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(country, sex, year, age, lg_cmr) %>% 
  group_by(country, sex, age) %>% 
  arrange(year) %>% 
  mutate(lagger = lag(lg_cmr)) %>% 
  mutate(change_lg_cmr = lg_cmr - lagger) %>%
  mutate(
    change_lg_cmr = ifelse(change_lg_cmr > 0.1, 0.1, change_lg_cmr),
    change_lg_cmr = ifelse(change_lg_cmr < -0.1, -0.1, change_lg_cmr)
  ) %>%      
  ungroup() %>% 
  levelplot(
    change_lg_cmr ~ year * age | sex + country, 
    data=., 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Year", cex=1.4),
    xlab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(40, 90, by = 10)), 
      x=list(cex=1.2, at = seq(1900, 2010, by = 10), rot = 90),
      alternating=T
    ),
    aspect = "iso",
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    at = seq(-0.1, 0.1, by = 0.025)
  )



  