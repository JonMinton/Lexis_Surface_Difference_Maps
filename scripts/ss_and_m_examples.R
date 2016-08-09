# Script to produce some example plots for SS&M Workshop 


rm(list = ls())


pacman::p_load(
  readr, plyr, stringr, car,
  tidyr, dplyr,
  lattice, latticeExtra, ggplot2,
  RColorBrewer, grid, 
  fields, spatstat, 
  xtable
)

# functions ---------------------------------------------------------------
source("scripts/smoother_function.R")
source("scripts/scotland_in_context__helper_functions.R")


dta <- read_csv("data/tidy/counts_germany_combined.csv")




# Examples 

# All cause mortality 

# USA, Norway, England & Wales, Scotland, Spain


# Heatmap
# Smoothed heatmap

# contour
# Smoothed levelplot
#  - w/ many cuts
#  - w/ few cuts 


# Isometric projection

# Borrow code from IJE blog 



this_dta <- dta %>% 
  filter(country %in% c("GBR_SCO", "GBRCENW", "FRACNP", "NOR") & sex !="total") %>% 
  mutate(country = recode(
    country, 
    "'GBR_SCO' = 'Scotland';
    'GBRCENW' = 'Eng/Wales';
    'NOR' = 'Norway';
    'FRACNP' = 'France';
    "
  )) %>% 
  arrange(year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(sex, country, year, age, lg_cmr)

png(filename="figures/ssm_examples/multi_country_simple_unlabelled_lines.png",
    width=50, height=150, res=300, units="cm"
)

this_dta %>% 
  filter(age <= 90) %>%
  filter(year >= 1900) %>% 
  contourplot(
    lg_cmr ~ year * age | sex + country, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    xlab=list(label="Year", cex=1.4),
    ylab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(0, 90, by = 20)), 
      x=list(cex=1.2, at = seq(1850, 2010, by = 20), rot = 90),
      alternating=3
    ),
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    cuts = 15,
    labels = F,
    aspect = "iso",
    panel = function(x, y, z, ...){
      panel.contourplot(x, y, z, ...)
      panel.abline(v = seq(1870, 2010, by= 10), lty = "dashed", col = "gray")
      panel.abline(h = seq(0, 90, by= 10), lty = "dashed", col = "gray")
      
    },
    colorkey = list(labels = list(cex = 1.3))
    
  )

dev.off()



png(filename="figures/ssm_examples/multi_country_more_contours.png",
    width=50, height=150, res=300, units="cm"
)

this_dta %>% 
  filter(age <= 90) %>%
  filter(year >= 1900) %>% 
  contourplot(
    lg_cmr ~ year * age | sex + country, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    xlab=list(label="Year", cex=1.4),
    ylab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(0, 90, by = 20)), 
      x=list(cex=1.2, at = seq(1850, 2010, by = 20), rot = 90),
      alternating=3
    ),
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    cuts = 30,
    labels = F,
    aspect = "iso",
    panel = function(x, y, z, ...){
      panel.contourplot(x, y, z, ...)
      panel.abline(v = seq(1870, 2010, by= 10), lty = "dashed", col = "gray")
      panel.abline(h = seq(0, 90, by= 10), lty = "dashed", col = "gray")
      
    },
    colorkey = list(labels = list(cex = 1.3))
    
  )

dev.off()


png(filename="figures/ssm_examples/multi_country_simple_labelled.png",
    width=50, height=150, res=300, units="cm"
)

this_dta %>% 
  filter(age <= 90) %>%
  filter(year >= 1900) %>% 
  contourplot(
    lg_cmr ~ year * age | sex + country, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    xlab=list(label="Year", cex=1.4),
    ylab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(0, 90, by = 20)), 
      x=list(cex=1.2, at = seq(1850, 2010, by = 20), rot = 90),
      alternating=3
    ),
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    cuts = 15,
    labels = T,
    aspect = "iso",
    panel = function(x, y, z, ...){
      panel.contourplot(x, y, z, ...)
      panel.abline(v = seq(1870, 2010, by= 10), lty = "dashed", col = "gray")
      panel.abline(h = seq(0, 90, by= 10), lty = "dashed", col = "gray")
      
    },
    colorkey = list(labels = list(cex = 1.3))
    
  )

dev.off()


# More labelled, but smoothed 

smooth_fn <- function(DTA, SMOOTH_PAR = 1.3){
  out <- DTA %>%   
    mutate(
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base=10)     
    ) %>% smooth_var(
      dta =.,
      group_vars= c("country", "sex"),
      smooth_var = "lg_cmr",
      smooth_par = SMOOTH_PAR
    ) 
  return(out)
}

this_dta_smoothed <- dta %>% 
  filter(country %in% c("GBR_SCO", "GBRCENW", "FRACNP", "NOR") & sex !="total") %>% 
  mutate(country = recode(
    country, 
    "'GBR_SCO' = 'Scotland';
    'GBRCENW' = 'Eng/Wales';
    'NOR' = 'Norway';
    'FRACNP' = 'France';
    "
  )) %>% 
  arrange(year, age) %>%
  smooth_fn(.) %>% 
  select(sex, country, year, age, lg_cmr)



png(filename="figures/ssm_examples/multi_country_more_contours_smoothed.png",
    width=50, height=150, res=300, units="cm"
)

this_dta_smoothed %>% 
  filter(age <= 90) %>%
  filter(year >= 1900) %>% 
  filter(year <= 2006) %>% 
  contourplot(
    lg_cmr ~ year * age | sex + country, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    xlab=list(label="Year", cex=1.4),
    ylab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(0, 90, by = 20)), 
      x=list(cex=1.2, at = seq(1850, 2010, by = 20), rot = 90),
      alternating=3
    ),
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    cuts = 30,
    labels = T,
    aspect = "iso",
    panel = function(x, y, z, ...){
      panel.contourplot(x, y, z, ...)
      panel.abline(v = seq(1870, 2010, by= 10), lty = "dashed", col = "gray")
      panel.abline(h = seq(0, 90, by= 10), lty = "dashed", col = "gray")
      
    },
    colorkey = list(labels = list(cex = 1.3))
    
  )

dev.off()


# Question: 
# Why is the last displayed year not the last year for which data are available? 
# What would happen if the smoothed values for the last few years were displayed? How might this be misleading?



# Unsmoothed levelplot


png(filename="figures/ssm_examples/multi_country_more_level.png",
    width=50, height=150, res=300, units="cm"
)

this_dta %>% 
  filter(age <= 90) %>%
  filter(year >= 1900) %>% 
  levelplot(
    lg_cmr ~ year * age | sex + country, 
    data=., 
    par.strip.text=list(cex=1.4, fontface="bold"),
    xlab=list(label="Year", cex=1.4),
    ylab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(0, 90, by = 20)), 
      x=list(cex=1.2, at = seq(1850, 2010, by = 20), rot = 90),
      alternating=3
    ),
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    cuts = 60,
    aspect = "iso",
    panel = function(x, y, z, ...){
      panel.contourplot(x, y, z, ...)
      panel.abline(v = seq(1870, 2010, by= 10), lty = "dashed", col = "gray")
      panel.abline(h = seq(0, 90, by= 10), lty = "dashed", col = "gray")
      
    },
    colorkey = list(labels = list(cex = 1.3))
    
  )

dev.off()


png(filename="figures/ssm_examples/multi_country_more_level_nogrid.png",
    width=50, height=150, res=300, units="cm"
)

this_dta %>% 
  filter(age <= 90) %>%
  filter(year >= 1900) %>% 
  levelplot(
    lg_cmr ~ year * age | sex + country, 
    data=., 
    par.strip.text=list(cex=1.4, fontface="bold"),
    xlab=list(label="Year", cex=1.4),
    ylab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(0, 90, by = 20)), 
      x=list(cex=1.2, at = seq(1850, 2010, by = 20), rot = 90),
      alternating=3
    ),
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    cuts = 60,
    aspect = "iso",
    colorkey = list(labels = list(cex = 1.3))
  )

dev.off()

# Question: 
# What do the white squares signify? What implications might they have for 
# interpretation of the plots?

png(filename="figures/ssm_examples/multi_country_more_level_nogrid_greys.png",
    width=50, height=150, res=300, units="cm"
)

this_dta %>% 
  filter(age <= 90) %>%
  filter(year >= 1900) %>% 
  levelplot(
    lg_cmr ~ year * age | sex + country, 
    data=., 
    par.strip.text=list(cex=1.4, fontface="bold"),
    xlab=list(label="Year", cex=1.4),
    ylab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      y=list(cex=1.2, at = seq(0, 90, by = 20)), 
      x=list(cex=1.2, at = seq(1850, 2010, by = 20), rot = 90),
      alternating=3
    ),
    col.regions = rev(gray(0:199/199)),
    cuts = 60,
    aspect = "iso",
    colorkey = list(labels = list(cex = 1.3))
  )

dev.off()


# Generic questions: 

# What is the age/mortality curve for the following?

# Periods: 1900, 1930, 1950, 1980, 2000
# Cohorts: 1900, 1930, 1950, 1980


# By period 

this_dta %>% 
  filter(year %in% c(1900, 1930, 1950, 1980, 2000)) %>%
  filter(age <= 90) %>% 
  mutate(year = factor(year)) %>% 
  mutate(mort = 10^lg_cmr) %>% 
  arrange(sex, country) %>% 
  ggplot(.) +
  geom_line(aes(x = age, y = mort, group = year, colour = year)) +
  facet_grid(country ~ sex) + 
  scale_x_continuous(limits= c(0, 90), breaks = seq(0, 90, by = 5)) +
  scale_y_log10(
    breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1), 
    labels = c("0.00001", "0.0001", "0.001", "0.01", "0.1")
    ) + 
  theme_minimal() + 
  labs(x = "Age in single years", y = "Probability of dying in next year", 
         title = "Mortality schedules for particular periods")


ggsave("figures/ssm_examples/schedules_by_period.png", height = 30, width = 25, units = "cm", dpi = 300)


# By cohort 

this_dta %>% 
  mutate(birth_year = year - age) %>% 
  filter(birth_year %in% c(1900, 1930, 1950, 1980)) %>%
  filter(age <= 90) %>% 
  mutate(birth_year = factor(birth_year)) %>% 
  mutate(mort = 10^lg_cmr) %>% 
  arrange(sex, country) %>% 
  ggplot(.) +
  geom_line(aes(x = age, y = mort, group = birth_year, colour = birth_year)) +
  facet_grid(country ~ sex) + 
  scale_x_continuous(limits= c(0, 90), breaks = seq(0, 90, by = 5)) +
  scale_y_log10(
    breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1), 
    labels = c("0.00001", "0.0001", "0.001", "0.01", "0.1")
  ) + 
  theme_minimal() + 
  labs(x = "Age in single years", y = "Probability of dying in next year", 
       title = "Mortality schedules for particular birth cohorts")


ggsave("figures/ssm_examples/schedules_by_cohort.png", height = 30, width = 25, units = "cm", dpi = 300)
