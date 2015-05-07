# Code for figs for EJE paper

# To do:  -----------------------------------------------------------------


# Load packages -----------------------------------------------------------

rm(list=ls())

require(plyr)
require(tidyr)
require(dplyr)
require(stringr)

require(ggplot2)
require(lattice)
require(latticeExtra)
require(RColorBrewer)
require(grid)

require(xtable)

# for smoothing
require(fields) 
require(spatstat)



# Manage data (base and derived) ------------------------------------------


source("scripts/manage_data.R")

  
# Figures and tables -----------------------------------------------------------------




# Correlations ------------------------------------------------------------


# Correlations : mean and var, e0 and e5 ------------------------------------------------------------

# correlations between means and variances for e0 and e5

dif_mnvars_e0 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))

dif_mnvars_e5 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))

dif_mnvars_e65 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))

# Now for 1950 onwards

dif_mnvars_e0 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  filter(year >=1950) %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))

dif_mnvars_e5 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  filter(year >=1950) %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))

dif_mnvars_e65 %>%
  arrange(sex, year) %>%
  select(year, sex, ex=mean_death_overall, vx=var_death_overall) %>%
  distinct %>%
  filter(year >=1950) %>%
  ddply(., .(sex), function(x) round(cor(x=x$ex, y= x$vx), 2))


