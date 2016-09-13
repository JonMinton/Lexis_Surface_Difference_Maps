# Quick script which shows how mortality contour hurdles have changed
# by cohort year. 

# England & Wales?

rm(list = ls())


require(readr)

require(plyr)
require(tidyr)
require(stringr)
require(dplyr)
require(car)

#graphics
require(lattice)
require(latticeExtra)

require(ggplot2) 
require(RColorBrewer)
require(grid)

# for smoothing
require(fields) 
require(spatstat)


dta <- read_csv("data/tidy/counts.csv")

this_dta <- dta %>% 
  filter(country == "GBRTENW" & sex !="total") %>% 
  mutate(birth_year = year - age) %>% 
  filter(birth_year >= 1850 & age >= 50 & age <=90) %>% 
  arrange(sex, birth_year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
    ) %>% 
  select(sex, birth_year, age, lg_cmr)



png(filename="figures/shifting_hurdles/shifting_hurdles_spectral.png", 
    width=30, height=20, res=300, units="cm"
)

this_is_a_very_long_name_that_i_have_decided_to_use_because_i_really_like_typing_for_the_sake_of_it <- 2

this

  contourplot(
    lg_cmr ~ age * birth_year | sex, 
    data=this_dta, 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Birth year", cex=1.4),
    xlab=list(label="Age in years", cex=1.4),
    par.settings=list(strip.background=list(col="lightgrey")),
    scales=list(
      x=list(cex=1.2, at = seq(50, 90, by = 5)), 
      y=list(cex=1.2, at = seq(1850, 1960, by = 10)),
      alternating=T
    ),
    col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    cuts = 10
  )
  
dev.off()


# Second version - with Scotland as well 

this_dta <- dta %>% 
  filter(country %in%  c("GBRTENW", "GBR_SCO") & sex !="total") %>%
  mutate(country = recode(country, 
    "'GBRTENW' = 'England & Wales';
    'GBR_SCO' = 'Scotland'
                          ")) %>% 
  mutate(birth_year = year - age) %>% 
  filter(birth_year >= 1850 & age >= 50 & age <=90) %>% 
  arrange(sex, birth_year, age) %>%
  mutate(
    cmr  = death_count / population_count, 
    lg_cmr = log(cmr, base = 10)
  ) %>% 
  select(country, sex, birth_year, age, lg_cmr)


png(filename="figures/shifting_hurdles/shifting_hurdles_spectral_twocountries.png", 
    width=40, height=40, res=300, units="cm"
)


contourplot(
  lg_cmr ~ age * birth_year | sex + country, 
  data=this_dta, 
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab=list(label="Birth year", cex=1.4),
  xlab=list(label="Age in years", cex=1.4),
  par.settings=list(strip.background=list(col="lightgrey")),
  scales=list(
    x=list(cex=1.2, at = seq(50, 90, by = 5)), 
    y=list(cex=1.2, at = seq(1850, 1960, by = 10)),
    alternating=3
  ),
  col.regions = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
  cuts = 10
)

dev.off()
  