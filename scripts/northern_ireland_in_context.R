# 3/12/2015

# Northern Ireland in Context paper

# The figures should be 

# 1) SCP of Northern Ireland, 1922 onwards
# 2) SCP of Northern Ireland, 1950 onwards
# 3) CLP of NI compared with 1) Ireland, 2) England & Wales; 3)Scotland
# 4) CLP of NI compared with European Regions


# Information -------------------------------------------------------------

# Prereqs -----------------------------------------------------------------

rm(list=ls())

# data management
require(readr)

require(plyr)
require(tidyr)
require(stringr)
require(dplyr)


#graphics
require(lattice)
require(latticeExtra)
require(ggplot2) 
require(RColorBrewer)
require(grid)

# for smoothing
require(fields) 
require(spatstat)

#tables
require(xtable)


# functions ---------------------------------------------------------------
source("scripts/smoother_function.R")
source("scripts/scotland_in_context__helper_functions.R")


# base data  --------------------------------------------------------------


dta <- read_csv("data/tidy/counts_germany_combined.csv")

# country group definitions
source("scripts/scotland_in_context__country_group_definitions.R")

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


#

# Figure 1: SCP of Northern Ireland, Inception to latest year --------------------
png(filename="figures/northern_ireland_in_context/figure_01_scp_ni_all_years_iso.png", 
    width=40, height=25, res=300, units="cm"
)




make_scp <- function(DTA_unsmoothed, DTA_smoothed, COUNTRY,
                     ASPECT= "iso",
                     AGE_RANGE = c(0, 90), 
                     YEAR_RANGE = c(1923, 2010),
                     COL.REGIONS = colorRampPalette(brewer.pal(6, "Reds"))(200),
                     CUTS = 25
){
  shade_part <- DTA_unsmoothed %>%
    filter(
      country == COUNTRY & 
        year >= YEAR_RANGE[1] & year <= YEAR_RANGE[2] &
        age >= AGE_RANGE[1] & age <= AGE_RANGE[2] &
        sex != "total"
    ) %>%
    mutate(
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base=10)
    ) %>%
    levelplot(
      lg_cmr ~ year * age | sex, 
      data=., 
      region=T, 
      par.strip.text=list(cex=1.4, fontface="bold"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      cuts =CUTS,
      aspect=ASPECT,
      col.regions=COL.REGIONS,
      main=NULL,
      xlim=YEAR_RANGE,
      scales=list(
        x=list(cex=1.4, at=seq(1925, 2005, by = 10)), 
        y=list(cex=1.4, at = seq(0, 90, by = 5)),
        alternating=3
      ),
      par.settings=list(strip.background=list(col="lightgrey")),
      panel = function(x, y, z, ...){
        panel.levelplot(x, y, z, ...)
#        panel.abline(a = -1920, b = 1, lty="dashed")
#        panel.abline(v = 1920, lty="dashed")
      }
    )
  
  contour_part <- DTA_smoothed  %>%  
    filter(
      country == COUNTRY & 
        year >= YEAR_RANGE[1] & year <= YEAR_RANGE[2] &
        age >= AGE_RANGE[1] & age <= AGE_RANGE[2] &
        sex != "total"
    ) %>%
    contourplot(
      lg_cmr ~ year + age | sex, 
      data=.,
      region=F,
      ylab="",
      xlab="",
      xlim=YEAR_RANGE,
      scales=list(NULL),
      cuts=CUTS,
      aspect=ASPECT,
      col="black",
      labels=list(
        cex=1.2
      ),
      main=NULL
    )
  
  output <- shade_part + contour_part
}

make_scp(
  DTA_unsmoothed = dta, 
  DTA_smoothed = smooth_fn(dta, 0.7),
  COUNTRY = "GBR_NIR",
  ASPECT = "iso",
  COL.REGIONS = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
  CUTS = 18
) %>% print


dev.off()

# Figure 2: Figure zooming in on male absolute mortality increase for males 
# aged between 15 and 50 from 1960 onwards 

png(filename="figures/northern_ireland_in_context/figure_02_scp_ni_zoomed_iso.png", 
    width=40, height=25, res=300, units="cm"
)




make_scp <- function(DTA_unsmoothed, DTA_smoothed, COUNTRY,
                     ASPECT= "iso",
                     AGE_RANGE = c(15, 50), 
                     YEAR_RANGE = c(1960, 2010),
                     COL.REGIONS = colorRampPalette(brewer.pal(6, "Reds"))(200),
                     CUTS = 25
){
  shade_part <- DTA_smoothed %>%
    filter(
      country == COUNTRY & 
        year >= YEAR_RANGE[1] & year <= YEAR_RANGE[2] &
        age >= AGE_RANGE[1] & age <= AGE_RANGE[2] &
        sex != "total"
    ) %>%
    levelplot(
      lg_cmr ~ year * age | sex, 
      data=., 
      region=T, 
      par.strip.text=list(cex=1.4, fontface="bold"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      cuts =CUTS,
      aspect=ASPECT,
      col.regions=COL.REGIONS,
      main=NULL,
      xlim=YEAR_RANGE,
      scales=list(
        x=list(cex=1.4, at=seq(1925, 2005, by = 5)), 
        y=list(cex=1.4, at = seq(0, 90, by = 5)),
        alternating=3
      ),
      par.settings=list(strip.background=list(col="lightgrey")),
      panel = function(x, y, z, ...){
        panel.levelplot(x, y, z, ...)
        panel.polygon(x = list(
          x = c(1971, 1971, 1990, 1990),
          y = c(17, 35, 35, 17)
          ),
          lty = "dashed",
          fill = NA
          )
        panel.polygon(x = list(
          x = c(1972, 1972, 1981, 1981),
          y = c(18, 34, 34, 18)
        ),
        lty = "dashed",
        fill = NA
        )        
        
        #        panel.abline(a = -1920, b = 1, lty="dashed")
        #        panel.abline(v = 1920, lty="dashed")
      }
    )
  
  contour_part <- DTA_smoothed  %>%  
    filter(
      country == COUNTRY & 
        year >= YEAR_RANGE[1] & year <= YEAR_RANGE[2] &
        age >= AGE_RANGE[1] & age <= AGE_RANGE[2] &
        sex != "total"
    ) %>%
    contourplot(
      lg_cmr ~ year + age | sex, 
      data=.,
      region=F,
      ylab="",
      xlab="",
      xlim=YEAR_RANGE,
      scales=list(NULL),
      cuts=CUTS,
      aspect=ASPECT,
      col="black",
      labels=list(
        cex=1.2
      ),
      main=NULL
    )
  
#  output <- shade_part + contour_part
  output <- shade_part
}

make_scp(
  DTA_unsmoothed = dta, 
  DTA_smoothed = smooth_fn(dta, 0.7),
  COUNTRY = "GBR_NIR",
  ASPECT = "iso",
  COL.REGIONS = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
  CUTS = 20
) %>% print


dev.off()

# Figure 3 : composite: CLP NIR - IRL, NIR - EnW, NIR - SCOT ---------------

dta_nir <- dta %>% 
  filter(
    country == "GBR_NIR",
    year >= 1950 & year <= 2010, 
    sex != "total"
) %>% 
  mutate(
    death_rate = death_count / population_count,
    lg_death_rate = log(death_rate, base = 10)
  )

dta_enw <- dta %>% 
  filter(
    country %in% c("GBRCENW"),
    year >= 1950 & year <= 2010,
    sex !="total"
  ) %>% 
  group_by(
    sex, year, age
  ) %>% 
  summarise(
    death_count = sum(death_count, na.rm = T), 
    population_count = sum(population_count, na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(
    death_rate = death_count / population_count,
    lg_death_rate = log(death_rate, base = 10)
  )


dta_irl <- dta %>% 
  filter(
    country %in% c("IRL"),
    year >= 1950 & year <= 2010,
    sex != "total"
  ) %>% 
  group_by(
    sex, year, age
  ) %>% 
  summarise(
    death_count = sum(death_count, na.rm= T), 
    population_count = sum(population_count, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    death_rate = death_count / population_count,
    lg_death_rate = log(death_rate, base = 10)
  )


dta_sco <- dta %>% 
  filter(
    country %in% c("GBR_SCO"),
    year >= 1950 & year <= 2010,
    sex != "total"
  ) %>% 
  group_by(
    sex, year, age
  ) %>% 
  summarise(
    death_count = sum(death_count, na.rm= T), 
    population_count = sum(population_count, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    death_rate = death_count / population_count,
    lg_death_rate = log(death_rate, base = 10)
  )




# Difference between ni and enw
tmp1 <- dta_nir %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Northern Ireland` = lg_death_rate )

tmp2 <- dta_enw  %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`England & Wales` = lg_death_rate)

dif_nir_enw <- tmp1 %>% 
  left_join(tmp2) %>% 
  mutate(
    `NI less E&W` = `Northern Ireland` - `England & Wales`
  ) %>% 
  select(
    year, age, sex, `NI less E&W`
  )

rm(tmp1, tmp2)

# Difference between ni and irl
tmp1 <- dta_nir %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Northern Ireland` = lg_death_rate )

tmp2 <- dta_irl  %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Ireland` = lg_death_rate)

dif_nir_irl <- tmp1 %>% 
  left_join(tmp2) %>% 
  mutate(
    `NI less Ireland` = `Northern Ireland` - `Ireland`
  ) %>% 
  select(
    year, age, sex, `NI less Ireland`
  )

rm(tmp1, tmp2)

# Difference between ni and scot
tmp1 <- dta_nir %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Northern Ireland` = lg_death_rate )

tmp2 <- dta_sco  %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Scotland` = lg_death_rate)

dif_nir_sco <- tmp1 %>% 
  left_join(tmp2) %>% 
  mutate(
    `NI less Scotland` = `Northern Ireland` - `Scotland`
  ) %>% 
  select(
    year, age, sex, `NI less Scotland`
  )

rm(tmp1, tmp2)


# Join the above together

comparisons <- dif_nir_enw %>% 
  left_join(dif_nir_irl) %>% 
  left_join(dif_nir_sco) %>% 
  gather(key = "comparison", value = "dif_logs", -year, -age, -sex)



png(filename="figures/northern_ireland_in_context/figure_03_clp_nir_irl_ruk.png", 
    width=30, height=30, res=300, units="cm"
)

comparisons %>% 
  mutate(
    dif_logs = ifelse(dif_logs < -0.30, -0.30, dif_logs),
    dif_logs = ifelse(dif_logs > 0.30, 0.30, dif_logs)
  ) %>%   
  smooth_var(., 
             group_vars = c("sex", "comparison"), 
             smooth_var = "dif_logs", 
             smooth_par = 0.7
  ) %>% 
  levelplot(
    dif_logs ~ year * age | comparison + sex,
    data=., 
    region=T,
    ylab="Age in years",
    xlab="Year",
    at = seq(from= -0.30, to = 0.30, by=0.01),
    col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(200),
    scales=list(alternating=3),
    main=NULL,
    aspect= "iso",
    ylim=c(0, 60),
    xlim=c(1950, 2010),
    par.settings=list(strip.background=list(col="lightgrey")), 
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      panel.polygon(x = list(
        x = c(1971, 1971, 1990, 1990),
        y = c(17, 35, 35, 17)
      ),
      fill = NA
      )
      panel.polygon(x = list(
        x = c(1972, 1972, 1981, 1981),
        y = c(18, 34, 34, 18)
      ),
      fill = NA
      )     
    }
  )


  dev.off()
  
  
  
  
# Northern Ireland compared with different European regions -----------------------

rgn_we <- dta %>% 
    filter(
      country  %in% europe_western,
      age <= 90, 
      sex != "total",
      year >= 1950 & year <= 2010
    ) %>% 
    filter(
      country != "GBR_NIR"
    ) %>% 
    group_by(sex, year, age) %>% 
    summarise(
      death_count = sum(death_count, na.rm = T),
      population_count = sum(population_count, na.rm = T)
    ) %>% 
    ungroup %>% 
    mutate(
      death_rate = death_count / population_count,
      lg_death_rate = log(death_rate, base = 10)
    ) %>% 
    select(
      sex, year, age, `Western Europe` = lg_death_rate
    )
  
  
rgn_ne <- dta %>% 
    filter(
      country  %in% europe_northern,
      age <= 90, 
      sex != "total",
      year >= 1950 & year <= 2010
    ) %>% 
    group_by(sex, year, age) %>% 
    summarise(
      death_count = sum(death_count, na.rm = T),
      population_count = sum(population_count, na.rm = T)
    ) %>% 
    ungroup %>% 
    mutate(
      death_rate = death_count / population_count,
      lg_death_rate = log(death_rate, base = 10)
    ) %>% 
    select(
      sex, year, age, `Northern Europe` = lg_death_rate
    )
  
rgn_ee <- dta %>% 
  filter(
    country  %in% europe_eastern,
    age <= 90, 
    sex != "total",
    year >= 1950 & year <= 2010
  ) %>% 
  group_by(sex, year, age) %>% 
  summarise(
    death_count = sum(death_count, na.rm = T),
    population_count = sum(population_count, na.rm = T)
  ) %>% 
  ungroup %>% 
  mutate(
    death_rate = death_count / population_count,
    lg_death_rate = log(death_rate, base = 10)
  ) %>% 
  select(
    sex, year, age, `Eastern Europe` = lg_death_rate
  )

rgn_se <- dta %>% 
  filter(
    country  %in% europe_southern,
    age <= 90, 
    sex != "total",
    year >= 1950 & year <= 2010
  ) %>% 
  group_by(sex, year, age) %>% 
  summarise(
    death_count = sum(death_count, na.rm = T),
    population_count = sum(population_count, na.rm = T)
  ) %>% 
  ungroup %>% 
  mutate(
    death_rate = death_count / population_count,
    lg_death_rate = log(death_rate, base = 10)
  ) %>% 
  select(
    sex, year, age, `Southern Europe` = lg_death_rate
  )


rgn <- rgn_se %>% 
  left_join(rgn_ee) %>% 
  left_join(rgn_ne) %>% 
  left_join(rgn_we) %>% 
  gather(key = "comparison", value = "lg_death_rate", -sex, -year, -age)

region_nir_difs <- dta_nir %>% 
  select(sex, age, year, `Northern Ireland` = lg_death_rate) %>% 
  left_join(rgn) %>% 
  mutate(
    dif_logs = `Northern Ireland` - lg_death_rate,
    dif_logs = ifelse(is.na(dif_logs), 0, dif_logs)
    ) %>% 
  select(-`Northern Ireland`, -lg_death_rate)





png(filename="figures/northern_ireland_in_context/figure_04_clp_nir_against_euro_regions.png", 
    width=40, height=30, res=300, units="cm"
)

region_nir_difs %>% 
  smooth_var(., 
             group_vars = c("comparison", "sex"), 
             smooth_var = "dif_logs", 
             smooth_par = 0.7
  ) %>% 
  mutate(
    dif_logs = ifelse(dif_logs < -0.40, -0.40, dif_logs),
    dif_logs = ifelse(dif_logs > 0.40, 0.40, dif_logs)
  ) %>%   
  levelplot(
    dif_logs ~ year * age | comparison + sex,
    data=., 
    region=T,
    ylab="Age in years",
    xlab="Year",
    at = seq(from= -0.40, to = 0.40, by=0.01),
    col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(200),
    scales=list(alternating=3),
    main=NULL,
    aspect= "iso",
    ylim=c(0, 60),
    xlim=c(1950, 2010),
    par.settings=list(strip.background=list(col="lightgrey")), 
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      panel.polygon(x = list(
        x = c(1971, 1971, 1990, 1990),
        y = c(17, 35, 35, 17)
      ),
      fill = NA
      )
      panel.polygon(x = list(
        x = c(1972, 1972, 1981, 1981),
        y = c(18, 34, 34, 18)
      ),
      fill = NA
      )     
    }
  )

dev.off()


  


# Appendix ----------------------------------------------------------------


# Scotland compared with England & Wales
# Scotland compared with Northern Ireland




# Figure 2 : composite: CLP Scot - rUK, Scot - rWE, UK - rWE ---------------

dta_scot <- dta %>% 
  filter(
    country == "GBR_SCO",
    year >= 1950 & year <= 2010, 
    sex != "total"
  ) %>% 
  mutate(
    death_rate = death_count / population_count,
    lg_death_rate = log(death_rate, base = 10)
  )

dta_enw <- dta %>% 
  filter(
    country %in% c("GBRCENW"),
    year >= 1950 & year <= 2010,
    sex !="total"
  ) %>% 
  group_by(
    sex, year, age
  ) %>% 
  summarise(
    death_count = sum(death_count, na.rm = T), 
    population_count = sum(population_count, na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(
    death_rate = death_count / population_count,
    lg_death_rate = log(death_rate, base = 10)
  )


dta_nir <- dta %>% 
  filter(
    country %in% c("GBR_NIR"),
    year >= 1950 & year <= 2010,
    sex !="total"
  ) %>% 
  group_by(
    sex, year, age
  ) %>% 
  summarise(
    death_count = sum(death_count, na.rm = T), 
    population_count = sum(population_count, na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(
    death_rate = death_count / population_count,
    lg_death_rate = log(death_rate, base = 10)
  )




# Difference between scotland and england & wales
tmp1 <- dta_scot %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(Scotland = lg_death_rate )

tmp2 <- dta_enw  %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`England & Wales` = lg_death_rate)

dif_scot_enw <- tmp1 %>% 
  left_join(tmp2) %>% 
  mutate(
    `Scotland less England & Wales` = Scotland - `England & Wales`
  ) %>% 
  select(
    year, age, sex, `Scotland less England & Wales`
  )

rm(tmp1, tmp2)


# Difference between scotland and Northern Ireland
tmp1 <- dta_scot %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(Scotland = lg_death_rate )

tmp2 <- dta_nir  %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Northern Ireland` = lg_death_rate)

dif_scot_nir <- tmp1 %>% 
  left_join(tmp2) %>% 
  mutate(
    `Scotland less Northern Ireland` = Scotland - `Northern Ireland`
  ) %>% 
  select(
    year, age, sex, `Scotland less Northern Ireland`
  )

rm(tmp1, tmp2)



# Join the above together

comparisons <- dif_scot_enw %>% 
  left_join(dif_scot_nir) %>% 
  gather(key = "comparison", value = "dif_logs", -year, -age, -sex)



png(filename="figures/scotland_in_context/final_figures/figure_A01_clp_scot_enw_nir.png", 
    width=30, height=30, res=300, units="cm"
)

comparisons %>% 
  mutate(
    dif_logs = ifelse(dif_logs < -0.30, -0.30, dif_logs),
    dif_logs = ifelse(dif_logs > 0.30, 0.30, dif_logs)
  ) %>%   
  smooth_var(., 
             group_vars = c("sex", "comparison"), 
             smooth_var = "dif_logs", 
             smooth_par = 0.7
  ) %>% 
  levelplot(
    dif_logs ~ year * age | comparison + sex,
    data=., 
    region=T,
    ylab="Age in years",
    xlab="Year",
    at = seq(from= -0.30, to = 0.30, by=0.01),
    col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(200),
    scales=list(alternating=3),
    main=NULL,
    aspect= "iso",
    ylim=c(0, 90),
    xlim=c(1950, 2010),
    par.settings=list(strip.background=list(col="lightgrey")), 
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      panel.abline(a = -1920, b = 1, col = "black")
      panel.abline(a = -1950, b = 1, col = "black")
      panel.abline(a = -1960, b = 1, col ="black")
      panel.abline(v = c(1970, 1990, 2000), col = "black")
      panel.abline(h = 18, col = "black")
    }
  )

dev.off()



# Difference between England and 1) Scotland; 2) rest of Western Europe
# 3) Rest of Western Europe except rest of UK



# Figure 2 : composite: CLP EnW - rUK, EnW - rWE, UK - rWE ---------------

dta_enw <- dta %>% 
filter(
  country == "GBRCENW",
  year >= 1950 & year <= 2010, 
  sex != "total"
) %>% 
mutate(
  death_rate = death_count / population_count,
  lg_death_rate = log(death_rate, base = 10)
)

dta_rUK <- dta %>% 
filter(
  country %in% c("GBR_SCO", "GBR_NIR"),
  year >= 1950 & year <= 2010,
  sex !="total"
) %>% 
group_by(
  sex, year, age
) %>% 
summarise(
  death_count = sum(death_count, na.rm = T), 
  population_count = sum(population_count, na.rm=T)
) %>% 
ungroup() %>% 
mutate(
  death_rate = death_count / population_count,
  lg_death_rate = log(death_rate, base = 10)
)


dta_uk <- dta %>% 
filter(
  country %in% c("GBR_SCO", "GBRCENW", "GBR_NIR"),
  year >= 1950 & year <= 2010,
  sex != "total"
) %>% 
group_by(
  sex, year, age
) %>% 
summarise(
  death_count = sum(death_count, na.rm= T), 
  population_count = sum(population_count, na.rm = T)
) %>% 
ungroup() %>% 
mutate(
  death_rate = death_count / population_count,
  lg_death_rate = log(death_rate, base = 10)
)


dta_we_less_enw <- dta %>% 
filter(
  country %in% c(
    Austria = "AUT",
    Belgium = "BEL",
    Switzerland = "CHE",
    Germany = "DEUT",
    France = "FRACNP",
    `Northern Ireland` = "GBR_NIR",
    Scotland = "GBR_SCO",
    Ireland = "IRL",
    Luxembourg = "LUX",
    Netherlands = "NLD"  
  ),
  year >= 1950 & year <= 2010, 
  sex != "total"
) %>% 
group_by(
  sex, year, age
) %>% 
summarise(
  death_count = sum(death_count, na.rm = T), 
  population_count = sum(population_count, na.rm = T)
) %>% 
ungroup() %>% 
mutate(
  death_rate = death_count / population_count,
  lg_death_rate = log(death_rate, base = 10)
)





dta_we_less_uk <- dta %>% 
filter(
  country %in% c(
    Austria = "AUT",
    Belgium = "BEL",
    Switzerland = "CHE",
    Germany = "DEUT",
    France = "FRACNP",
    Ireland = "IRL",
    Luxembourg = "LUX",
    Netherlands = "NLD"  
  ),
  year >= 1950 & year <= 2010, 
  sex != "total"
) %>% 
group_by(
  sex, year, age
) %>% 
summarise(
  death_count = sum(death_count, na.rm = T), 
  population_count = sum(population_count, na.rm = T)
) %>%   
ungroup() %>% 
mutate(
  death_rate = death_count / population_count,
  lg_death_rate = log(death_rate, base = 10)
)


# Difference between scotland and ruk
tmp1 <- dta_enw %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`England & Wales` = lg_death_rate )

tmp2 <- dta_rUK  %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Rest of UK` = lg_death_rate)

dif_enw_rest_UK <- tmp1 %>% 
left_join(tmp2) %>% 
mutate(
  `England & Wales less Rest of UK` = `England & Wales` - `Rest of UK`
) %>% 
select(
  year, age, sex, `England & Wales less Rest of UK`
)

rm(tmp1, tmp2)


# Difference between England & Wales and rest of WE

tmp1 <- dta_enw %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`England & Wales` = lg_death_rate )

tmp2 <- dta_we_less_enw  %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Rest of WE` = lg_death_rate)

dif_enw_rest_WE <- tmp1 %>% 
left_join(tmp2) %>% 
mutate(
  `England & Wales less Rest of WE` = `England & Wales` - `Rest of WE`
) %>% 
select(
  year, age, sex, `England & Wales less Rest of WE`
)

rm(tmp1, tmp2)


# Difference between UK and rest of WE

tmp1 <- dta_uk %>% 
select(year, age, sex, lg_death_rate) %>% 
rename(UK = lg_death_rate)

tmp2 <- dta_we_less_uk %>% 
select(year, age, sex, lg_death_rate) %>% 
rename(`Rest of WE` = lg_death_rate)

dif_UK_rest_WE <- tmp1 %>% 
left_join(tmp2) %>% 
mutate(`UK less Rest of WE` = UK - `Rest of WE`) %>% 
select(year, age, sex, `UK less Rest of WE`)

rm(tmp1, tmp2)



# Difference between England & Wales and rest of non-UK WE

tmp1 <- dta_enw %>% 
select(year, age, sex, lg_death_rate) %>% 
rename(`England & Wales` = lg_death_rate )

tmp2 <- dta_we_less_uk  %>% 
select(year, age, sex, lg_death_rate) %>% 
rename(`Rest of WE` = lg_death_rate)

dif_enw_rest_nonUK_WE <- tmp1 %>% 
left_join(tmp2) %>% 
mutate(
  `England & Wales less Rest of Non-UK WE` = `England & Wales` - `Rest of WE`
) %>% 
select(
  year, age, sex, `England & Wales less Rest of Non-UK WE`
)

rm(tmp1, tmp2)
# Join the above together

comparisons <- dif_enw_rest_UK %>% 
left_join(dif_enw_rest_WE) %>% 
left_join(dif_UK_rest_WE) %>% 
left_join(dif_enw_rest_nonUK_WE) %>% 
gather(key = "comparison", value = "dif_logs", -year, -age, -sex)




png(filename="figures/scotland_in_context/final_figures/figure_A02_clp_enw_uk_we.png", 
    width=45, height=30, res=300, units="cm"
)

comparisons %>% 
mutate(
  dif_logs = ifelse(dif_logs < -0.30, -0.30, dif_logs),
  dif_logs = ifelse(dif_logs > 0.30, 0.30, dif_logs)
) %>%   
smooth_var(., 
           group_vars = c("sex", "comparison"), 
           smooth_var = "dif_logs", 
           smooth_par = 0.7
) %>% 
levelplot(
  dif_logs ~ year * age | comparison + sex,
  data=., 
  region=T,
  ylab="Age in years",
  xlab="Year",
  at = seq(from= -0.30, to = 0.30, by=0.01),
  col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(200),
  scales=list(alternating=3),
  main=NULL,
  aspect= "iso",
  ylim=c(0, 90),
  xlim=c(1950, 2010),
  par.settings=list(strip.background=list(col="lightgrey")), 
  panel = function(x, y, z, ...){
    panel.levelplot(x, y, z, ...)
    panel.abline(a = -1920, b = 1, col = "black")
    panel.abline(a = -1950, b = 1, col = "black")
    panel.abline(a = -1960, b = 1, col ="black")
    panel.abline(v = c(1970, 1990, 2000), col = "black")
    panel.abline(h = 18, col = "black")
  }
)

dev.off()




