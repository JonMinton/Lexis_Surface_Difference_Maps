# 19/9/2015

# Final figures for Scotland in Context paper

# The final figures should be 

# 1) SCP of Scotland, 1900 to 2010
# 2) CLP Lattice plot
#   i ) Scotland - rUK
#  ii ) Scotland - rWE
#  iii) UK - rWE


# Information -------------------------------------------------------------

# How does Scotland compare with the rest of the UK, and the rest of Western Europe?

# Paper suggested by Gerry MacCartney 
# 29/5/2015

# Do this in a single script file

# UK
# - Northern Ireland
# - England & Wales
# - Scotland

# Western Europe:
# - Northern Ireland
# - England & Wales
# - Scotland 
# - Ireland
# - France
# - Belgium
# - Luxembourg
# - Netherlands
# - Germany
# - Switzerland
# - Austria

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

dta_smoothed_0_1 <- smooth_fn(dta, 0.1)
dta_smoothed_0_5 <- smooth_fn(dta, 0.5) 
dta_smoothed_1_0 <- smooth_fn(dta, 1.0)
dta_smoothed_1_5 <- smooth_fn(dta, 1.5)

#

# Figure 1: SCP of Scotland, 1900-2010, less smoothing --------------------
png(filename="figures/scotland_in_context/final_figures/rest_uk_overall_scp_iso.png", 
    width=40, height=25, res=300, units="cm"
)


make_scp(
  DTA_unsmoothed = dta, 
  DTA_smoothed = dta_smoothed_0_5,
  COUNTRY = "GBR_SCO",
  ASPECT = "iso",
  COL.REGIONS = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
  CUTS = 18
  ) %>% print

dev.off()



# Figure2 : composite: CLP Scot - rUK, Scot - rWE, UK - rWE ---------------

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

dta_rUK <- dta %>% 
  filter(
    country %in% c("GBR_CENW", "GBR_NIR"),
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
    country %in% c("GBR_SCO", "GBR_CENW", "GBR_NIR"),
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


dta_we_less_scot <- dta %>% 
  filter(
    country %in% c(
      Austria = "AUT",
      Belgium = "BEL",
      Switzerland = "CHE",
      Germany = "DEUT",
      France = "FRACNP",
      `Northern Ireland` = "GBR_NIR",
      `England & Wales` = "GBRCENW",
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
tmp1 <- dta_scot %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(Scotland = lg_death_rate )

tmp2 <- dta_rUK  %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Rest of UK` = lg_death_rate)

dif_scot_rest_UK <- tmp1 %>% 
  left_join(tmp2) %>% 
  mutate(
    `Scotland less Rest of UK` = Scotland - `Rest of UK`
  ) %>% 
  select(
    year, age, sex, `Scotland less Rest of UK`
  )

rm(tmp1, tmp2)


# Difference between Scotland and rest of WE

tmp1 <- dta_scot %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(Scotland = lg_death_rate )

tmp2 <- dta_we_less_scot  %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Rest of WE` = lg_death_rate)

dif_scot_rest_WE <- tmp1 %>% 
  left_join(tmp2) %>% 
  mutate(
    `Scotland less Rest of WE` = Scotland - `Rest of WE`
  ) %>% 
  select(
    year, age, sex, `Scotland less Rest of WE`
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

# Join the above together

comparisons <- dif_scot_rest_UK %>% 
  left_join(dif_scot_rest_WE) %>% 
  left_join(dif_UK_rest_WE) %>% 
  gather(key = "comparison", value = "dif_logs", -year, -age, -sex)



make_clp_lattice <- function(DTA, DTA_overall, CODES,
                             ASPECT = "iso",
                             YEAR_RANGE = , 
                             AGE_RANGE = ,
                             COL.REGIONS = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
                             ADD_CONTOURS = F,
                             AT = 
){
  tmp1 <- DTA  %>% 
    mutate(cmr = death_count/ population_count)  %>% 
    select(country, year, age, sex, cmr)
  
  tmp2 <- DTA_overall %>%  
    select(year, age, sex, overall_cmr = cmr)
  
  tmp3 <- tmp1 %>% left_join(tmp2)
  
  dif_dta <- tmp3 %>% 
    filter(!is.na(cmr) &!is.na(overall_cmr)) %>% 
    mutate(dif_lg_cmr = log(cmr, base = 10) - log(overall_cmr, base = 10))
  rm(tmp1, tmp2, tmp3)
  
  dif_dta_blurred <- dif_dta %>% smooth_var(
    dta=.,
    group_vars= c("country", "sex"),
    smooth_var="dif_lg_cmr",
    smooth_par=1.4
  )
  
  
  comparisons %>% 
    levelplot(
      dif_logs ~ year * age | comparison + sex,
      data=., 
      region=T,
      ylab="Age in years",
      xlab="Year",
      at = seq(from= -1.2, to = 1.2, by=0.2),
      col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
      scales=list(alternating=3),
      main=NULL,
      aspect= "iso",
      ylim=c(0, 90),
      xlim=c(1950, 2010),
      par.settings=list(strip.background=list(col="lightgrey"))
    )
  