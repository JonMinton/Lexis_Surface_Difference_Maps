# 12/5/ 2016


# Annotated version of levelplot 

# The purpose of this code is to create an annotated levelplot highlighting different features


# To start with, let's try to add all attributes to all figures 


# Prereqs -----------------------------------------------------------------

rm(list=ls())

#install.packages("pacman")
require(pacman)

p_load(
  readr, plyr, stringr, dplyr,
  lattice, latticeExtra, ggplot2,
  RColorBrewer, grid, 
  fields, spatstat, 
  xtable
)

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

dta_rUK <- dta %>% 
  filter(
    country %in% c("GBRCENW", "GBR_NIR"),
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



# png(filename="figures/scotland_in_context/final_figures/figure_02_clp_scot_uk_we.png", 
#     width=30, height=30, res=300, units="cm"
# )

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

# Produce 'plain' version of above for adding annotations to  
png(filename="figures/scotland_in_context/final_figures/figure_02a_clp_scot_uk_we_plain.png", 
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
  )

dev.off()



# Scotland compared with different European regions -----------------------

rgn_we <- dta %>% 
  filter(
    country  %in% europe_western,
    age <= 90, 
    sex != "total",
    year >= 1950 & year <= 2010
  ) %>% 
  filter(
    country != "GBR_SCO"
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

region_scot_difs <- dta_scot %>% 
  select(sex, age, year, Scotland = lg_death_rate) %>% 
  left_join(rgn) %>% 
  mutate(
    dif_logs = Scotland - lg_death_rate,
    dif_logs = ifelse(is.na(dif_logs), 0, dif_logs)
  ) %>% 
  select(-Scotland, -lg_death_rate)





png(filename="figures/scotland_in_context/final_figures/figure_03_clp_scot_against_euro_regions.png", 
    width=30, height=30, res=300, units="cm"
)

region_scot_difs %>% 
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




