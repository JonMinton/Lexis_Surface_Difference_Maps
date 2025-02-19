# 17/11/2015

# Exploration of Case & Deaton results 

# Figures: 

# SCP of USA
# CLP: USA vs Canada
# CLP: USA vs Anglophone
# CLP: USA vs Western Europe
# CLP: USA vs Europe 


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


# Quick bathtub curves for illustration -----------------------------------


dta %>% 
  filter(country =="GBR_SCO") %>% 
  mutate(death_rate = death_count  / population_count) %>% 
  filter(year %in% c(1855, 1900, 1950, 2000)) %>% 
  mutate(year = as.factor(year)) %>% 
  filter(age <=90) %>% 
  filter(sex != "total") %>% 
  ggplot(data = .) + 
  geom_line(aes(x = age, y = death_rate, group = year, colour = year, linetype = year)) +
  scale_x_continuous(breaks = c(0, seq(5, 90, by = 5))) + 
  facet_wrap(~ sex) + 
  labs(x = "Age in years", y = "Risk of dying in next year") + 
  theme_bw()

ggsave("figures/scot_bathtub_selected_years.png", width = 25, height = 15, units = "cm", dpi = 300)

dta %>% 
  filter(country =="GBR_SCO") %>% 
  mutate(death_rate = death_count  / population_count) %>% 
  filter(year %in% c(1855, 1900, 1950, 2000)) %>% 
  mutate(year = as.factor(year)) %>% 
  filter(age <=90) %>% 
  filter(sex != "total") %>% 
  ggplot(data = .) + 
  geom_line(aes(x = age, y = death_rate, group = year, colour = year, linetype = year)) + 
  scale_y_log10() + 
  scale_x_continuous(breaks = c(0, seq(5, 90, by = 5))) + 
  facet_wrap(~ sex) + 
  labs(x = "Age in years", y = "Risk of dying in next year") + 
  theme_bw()

ggsave("figures/scot_bathtub_selected_years_log10.png", width = 25, height = 15, units = "cm", dpi = 300)


# e0 and e5 for each year

dta %>% filter(country == "GBR_SCO") %>% 
  filter(sex != "total") %>% 
  filter(age <= 90) %>% 
  group_by(year, sex) %>% 
  mutate(ad = age * death_count) %>% 
  summarise(
    e0 = sum(ad) / sum(death_count), 
    e5 = sum(ad[age >= 5]) / sum(death_count[age >= 5])
    ) %>% 
  gather(key = measure, value = value, e0, e5) %>% 
  ggplot(data = .) +
  geom_line(aes(x = year, y = value, group = measure, linetype = measure)) + 
  facet_wrap( ~ sex) + 
  scale_x_continuous(breaks = seq(1860, 2000, by = 20)) + 
  labs(x = "Year", y = "Period life expectancy in years of age") + 
  theme_bw()

ggsave("figures/e0_e5_scotland.png", width = 25, height = 15, units = "cm", dpi = 300)


dta %>% 
  filter(country =="GBR_SCO") %>% 
  mutate(death_rate = death_count  / population_count) %>% 
  filter(year %in% c(2010)) %>% 
  mutate(year = as.factor(year)) %>% 
  filter(age <=90) %>% 
  filter(sex == "male") %>% 
  ggplot(data = .) + 
  geom_line(aes(x = age, y = death_rate)) + 
  scale_y_log10() + 
  scale_x_continuous(breaks = c(0, seq(5, 90, by = 5))) +  
  labs(x = "Age in years", y = "Risk of dying in next year") + 
  theme_bw()

ggsave("figures/scot_bathtub_male_2010.png", width = 15, height = 15, units = "cm", dpi = 300)

#

# Figure 1: SCP of Scotland, 1900-2010, less smoothing --------------------
png(filename="figures/case_deaton/usa_scp_all.png", 
    width=40, height=25, res=300, units="cm"
)




make_scp <- function(DTA_unsmoothed, DTA_smoothed, COUNTRY,
                     ASPECT= "iso",
                     AGE_RANGE = c(0, 90), 
                     YEAR_RANGE = c(1933, 2010),
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
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      ),
      par.settings=list(strip.background=list(col="lightgrey")),
      panel = function(x, y, z, ...){
        panel.levelplot(x, y, z, ...)
        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
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
  
  output <- shade_part  + contour_part
}

make_scp(
  DTA_unsmoothed = dta, 
  DTA_smoothed = smooth_fn(dta, 0.2),
  COUNTRY = "USA",
  ASPECT = "iso",
  COL.REGIONS = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
  CUTS = 18
) %>% print


dev.off()





# Figure just of Case & Deaton age group

png(filename="figures/case_deaton/usa_scp_subgroup.png", 
    width=40, height=25, res=300, units="cm"
)




make_scp <- function(DTA_unsmoothed, DTA_smoothed, COUNTRY,
                     ASPECT= "iso",
                     AGE_RANGE = c(43, 55), 
                     YEAR_RANGE = c(1999, 2010),
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
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      ),
      par.settings=list(strip.background=list(col="lightgrey")),
      panel = function(x, y, z, ...){
        panel.levelplot(x, y, z, ...)
#        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
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
  
  output <- shade_part  + contour_part
}

make_scp(
  DTA_unsmoothed = dta, 
  DTA_smoothed = smooth_fn(dta, 0.1),
  COUNTRY = "USA",
  ASPECT = "iso",
  COL.REGIONS = rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
  CUTS = 25
) %>% print


dev.off()



# Figure 2 : composite: CLP Scot - rUK, Scot - rWE, UK - rWE ---------------

dta_usa <- dta %>% 
  filter(
    country == "USA",
    year >= 1933 & year <= 2010, 
    sex != "total"
) %>% 
  mutate(
    death_rate = death_count / population_count,
    lg_death_rate = log(death_rate, base = 10)
  )

dta_can <- dta %>% 
  filter(
    country %in% c("CAN"),
    year >= 1933 & year <= 2010,
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


dta_anglophone <- dta %>% 
  filter(
    country %in% c("GBR_SCO", "GBRCENW", "GBR_NIR", "CAN", "AUS", "NZL_NP", "IRL"),
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


dta_we <- dta %>% 
  filter(
    country %in% c(
      Austria = "AUT",
      Belgium = "BEL",
      Switzerland = "CHE",
      Germany = "DEUT",
      France = "FRACNP",
      `Northern Ireland` = "GBR_NIR",
      `England & Wales` = "GBRCENW",
      `Scotland` = "GBR_SCO",
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



# Difference between USA and Canada
tmp1 <- dta_usa %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(USA = lg_death_rate )

tmp2 <- dta_can  %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Canada` = lg_death_rate)

dif_usa_can <- tmp1 %>% 
  left_join(tmp2) %>% 
  mutate(
    `USA less Canada` = USA - `Canada`
  ) %>% 
  select(
    year, age, sex, `USA less Canada`
  )

rm(tmp1, tmp2)


# Difference between USA and Other Anglophone nations

tmp1 <- dta_usa %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(USA = lg_death_rate )

tmp2 <- dta_anglophone  %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Rest of Anglophone` = lg_death_rate)

dif_USA_rest_anglophone <- tmp1 %>% 
  left_join(tmp2) %>% 
  mutate(
    `USA less Rest of Anglophone` = USA - `Rest of Anglophone`
  ) %>% 
  select(
    year, age, sex, `USA less Rest of Anglophone`
  )

rm(tmp1, tmp2)


# Difference between USA and  WE

tmp1 <- dta_usa %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(USA = lg_death_rate)

tmp2 <- dta_we %>% 
  select(year, age, sex, lg_death_rate) %>% 
  rename(`Western Europe` = lg_death_rate)

dif_USA_WE <- tmp1 %>% 
  left_join(tmp2) %>% 
  mutate(`USA less Western Europe` = USA - `Western Europe`) %>% 
  select(year, age, sex, `USA less Western Europe`)

rm(tmp1, tmp2)

# Join the above together

comparisons <- dif_usa_can %>% 
  left_join(dif_USA_rest_anglophone) %>% 
  left_join(dif_USA_WE) %>% 
  gather(key = "comparison", value = "dif_logs", -year, -age, -sex)



png(filename="figures/case_deaton/clp_main.png", 
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
      panel.abline(a = -1930, b = 1, col = "black")
      panel.abline(a = -1945, b = 1, col = "black")
      panel.abline(a = -1966, b = 1, col = "black")
      panel.abline(v = c(1985, 1996, 2003), col = "black")
      panel.abline(h = c(14, 18, 32), col = "black")
    }
  )

  dev.off()
  
  
  # CLP - plain
  
  png(filename="figures/case_deaton/clp_plain.png", 
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
 #       panel.abline(a = -1920, b = 1, col = "black")
#        panel.abline(a = -1940, b = 1, col = "black")
#        panel.abline(v = c(1985, 1996, 2003), col = "black")
#        panel.abline(h = c(14, 18, 32), col = "black")
      }
    )
  
  dev.off()
  
  
  
  
  



# Longer term data looking at convergence of all-cause mort by race -------


dta_old <- read_delim(file = "data/usa_multiple_cause/Compressed Mortality, 1979-1998.txt", delim = "\t", na = "Not Applicable")

require(car)

dta_old %>% 
  slice(1:1674) %>% 
  select(age = `Age Group`, year = Year, sex = `Gender`, race = `Race`, death_count=`Deaths`, population = `Population`) %>% 
  mutate(death_rate = death_count / population) %>% 
  filter(!is.na(death_rate)) %>% 
  
  select(-death_count, -population) %>% 
  mutate(race = recode(
    race, 
    "'Black or African American' = 'black'; 
    'White' = 'white'; 
    'Other Race' = 'other'"
    ), 
    sex = tolower(sex)
    ) %>% 
  arrange(sex, race) %>% 
  ggplot(data = ., aes(y = log(death_rate, 10), x = year, group = age, colour = age)) +
  geom_line() + facet_grid(sex ~ race) + theme_minimal()




# black:white convergence


dta_old %>% 
  slice(1:1674) %>% 
  select(age = `Age Group`, year = Year, sex = `Gender`, race = `Race`, death_count=`Deaths`, population = `Population`) %>% 
  mutate(death_rate = death_count / population) %>% 
  filter(!is.na(death_rate)) %>% 
  
  select(-death_count, -population) %>% 
  mutate(race = recode(
    race, 
    "'Black or African American' = 'black'; 
    'White' = 'white'; 
    'Other Race' = 'other'"
  ), 
  sex = tolower(sex)
  ) %>% 
  filter(race %in% c("white", "black")) %>% 
  spread(key = race, value = death_rate) %>% 
  mutate(ratio = black/white) %>% 
  ggplot(data = ., aes(y = ratio, x = year, group = age, colour = age)) +
  geom_line() + facet_grid(sex ~ .) + theme_minimal()




rr_old <- dta_old %>% 
  slice(1:1674) %>% 
  select(age = `Age Group`, year = Year, sex = `Gender`, race = `Race`, death_count=`Deaths`, population = `Population`) %>% 
  mutate(death_rate = death_count / population) %>% 
  filter(!is.na(death_rate)) %>% 
  
  select(-death_count, -population) %>% 
  mutate(race = recode(
    race, 
    "'Black or African American' = 'black'; 
    'White' = 'white'; 
    'Other Race' = 'other'"
  ), 
  sex = tolower(sex)
  ) %>% 
  filter(race %in% c("white", "black")) %>% 
  spread(key = race, value = death_rate) %>% 
  mutate(ratio = black/white) %>%
  select(-black, -white) %>% 
  group_by(year, sex) %>% 
  summarise(mean_ratio = mean(ratio)) %>% 
  mutate(dta_source = "old")




dta <- read_csv("data/usa_multiple_cause/tidied_and_simplified.csv")

rr_new <- dta  %>% 
  select(year, age, sex, group, all_cause, population) %>% 
  mutate(
    race = recode(
      group,
      "'Black Non-Hispanic' = 'black';
      'White Non-Hispanic' = 'white';
      'Hispanic' = 'hispanic'"
    ),
    sex = tolower(sex)
  ) %>% select(year, age, sex, race, all_cause, population) %>% 
  filter(race %in% c("black", "white")) %>% 
  mutate(death_rate = all_cause / population) %>% 
  select(-all_cause, -population) %>% 
  spread(key = race, value = death_rate) %>% 
  mutate(ratio = black / white) %>% 
  group_by(year, sex) %>% 
  summarise(mean_ratio = mean(ratio)) %>% 
  mutate(dta_source = "new")



rr_combined <- rbind(rr_old, rr_new)


rr_combined %>% unite(col = sexage, sex, dta_source, remove = F) %>% 
ggplot(data = ., aes(y = mean_ratio, x = year, linetype = sex, group = sexage)) +
  geom_line() + theme_minimal() + labs(y = "Mean black/white mortality ratio", x = "Year")


ggsave("figures/case_deaton/mean_black_white_mort_ratio.png", dpi = 150, height = 10, width = 10, units = "cm")





