
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


# base data  --------------------------------------------------------------


dta <- read.csv("data/tidy/counts_germany_combined.csv") %>%
  tbl_df


uk_codes <- c(
  "GBRTENW",
  "GBR_NIR",
  "GBR_SCO"
  )

w_europe_codes <- c(
  "AUS",
  "BEL",
  "CHE",
  "DEUT",
  "FRATNP",
  "GBR_NIR",
  "GBR_SCO",
  "GBRTENW",
  "IRL",
  "LUX",
  "NLD"  
  )

dta_uk <- dta %>%
  filter(country %in% uk_codes)
# Cannot add Ireland as records only start in 1950...


tmp <- dta %>% 
  filter(
    country == "IRL" &
    year < 1922       
           ) %>% 
  mutate(country=="GBR_NIR")

dta_uk_smoothed <- dta_uk %>%   
  mutate(
  cmr = death_count / population_count,
  lg_cmr = log(cmr, base=10)     
) %>% smooth_var(
  dta =.,
  group_vars= c("country", "sex"),
  smooth_var = "lg_cmr",
  smooth_par = 1.3
) 
  
dta_we <- dta %>%
  filter(country %in% w_europe_codes)

dta_we_smoothed <- dta_we %>%   
  mutate(
    cmr = death_count / population_count,
    lg_cmr = log(cmr, base=10)     
  ) %>% smooth_var(
    dta =.,
    group_vars= c("country", "sex"),
    smooth_var = "lg_cmr",
    smooth_par = 1.3
  ) 


# Derived data ------------------------------------------------------------

dta_uk_overall <- dta_uk %>%
  group_by(age, sex, year) %>%
  summarise(
    death_count = sum(death_count),
    population_count = sum(population_count)
            ) %>%
  mutate(
    cmr = death_count / population_count,
    lg_cmr = log(cmr, base=10)     
         )

dta_uk_overall_smoothed <- smooth_var(
  dta = dta_uk_overall,
  group_vars= "sex",
  smooth_var = "lg_cmr",
  smooth_par = 1.3
) 


dta_we_overall <- dta_w_europe %>%
  group_by(age, sex, year) %>%
  summarise(
    death_count = sum(death_count),
    population_count = sum(population_count)
    ) %>%
  mutate(
    cmr = death_count / population_count,
    lg_cmr = log(cmr, base=10)
    )

dta_we_overall_smoothed <- smooth_var(
  dta = dta_we_overall,
  group_vars= "sex",
  smooth_var = "lg_cmr",
  smooth_par = 1.3
)

# SCP - pop - Scot only -----------------------------------------------------------

dta_scot_only <- dta %>% 
  filter(country=="GBR_SCO" & age <= 90) %>% 
  mutate(
    cmr = death_count / population_count,
    lg_cmr = log(cmr, base=10)     
  )

dta_all_smoothed <- dta%>% 
  filter(sex!="total") %>% 
  mutate(
    cmr = death_count / population_count,
    lg_cmr = log(cmr, base=10)
  ) %>% 
  smooth_var(dta=., group_vars = c("sex", "country"),
             smooth_var= "lg_cmr",
             smooth_par=1.3)



png(filename="figures/scotland_in_context/scotland_scp.png", 
    width=40, height=20, res=300, units="cm"
)
# Let's look at the mort rates only

shade_part <- dta_uk %>%
  filter(
    country == "GBR_SCO" & 
      year >= 1900 & year <= 2010 &
      age <= 90 &
      sex != "total"
  ) %>%
  mutate(
    cmr = death_count / population_count,
    ln_cmr = log(cmr)
  ) %>%
  levelplot(
    ln_cmr ~ year * age | sex, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions=colorRampPalette(brewer.pal(6, "Reds"))(200),
    main=NULL,
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    par.settings=list(strip.background=list(col="lightgrey"))
  )

contour_part <- dta_uk_smoothed  %>%  
  filter(
    country == "GBR_SCO" & 
    year >= 1900 & year <= 2008 &
    age <= 90 
  ) %>%
  contourplot(
    lg_cmr ~ year + age | sex, 
    data=.,
    region=F,
    ylab="",
    xlab="",
    scales=list(NULL),
    cuts=25,
    col="blue",
    labels=list(
      cex=1.2
    ),
    main=NULL
  )

print(shade_part + contour_part)

dev.off()

# Eng wales only


png(filename="figures/scotland_in_context/england_wales_scp.png", 
    width=40, height=20, res=300, units="cm"
)
# Let's look at the mort rates only

shade_part <- dta_uk %>%
  filter(
    country == "GBRTENW" & 
      year >= 1900 & year <= 2010 &
      age <= 90 &
      sex != "total"
  ) %>%
  mutate(
    cmr = death_count / population_count,
    ln_cmr = log(cmr)
  ) %>%
  levelplot(
    ln_cmr ~ year * age | sex, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions=colorRampPalette(brewer.pal(6, "Reds"))(200),
    main=NULL,
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    par.settings=list(strip.background=list(col="lightgrey"))
  )

contour_part <- dta_uk_smoothed  %>%  
  filter(
    country ==  "GBRTENW" & 
      year >= 1900 & year <= 2008 &
      age <= 90 
  ) %>%
  contourplot(
    ln_cmr ~ year + age | sex, 
    data=.,
    region=F,
    ylab="",
    xlab="",
    scales=list(NULL),
    cuts=25,
    col="blue",
    labels=list(
      cex=1.2
    ),
    main=NULL
  )

print(shade_part + contour_part)

dev.off()

png(filename="figures/scotland_in_context/northern_ireland_scp.png", 
    width=40, height=20, res=300, units="cm"
)
shade_part <- dta_uk %>%
  filter(
    country == "GBR_NIR" & 
      year >= 1900 & year <= 2010 &
      age <= 90 &
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
    col.regions=colorRampPalette(brewer.pal(6, "Reds"))(200),
    main=NULL,
    xlim=c(1900, 2010),
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    par.settings=list(strip.background=list(col="lightgrey"))
  )

contour_part <- dta_uk_smoothed  %>%  
  filter(
    country ==  "GBR_NIR" & 
      year >= 1924 & year <= 2008 &
      age <= 90 
  ) %>%
  contourplot(
    lg_cmr ~ year + age | sex, 
    data=.,
    region=F,
    ylab="",
    xlab="",
    scales=list(NULL),
    cuts=25,
    xlim=c(1900, 2010),

    col="blue",
    labels=list(
      cex=1.2
    ),
    main=NULL
  )

print(shade_part + contour_part)

dev.off()

# SCP UK ------------------------------------------------------------------

png(filename="figures/scotland_in_context/uk_overall_scp.png", 
    width=40, height=20, res=300, units="cm"
)
shade_part <- dta_uk_overall %>%
  filter(
      year >= 1900 & year <= 2010 &
      age <= 90 &
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
    col.regions=colorRampPalette(brewer.pal(6, "Reds"))(200),
    main=NULL,
    xlim=c(1900, 2010),
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    par.settings=list(strip.background=list(col="lightgrey"))
  )

contour_part <- dta_uk_overall_smoothed  %>%  
  filter(
      year >= 1900 & year <= 2008 &
      age <= 90 
  ) %>%
  contourplot(
    lg_cmr ~ year + age | sex, 
    data=.,
    region=F,
    ylab="",
    xlab="",
    scales=list(NULL),
    cuts=25,
    col="blue",
    labels=list(
      cex=1.2
    ),
    main=NULL
  )

print(shade_part + contour_part)

dev.off()

 

# SCP rUK overall -----------------------------------------------------------------


# SCP Countries in rUK ----------------------------------------------------
png(filename="figures/scotland_in_context/uk_countries_scp.png", 
    width=30, height=30, res=300, units="cm"
)

shade_part <- dta_uk %>%
  filter(
      year >= 1900 & year <= 2010 &
      age <= 90 &
      sex != "total"
  ) %>%
  mutate(
    cmr = death_count / population_count,
    lg_cmr = log(cmr, base=10),
    country = mapvalues(
      country,
      from=c("GBR_NIR", "GBR_SCO", "GBRTENW"),
      to=c("Northern Ireland", "Scotland", "England & Wales")
      )
  ) %>%
  levelplot(
    lg_cmr ~ year * age | country + sex, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    col.regions=colorRampPalette(brewer.pal(6, "Reds"))(200),
    main=NULL,
    xlim=c(1900, 2010),
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    par.settings=list(strip.background=list(col="lightgrey"))
  )

contour_part <- dta_uk_smoothed  %>%  
  filter(
      year >= 1900 & year <= 2008 &
      age <= 90
  ) %>%
  contourplot(
    lg_cmr ~ year + age | country + sex, 
    data=.,
    region=F,
    ylab="",
    xlab="",
    scales=list(NULL),
    cuts=25,    
    col="blue",
    labels=list(
      cex=1.2
    ),
    main=NULL
  )

print(shade_part + contour_part)

dev.off()

# CLP Scot in UK ----------------------------------------------------------

tmp1 <- dta_uk  %>% 
  filter(country=="GBR_SCO")  %>% 
  mutate(cmr = death_count/ population_count)  %>% 
  select(country, year, age, sex, cmr)

tmp2 <- dta_uk_overall  %>% 
  mutate(country="UK")  %>%  
  select(country, year, age, sex, cmr)

tmp3 <- bind_rows(tmp1, tmp2)
rm(tmp1, tmp2)

tmp3

dif_scot_uk  <- tmp3 %>% 
  mutate(lg_cmr = log(cmr, base=10))  %>% 
  select(-cmr)  %>% 
  spread(key=country, value=lg_cmr)  %>% 
  filter(!is.na(GBR_SCO))  %>% 
  mutate(dif = GBR_SCO - UK)  %>% 
  select(year, age, sex, dif)

lev_part <- dif_scot_uk %>% filter(
  sex!="total"
  & age <=90 &
    year >=1900
  ) %>% 
  levelplot(
    dif ~ year * age | sex,
      data=., 
      region=T,
      ylab="Age in years",
      xlab="Year",
      at = seq(from= -1.2, to = 1.2, by=0.2),
      col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
      scales=list(alternating=3),
      main=NULL,
      par.settings=list(strip.background=list(col="lightgrey"))
  )

dif_blurred <- dif_scot_uk %>% smooth_var(
  dta=.,
  group_vars= "sex",
  smooth_var="dif",
  smooth_par=1.5
)


ident_part <- dif_blurred %>%
  filter(sex!="total" & age <=90) %>%
  contourplot(
    dif ~ year + age | sex, 
    data=.,
    region=F,
    ylab="",
    xlab="",
    scales=list(NULL),
    at=0,
    labels=F,
    main=NULL
  )

print(lev_part + ident_part)


# SCP Western Europe overall------------------------------------------------------


# SCP countries in Western Europe -----------------------------------------

# CLP Scot in Western Europe ----------------------------------------------

tmp1 <- dta_we_overall  %>% 
  mutate(country="Western_Europe")  %>% 
  select(country, sex, age, year, lg_cmr)

tmp2 <- dta_we  %>% 
  filter(country=="GBR_SCO")  %>% 
  mutate(lg_cmr = log(death_count/population_count, base=10))  %>% 
  select(country, sex, age, year, lg_cmr)

tmp3 <- bind_rows(tmp1, tmp2) 
rm(tmp1, tmp2)

scot_in_we <- tmp3  %>% 
  spread(key=country, value=lg_cmr)  %>% 
  filter(!is.na(GBR_SCO))  %>% 
  mutate(dif = GBR_SCO - Western_Europe)  %>% 
  select(sex, age, year, dif)

rm(tmp3)

lev_part <- scot_in_we %>% filter(
    sex!="total"
    & age <=90 &
      year >=1900
  ) %>% 
  levelplot(
    dif ~ year * age | sex,
    data=., 
    region=T,
    ylab="Age in years",
    xlab="Year",
    at = seq(from= -1.2, to = 1.2, by=0.2),
    col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
    scales=list(alternating=3),
    main=NULL,
    par.settings=list(strip.background=list(col="lightgrey"))
  )

dif_blurred <- scot_in_we %>% smooth_var(
  dta=.,
  group_vars= "sex",
  smooth_var="dif",
  smooth_par=1.4
)


ident_part <- dif_blurred %>%
  filter(sex!="total" & age <=90) %>%
  contourplot(
    dif ~ year + age | sex, 
    data=.,
    region=F,
    ylab="",
    xlab="",
    scales=list(NULL),
    at=0,
    labels=F,
    main=NULL
  )

print(lev_part + ident_part)

