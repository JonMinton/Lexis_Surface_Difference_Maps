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


uk_codes <- c(
  `England & Wales`= "GBRTENW",
  `Northern Ireland` = "GBR_NIR",
  `Scotland`= "GBR_SCO"
  )

w_europe_codes <- c(
  Austria = "AUT",
  Belgium = "BEL",
  Switzerland = "CHE",
  Germany = "DEUT",
  France = "FRATNP",
  `Northern Ireland` = "GBR_NIR",
  Scotland = "GBR_SCO",
  `England & Wales` = "GBRTENW",
  Ireland = "IRL",
  Luxembourg = "LUX",
  Netherlands = "NLD"  
  )

europe_codes <- c(
  Austria = "AUT",
  Belgium = "BEL",
  Switzerland = "CHE",
  Germany = "DEUT",
  France = "FRATNP",
  `Northern Ireland` = "GBR_NIR",
  Scotland = "GBR_SCO",
  `England & Wales` = "GBRTENW",
  Ireland = "IRL",
  Luxembourg = "LUX",
  Netherlands = "NLD",  
  Bulgaria = "BGR",
  Belarus = "BLR",
  `Czech Republic` = "CZE",
  Denmark = "DNK",
  Spain = "ESP",
  Estonia = "EST", 
  Finland = "FIN",
  Hungary = "HUN",
  Iceland = "ISL",
  Italy = "ITA",
  Lithuania = "LTU",
  Latvia = "LVA",
  Norway = "NOR",
  Poland = "POL",
  Portugal = "PRT",
  Slovakia = "SVK",
  Skovenia = "SVN",
  Sweden = "SWE",
  Ukraine = "UKR"
)

all_codes <- c(
  Austria = "AUT",
  Belgium = "BEL",
  Switzerland = "CHE",
  Germany = "DEUT",
  France = "FRATNP",
  `Northern Ireland` = "GBR_NIR",
  Scotland = "GBR_SCO",
  `England & Wales` = "GBRTENW",
  Ireland = "IRL",
  Luxembourg = "LUX",
  Netherlands = "NLD",  
  Bulgaria = "BGR",
  Belarus = "BLR",
  `Czech Republic` = "CZE",
  Denmark = "DNK",
  Spain = "ESP",
  Estonia = "EST", 
  Finland = "FIN",
  Hungary = "HUN",
  Iceland = "ISL",
  Italy = "ITA",
  Lithuania = "LTU",
  Latvia = "LVA",
  Norway = "NOR",
  Poland = "POL",
  Portugal = "PRT",
  Slovakia = "SVK",
  Sweden = "SWE",
  Ukraine = "UKR",
  Australia = "AUS",
  Isreal = "ISR", 
  Japan = "JPN",
  `New Zealand` = "NZL_NP",
  Russia = "RUS",
  Taiwan = "TWN",
  USA = "USA"
)



# country_group_selections ------------------------------------------------

dta_uk <- dta %>% filter(country %in% uk_codes)
dta_we <- dta %>% filter(country %in% w_europe_codes)
dta_europe <- dta %>% filter(country %in% europe_codes)
dta_all <- dta %>% filter(country %in% all_codes)


# country group selections - smoothed -------------------------------------


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

dta_europe_smoothed <- dta_europe %>% 
  mutate(
    cmr = death_count / population_count,
    lg_cmr = log(cmr, base=10)     
  ) %>% smooth_var(
    dta =.,
    group_vars= c("country", "sex"),
    smooth_var = "lg_cmr",
    smooth_par = 1.3
  ) 

dta_all_smoothed <- dta_all %>% 
  mutate(
    cmr = death_count / population_count,
    lg_cmr = log(cmr, base=10)     
  ) %>% smooth_var(
    dta =.,
    group_vars= c("country", "sex"),
    smooth_var = "lg_cmr",
    smooth_par = 1.3
  ) 



# country group selections - combined  ---------------------------------------



dta_uk_overall <- grouper(dta_uk)
dta_we_overall <- grouper(dta_we)
dta_europe_overall <- grouper(dta_europe)
dta_all_overall <- grouper(dta_all)


# country group selections - combined and smoothed -------------------------------


dta_uk_overall_smoothed <- smooth_var(
  dta_uk_overall,   
  group_vars= "sex", smooth_var = "lg_cmr", smooth_par = 1.3)

dta_we_overall_smoothed <- smooth_var(
  dta_we_overall,   
  group_vars= "sex", smooth_var = "lg_cmr", smooth_par = 1.3)

dta_europe_overall_smoothed <- smooth_var(
  dta_europe_overall,   
  group_vars= "sex", smooth_var = "lg_cmr", smooth_par = 1.3)

dta_all_overall_smoothed <- smooth_var(
  dta_all_overall,   
  group_vars= "sex", smooth_var = "lg_cmr", smooth_par = 1.3)



# Shaded contour plots of individual countries ----------------------------


# SCP of Scotland ---------------------------------------------------------

png(filename="figures/scotland_in_context/scotland_scp.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_scp(dta_uk, dta_uk_smoothed, "GBR_SCO"))
dev.off()

# SCP of England & Wales --------------------------------------------------

png(filename="figures/scotland_in_context/england_wales_scp.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_scp(dta_uk, dta_uk_smoothed, "GBRTENW"))
dev.off()

# SCP of Northern Ireland

png(filename="figures/scotland_in_context/northern_ireland_scp.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_scp(dta_uk, dta_uk_smoothed, "GBR_NIR"))
dev.off()

# SCPs of aggregated groups of countries 

# SCP of UK Overall -------------------------------------------------------

png(filename="figures/scotland_in_context/uk_overall_scp.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_scp_overall(dta_uk_overall, dta_uk_overall_smoothed))
dev.off()

# SCP of Western Europe Overall

png(filename="figures/scotland_in_context/we_overall_scp.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_scp_overall(dta_we_overall, dta_we_overall_smoothed))
dev.off()

# SCP of Europe Overall
png(filename="figures/scotland_in_context/europe_overall_scp.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_scp_overall(dta_europe_overall, dta_europe_overall_smoothed))
dev.off()

# SCP of all data Overall

png(filename="figures/scotland_in_context/allcountries_overall_scp.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_scp_overall(dta_all_overall, dta_all_overall_smoothed))
dev.off()




# Latticeplots ------------------------------------------------------------


# Latticeplot of UK countries ---------------------------------------------



# UK countries SCP latticeplot

png(filename="figures/scotland_in_context/uk_countries_scp.png", 
    width=30, height=30, res=300, units="cm"
)

print(make_scp_lattice(dta_uk, dta_uk_smoothed, uk_codes))

dev.off()

# Western Europe SCP Latticeplot
png(filename="figures/scotland_in_context/we_countries_scp.png", 
    width=110, height=30, res=300, units="cm"
)
print(make_scp_lattice(dta_we, dta_we_smoothed, w_europe_codes))
dev.off()

# All Europe SCP latticeplot
png(filename="figures/scotland_in_context/european_countries_scp.png", 
    width=300, height=30, res=300, units="cm"
)
print(make_scp_lattice(dta_europe, dta_europe_smoothed, europe_codes))
dev.off()


#CLPs of single countries 



# CLP Scot scot_against_uk ----------------------------------------------------------

png(filename="figures/scotland_in_context/clp_scotland_against_UK.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_single_clp(dta_uk, dta_uk_overall, "GBR_SCO"))
dev.off()


# CLP Scotland against Western Europe -------------------------------------

png(filename="figures/scotland_in_context/clp_scotland_against_western_europe.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_single_clp(dta_we, dta_we_overall, "GBR_SCO"))
dev.off()

# CLP Scotland against Europe -------------------------------------
png(filename="figures/scotland_in_context/clp_scotland_against_europe.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_single_clp(dta_europe, dta_europe_overall, "GBR_SCO"))
dev.off()

# CLP Scotland against Affluent World -------------------------------------
png(filename="figures/scotland_in_context/clp_scotland_against_affluent_world.png", 
    width=40, height=20, res=300, units="cm"
)
print(make_single_clp(dta_all, dta_all_overall, "GBR_SCO"))
dev.off()

# latticeplot of SCPs of each Western European Country --------------------





shade_part <- dta_we %>%
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
      from=c("AUS", "BEL", "CHE", "DEUT", "FRATNP", "GBR_NIR", "GBR_SCO", "GBRTENW", "IRL", "LUX", "NLD"),
      to=c("Austria", "Belgium", "Switzerland", "Germany", "France", "Northern Ireland", "Scotland", "England & Wales", "Ireland", "Luxemboug", "Holland")
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

contour_part <- dta_we_smoothed  %>%  
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
    col="black",
    labels=list(
      cex=1.2
    ),
    xlim=c(1900, 2010),
    main=NULL
  )

print(shade_part + contour_part)

dev.off()



# Lattice CLPs  ----------------------------------



# Lattice CLP - UK level --------------------------------------------------
png(filename="figures/scotland_in_context/clp_lattice_uk.png",
    width=50, height=30, res=300, units="cm"
)
print(make_clp_lattice(dta_uk, dta_uk_overall, uk_codes))
dev.off()


# Lattice CLP - Western Europe Level --------------------------------------

png(filename="figures/scotland_in_context/clp_lattice_western_europe.png",
    width=120, height=30, res=300, units="cm"
)
  print(make_clp_lattice(dta_we, dta_we_overall, w_europe_codes))
dev.off()

# Lattice CLP - Whole of Europe Level 

png(filename="figures/scotland_in_context/clp_lattice_all_europe.png",
    width=300, height=30, res=300, units="cm"
)
print(make_clp_lattice(dta_europe, dta_europe_overall, europe_codes))
dev.off()


# SCPs of each country, individually --------------------------------------


fn <- function(this_country){
  
  this_country_name = names(w_europe_codes[w_europe_codes==this_country])
  
  this_unsmoothed <- dta_we %>% 
    filter(
      country==this_country &
        sex !="total" &
     year >=1900 &
       age <=90
    ) %>% 
  levelplot(
    lg_cmr ~ year * age | sex,
    data = . ,
    region = T , 
    par.strip.text=list(cex=1.4, fontface="bold"),
    par.settings=list(strip.background=list(col="lightgrey")),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    xlim=c(1900, 2010), 
    cex=1.4,
    cuts=25,
    col.regions=colorRampPalette(brewer.pal(6, "RdYlBu"))(200),
    main=this_country_name,
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
  
  this_smoothed <- dta_we_smoothed %>% 
    filter(
      country == this_country &
        sex !="total" &
        year >=1900 &
        age <=90
    ) %>% 
    contourplot(
      lg_cmr ~ year * age | sex,
      data = . ,
      region = F ,
      cex=1.4,
      cuts=25,
      col.regions=colorRampPalette(brewer.pal(6, "RdYlBu"))(200),
      scales=list(
        x=NULL, y=NULL
      ),
      xlim=c(1900, 2010), 
      xlab=NULL, ylab=NULL, 
      main=NULL,
      labels=list(cex=1.2),
      col="darkgreen"
    )
  
  
  png(
    filename=paste0(
    "figures/scotland_in_context/all_countries/cmr_",
    this_country, ".png"
    ),
      width=30, height=15, res=300, units="cm"
  )
  print(this_unsmoothed + this_smoothed)
  
  dev.off()  
    
  return(NULL)
  
}

l_ply(w_europe_codes, fn)



# Suggestions from Gerry's manuscript -------------------------------------


# Scotland vs England & Wales, 45 degree line, Longest available p --------




make_single_clp <- function(DTA, DTA_overall, SELECTION){
  
  tmp1 <- DTA  %>% 
    filter(country == SELECTION)  %>% 
    mutate(cmr = death_count/ population_count)  %>% 
    mutate(country = "specific") %>% 
    select(country, year, age, sex, cmr)
  
  tmp2 <- DTA_overall  %>% 
    mutate(country = "overall" )  %>%  
    select(country, year, age, sex, cmr)
  
  tmp3 <- bind_rows(tmp1, tmp2)
  rm(tmp1, tmp2)
  
  dif_to_overall  <- tmp3 %>% 
    mutate(lg_cmr = log(cmr, base=10))  %>% 
    select(-cmr)  %>% 
    spread(key=country, value=lg_cmr)  %>% 
    filter(!is.na(specific))  %>% 
    mutate(dif = specific  - overall) %>% 
    select(year, age, sex, dif)
  
  lev_part <- dif_to_overall %>% filter(
    sex!="total"
    & age <=90 &
      year >=1900
  ) %>% 
    levelplot(
      dif ~ year * age | sex,
      data=., 
      region=T,
      xlim=c(1900, 2010),
      ylab="Age in years",
      xlab="Year",
      at = seq(from= -1.2, to = 1.2, by=0.2),
      col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
      scales=list(alternating=3),
      main=NULL,
      par.settings=list(strip.background=list(col="lightgrey"))
    )
  
  dif_blurred <- dif_to_overall %>% smooth_var(
    dta=.,
    group_vars= "sex",
    smooth_var="dif",
    smooth_par=1.4
  )
  
  
  zero_part <- dif_blurred %>%
    filter(sex!="total" & age <=90) %>%
    contourplot(
      dif ~ year + age | sex, 
      data=.,
      region=F,
      ylab="",
      xlab="",
      scales=list(NULL),
      at=0,
      lwd=1,
      labels=F,
      xlim=c(1900, 2010),
      main=NULL
    )
  
  quarter_part <- dif_blurred %>% 
    filter(sex != "total" & age <=90) %>% 
    contourplot(
      dif ~ year + age | sex, 
      data = . , 
      region = F,
      ylab = "", 
      xlab = "", 
      scales = list (NULL),
      at = c(-0.25, 0.25),
      lwd=1.5, 
      labels = F,
      xlim=c(1900, 2010),
      main = NULL
    )
  
  half_part <- dif_blurred %>% 
    filter(sex!="total" & age <=90) %>% 
    contourplot(
      dif ~ year + age | sex,
      data = . ,
      region = F,
      ylab="", 
      xlab="",
      scales = list (NULL),
      at =c(-0.5, 0.5),
      lwd=2.0,
      labels =F,
      xlim=c(1900, 2010),
      main=NULL
    )
  
  output <- lev_part + zero_part + quarter_part + half_part
  
  return(output)
}



# •	Contour plots comparing Scotland 
# ( for the longest time period available, all-cause mortality, 
#     using the same scale for period and age so that birth cohort effects are 45 degree lines if possible) 
# with: 
#   o	England & Wales
# o	Northern Ireland
# o	Republic of Ireland (all above because of the similarities in history/culture/politics)
# o	USA (there is a strong suggestion from other work that there are similarities between the USA and Scotland in terms of the mortality phenomena – something we have started to explore but which has been delayed for a long time now). 
# 
# o	Western/central European average (to highlight the divergence and where this occurs) – 
# would include as many countries as possible in this – I presume we weight the countries by population size or have a numerator for each death and denominator for each individual alive for each age/time point to avoid smaller countries have too many influence?] 
# o	Eastern European average (to see if the divergence has similarities to that in this region) 
# – would be interesting to split former USSR with non-USSR Eastern Bloc countries as I imagine Scotland will be closer to the latter 
# 
