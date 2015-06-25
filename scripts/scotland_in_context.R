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


# country_group_selections ------------------------------------------------

dta_uk <- dta %>% filter(country %in% uk_codes)
dta_we <- dta %>% filter(country %in% w_europe_codes)
dta_europe <- dta %>% filter(country %in% europe_codes)
dta_all <- dta %>% filter(country %in% all_codes)
dta_ne <- dta %>% filter(country %in% europe_northern)
dta_se <- dta %>% filter(country %in% europe_southern)
dta_ee <- dta %>% filter(country %in% europe_eastern)

# country group selections - smoothed -------------------------------------

fn <- function(DTA) {
  out <- DTA %>%   
    mutate(
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base=10)     
    ) %>% smooth_var(
      dta =.,
      group_vars= c("country", "sex"),
      smooth_var = "lg_cmr",
      smooth_par = 1.3
    ) 
  return(out)
}


dta_uk_smoothed <- fn(dta_uk)
dta_we_smoothed <- fn(dta_we)
dta_europe_smoothed <- fn(dta_europe)
dta_all_smoothed <- fn(dta_all)
dta_ne_smoothed <- fn(dta_ne)
dta_se_smoothed <- fn(dta_se)
dta_ee_smoothed <- fn(dta_ee)

rm(fn)
# country group selections - combined  ---------------------------------------


dta_uk_overall <- grouper(dta_uk)
dta_we_overall <- grouper(dta_we)
dta_europe_overall <- grouper(dta_europe)
dta_all_overall <- grouper(dta_all)
dta_ne_overall <- grouper(dta_ne)
dta_se_overall <- grouper(dta_se)
dta_ee_overall <- grouper(dta_ee)

# Data allowing comparison of broad European regions against European average

tmp1 <- dta_we_overall %>% mutate(country="Western Europe")
tmp2 <- dta_ee_overall %>% mutate(country="Eastern Europe")
tmp3 <- dta_ne_overall %>% mutate(country="Northern Europe")
tmp4 <- dta_se_overall %>% mutate(country="Southern Europe")
tmp5 <- dta_europe_overall %>% mutate(country = "Europe Overall")

dta_euro_regions <- tmp1 %>% bind_rows(tmp2) %>% bind_rows(tmp3) %>% bind_rows(tmp4) %>% bind_rows(tmp5)
  
dta_euro_regions_smoothed <- dta_euro_regions %>% smooth_var(dta=., group_vars = c("sex", "country"), smooth_var="lg_cmr", smooth_par=1.3)

rm(tmp1, tmp2, tmp3, tmp4, tmp5)

fn <- function(DTA) {
  out <- DTA %>%   
    mutate(
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base=10)     
    ) %>% smooth_var(
      dta =.,
      group_vars= c("sex"),
      smooth_var = "lg_cmr",
      smooth_par = 1.3
    ) 
  return(out)
}

dta_uk_overall_smoothed <- fn(dta_uk_overall)
dta_we_overall_smoothed <- fn(dta_we_overall)
dta_europe_overall_smoothed <- fn(dta_europe_overall)
dta_all_overall_smoothed <- fn(dta_all_overall)
dta_ne_overall_smoothed <- fn(dta_ne_overall)
dta_se_overall_smoothed <- fn(dta_se_overall)
dta_ee_overall_smoothed <- fn(dta_ee_overall)

rm(fn)


# country group selections - combined and smoothed -------------------------------




# Shaded contour plots of individual countries ----------------------------


# SCP of Scotland ---------------------------------------------------------

png(filename="figures/scotland_in_context/scotland_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp(dta_uk, dta_uk_smoothed, "GBR_SCO"))
dev.off()

# SCP of England & Wales --------------------------------------------------

png(filename="figures/scotland_in_context/england_wales_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp(dta_uk, dta_uk_smoothed, "GBRTENW"))
dev.off()

# SCP of Northern Ireland

png(filename="figures/scotland_in_context/northern_ireland_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp(dta_uk, dta_uk_smoothed, "GBR_NIR"))
dev.off()

# SCPs of aggregated groups of countries 

# SCP of UK Overall -------------------------------------------------------

png(filename="figures/scotland_in_context/uk_overall_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp_overall(dta_uk_overall, dta_uk_overall_smoothed))
dev.off()

# SCP of Western Europe Overall

png(filename="figures/scotland_in_context/we_overall_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp_overall(dta_we_overall, dta_we_overall_smoothed))
dev.off()

# SCP of Europe Overall
png(filename="figures/scotland_in_context/europe_overall_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp_overall(dta_europe_overall, dta_europe_overall_smoothed))
dev.off()

# SCP of Northern Europe
png(filename="figures/scotland_in_context/northern_europe_overall_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp_overall(dta_ne_overall, dta_ne_overall_smoothed))
dev.off()

# SCP of Eastern Europe
png(filename="figures/scotland_in_context/southern_europe_overall_scp_iso.png", 
    width=25, height=25, res=300, units="cm"
)
print(make_scp_overall(dta_ee_overall, dta_ee_overall_smoothed, YEAR_RANGE=c(1950, 2010)))
dev.off()


# SCP of Southern Europe
png(filename="figures/scotland_in_context/southern_europe_overall_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
)
print(make_scp_overall(dta_se_overall, dta_se_overall_smoothed, YEAR_RANGE=c(1900, 2010)))
dev.off()



# SCP of all data Overall

png(filename="figures/scotland_in_context/allcountries_overall_scp_iso.png", 
    width=20, height=40, res=300, units="cm"
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



# Region level SCP latticeplot  -------------------------------------------

tmp1 <- c(
  `Western Europe`="Western Europe",
  `Eastern Europe` = "Eastern Europe",
  `Northern Europe` = "Northern Europe",
  `Southern Europe` = "Southern Europe"
)

tmp2 <- dta_euro_regions %>% filter(country !="Europe Overall")
tmp3 <- dta_euro_regions_smoothed %>% filter(country !="Europe Overall")

png(filename="figures/scotland_in_context/euro_regions_scp_lattice_spectral.png", 
    width=70, height=40, res=300, units="cm"
)
make_scp_lattice(tmp2, tmp3, tmp1, 
                 COL.REGIONS=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200))
                 )
dev.off()


# Create CLP comparing Scotland against each of the main European regions


tmp3 <- dta %>% filter(country == "GBR_SCO") %>% select(-country) %>% 
  mutate(cmr = death_count/population_count, lg_cmr = log(cmr, base = 10))

png(filename="figures/scotland_in_context/scotland_compared_with_euro_regions_clp_lattice_1900_2010.png",
    width=60, height=30, res=300, units="cm"
)
print(make_clp_lattice(tmp2, tmp3, tmp1, 
                       COL.REGIONS= colorRampPalette(brewer.pal(6, "RdBu"))(64))
)
dev.off()

png(filename="figures/scotland_in_context/scotland_compared_with_euro_regions_clp_lattice_1950_2010.png",
    width=30, height=30, res=300, units="cm"
)
print(make_clp_lattice(tmp2, tmp3, tmp1, YEAR_RANGE=c(1950, 2010),
                       COL.REGIONS= colorRampPalette(brewer.pal(6, "RdBu"))(64))
)
dev.off()

#CLPs of single countries 

#Lattice plot of 

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


# Additional from Gerry ---------------------------------------------------

# Scotland against England & Wales Only, isometric ------------------------

png(filename="figures/scotland_in_context/clp_scotland_minus_england_iso.png",
    height=30,width=30, res=300, units="cm"
)
print(make_two_country_clp(DTA=dta, "GBRTENW", "GBR_SCO",
                            YEAR_RANGE = c(1855, 2011)))
dev.off()


# Scotland against Northern Ireland only, isometric -----------------------
png(filename="figures/scotland_in_context/clp_scotland_minus_northern_ireland_iso.png",
    height=20,width=30, res=300, units="cm"
)

print(make_two_country_clp(DTA=dta, "GBR_NIR", "GBR_SCO",
                           YEAR_RANGE = c(1922, 2011)))

dev.off()


# Scotland against Republic of Ireland Only, isometric --------------------
png(filename="figures/scotland_in_context/clp_scotland_minus_ireland_iso.png",
    height=25, width=25, res=300, units="cm"
    )
print(make_two_country_clp(DTA=dta, "IRL", "GBR_SCO",
                           YEAR_RANGE = c(1950, 2009)))
dev.off()


# Scotland against USA only, isometric
png(filename="figures/scotland_in_context/clp_scotland_minus_usa_iso.png",
    height=25, width=25, res=300, units="cm"
)

print(make_two_country_clp(DTA=dta, "USA", "GBR_SCO",
                           YEAR_RANGE = c(1933, 2010)))

dev.off()

# Scotland against Russia only, isometric
png(filename="figures/scotland_in_context/clp_scotland_minus_russia_iso.png",
    height=25, width=25, res=300, units="cm"
)

print(make_two_country_clp(DTA=dta, "RUS", "GBR_SCO",
                           YEAR_RANGE = c(1959, 2010)))

dev.off()

# latticeplot of SCPs of each Western European Country --------------------

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



# Similarity scores -------------------------------------------------------


# The idea of this section is to try to rank countries in terms of how similar they are
# in different years, compared with Scotland.
# Similarity is defined as closeness to Scotland's age-and-sex-specific mortality rates 
# in each year. 

# We will need dta_all and all_codes

fn <- function(CODE){
  dta_other <- dta_all %>% 
    filter(country == CODE) %>% 
    mutate(cmr_other = death_count / population_count) %>% 
    select(year, age, sex, cmr_other)
  
  dta_scot <- dta_all %>% 
    filter(country == "GBR_SCO") %>% 
    mutate(cmr_scot = death_count/population_count) %>% 
    select(year, age, sex, cmr_scot)
  
  dta_joined <- inner_join(dta_other, dta_scot)
  
  output <- dta_joined %>% 
    filter(age <= 90) %>% 
    mutate(cmr_dif = cmr_scot - cmr_other) %>% 
    select(year, age, sex, cmr_dif) %>% 
    group_by(year) %>% 
    summarise(avg_cmr_dif = mean(cmr_dif)) %>% 
    mutate(country = CODE) %>% 
    select(country, year, avg_cmr_dif)
  
  return(output)
}

codes_minus_scot <-  all_codes[all_codes!="GBR_SCO"]
all_difs <- ldply(codes_minus_scot, fn, .id="country_name") %>% tbl_df

all_difs$region <- NA
all_difs$region[all_difs$country %in% europe_eastern] <- "Eastern Europe"
all_difs$region[all_difs$country %in% europe_northern] <- "Northern Europe"
all_difs$region[all_difs$country %in% europe_southern] <- "Southern Europe"
all_difs$region[all_difs$country %in% europe_western] <- "Western Europe"
all_difs$region[all_difs$country %in% uk_codes] <- "Rest of UK"

tmp <- all_difs %>% group_by(country) %>% filter(year == max(year) & !is.na(region))
tmp2 <- all_difs %>% group_by(country) %>% filter(year == max(1950, min(year)) & !is.na(region))
all_difs %>% filter(year >= 1950 & !is.na(region)) %>% ggplot() +
  geom_line(aes(x=year, y= avg_cmr_dif, group=country)) +
  facet_wrap( ~ region) + geom_hline(aes(yintercept = 0)) + 
  labs(x="Year", y="Average difference in age and\nsex specific mortality rate") +
  geom_text(aes(x=year + 3.5, y=avg_cmr_dif, group=country, label=country), size = 3, 
            data = tmp) + 
  geom_text(aes(x=year - 3.5 , y=avg_cmr_dif, group=country, label=country), size = 3, 
            data = tmp2)

ggsave(filename = "figures/scotland_in_context/differences_over_time.png",
       dpi=300, height=20, width=30, units = "cm"
       )

