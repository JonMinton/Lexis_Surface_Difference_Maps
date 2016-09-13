# 12/5/ 2016


# Annotated version of levelplot 

# The purpose of this code is to create an annotated levelplot highlighting different features


# To start with, let's try to add all attributes to all figures 


# Prereqs -----------------------------------------------------------------
.libPaths("E:/R-3.1.2/library")

rm(list=ls())

#install.packages("pacman")
require(pacman)

p_load(
  readr, plyr, stringr, 
  tidyr, dplyr,
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



png(filename="figures/scotland_in_context/final_figures/clp_feature_guide.png",
    width=15, height=15, res=300, units="cm"
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
  mutate(dif_logs = 0) %>% 
  filter(sex == "male", comparison == "Scotland less Rest of WE") %>% 
  levelplot(
    dif_logs ~ year * age ,
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
      panel.polygon(col = "red", alpha = 0.4, 
                    x = c(1950, 1950, 1995, 1995),
                    y = c(90, 0, 0, 90)
                    )
      panel.text(
        x = c(1975, 1975, 1975),
        y = c(10, 35, 65), labels = c("A0", "A1", "A2")
        )
      
      panel.polygon(col = "blue", alpha = 0.4, 
                    x = c(1950, 1960, 2010, 2010),
                    y = c(0, 0, 50, 60)
      )

      panel.polygon(col = "blue", alpha = 0.4, 
                    x = c(1950, 1960, 1962, 1952),
                    y = c(0, 0, 2, 2)
      )
      panel.text(
        x = c(1985, 1957.5), 
        y = c(30, 3.5), 
        labels = c("B", "B0")
        )
      
      panel.polygon(col = "green", alpha = 0.4, 
                    x = c(1995, 2010, 2010, 1995),
                    y = c(20, 20, 50, 35)
      )
      panel.polygon(col = "darkgreen", alpha = 0.4, 
                    x = c(2005, 2010, 2010, 2005),
                    y = c(30, 30, 40, 40)
      )
      
      panel.text(
        x = c(2002, 2008), 
        y = c(30, 35), 
        labels = c("C", "C*")
        )
      
      panel.abline(h = c(20, 50), lty = "dashed")
    }
  )

 dev.off()
