# Code for figs for EJE paper


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


# Load and tidy sources ---------------------------------------------------


# load counts with east and west germany combined 

country_codes <- read.csv("data/tidy/country_codes__new.csv", stringsAsFactors=F) %>%
  tbl_df

europe_codes <- country_codes$short[country_codes$europe==1]
counts <- read.csv("data/tidy/counts_germany_combined.csv")

counts_eu <- counts  %>% filter(country %in% europe_codes) %>%
  tbl_df

# GDP scores

gdp <- read.csv("data/gdp/gdp_tidy.csv") %>%
  tbl_df


# Produce aggregate mortality rates ---------------------------------------


mrate_aggregated <- counts_eu %>%
  group_by(sex, year, age) %>%
  summarise(
    n_countries=length(death_count),
    death_count=sum(death_count),
    population_count=sum(population_count)
    ) %>%
  mutate(death_rate_overall=death_count/population_count, 
         ldeath_rate_overall = log(death_rate_overall),
         n=population_count) %>%
  select(sex, year, age, n_countries, death_rate_overall, ldeath_rate_overall, n)

mrate_each_country <- counts_eu %>%
  mutate(death_rate_specific=death_count/population_count, 
         ldeath_rate_specific=log(death_rate_specific),
         fr=population_count) %>%
  select(country, year, age, sex, death_rate_specific, ldeath_rate_specific, fr)

mrate_joined <- mrate_each_country %>%
  inner_join(mrate_aggregated)

var_mrates <- mrate_joined %>%
  group_by(year, age, sex) %>%
  summarise(
    n_countries=n_countries[1],
    var_mrate =sum(fr*death_rate_specific^2)/n[1] - death_rate_overall[1]^2,
    var_lmrate = sum(fr*ldeath_rate_specific^2)/n[1] - ldeath_rate_overall[1]^2
            )


# Produce log mortality rate contourplot ----------------------------------


png(filename="figures/mort_all_europe.png", 
    width=40, height=20, res=300, units="cm"
)
# Let's look at the mort rates only
contourplot(
  log(death_rate_overall) ~ year * age | sex, 
  data=subset(mrate_aggregated, subset=sex!="total" & 
                age <=90 & year >=1950 & year <=2010 ), 
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab=list(label="Age in years", cex=1.4),
  xlab=list(label="Year", cex=1.4),
  cex=1.4,
  cuts=50,
  col.regions=rev(heat.colors(200)),
  main=NULL,
  labels=list(cex=1.2),
  col="blue",
  scales=list(
    x=list(cex=1.4), 
    y=list(cex=1.4),
    alternating=3
    )
)
dev.off()



### Can we do mean-var coplots for all included countries?

# using mrate_aggregated and mrate_each_country

vitstat_all <- counts_eu %>%
  group_by(sex, year, age) %>%
  summarise(
    n_countries=length(death_count),
    death_count=sum(death_count),
    population_count=sum(population_count)
  )

meandeath_all <- vitstat_all %>%
  group_by(sex, year) %>%
  summarise(mean_death = sum(age * death_count)/sum(death_count)) %>%
  filter(sex !="total") 

mean_e5_all <- vitstat_all %>%
  filter(sex!="total" & age >=5) %>%
  summarise(mean_e5 = sum(age * death_count)/sum(death_count)) 

meandeath_all %>%
  ggplot(data=.) + 
  geom_line(aes(y=mean_death, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="e0 in years", x="Year") + ylim(c(0,90)) + 
  theme(legend.justification = c(0, 1), legend.position=c(0,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/period_life_expectancy_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)

mean_e5_all %>%
  ggplot(data=.) + 
  geom_line(aes(y=mean_e5, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="e5 in years", x="Year") + ylim(c(0,90)) + 
  theme(legend.justification = c(0, 1), legend.position=c(0,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/e5_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)


tab <- meandeath_all %>%
  arrange(year) %>%
  filter(year %in% c(1800, 1850, 1900, 1950, 2000)) %>%
  select(-country) %>% ungroup %>%
  spread(key=sex, value=mean_death) %>%
  round(1)

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/e0_mean_1750_onwards.html")

## Variance e0

varpar_all <- vitstat_all %>%
  group_by(sex, year) %>%
  summarise(var_par_death = sum(death_count * age^2)/sum(death_count))

vardeath_all <- varpar_all %>%
  inner_join(meandeath_all) %>%
  mutate(var_death = var_par_death - mean_death^2)

varpar_e5_all <- vitstat_all %>%
  filter(age >=5) %>%
  group_by(sex, year) %>%
  summarise(var_par_death = sum(death_count * age ^2) / sum(death_count))

vardeath_e5_all <- varpar_e5_all %>%
  inner_join(mean_e5_all) %>%
  mutate(var_death = var_par_death - mean_e5^2)

vardeath_all %>%
  ggplot(data=.) +
  geom_line(aes(y=var_death, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="Variance in e0 (Years squared)", x="Year") +
  theme(legend.justification = c(0, 0), legend.position=c(0,0), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/var_e0_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)

tab <- vardeath_all %>%
  arrange(year) %>%
  filter(year %in% c(1800, 1850, 1900, 1950, 2000)) %>%
  select(sex, year, var_death) %>% ungroup %>%
  spread(key=sex, value=var_death) %>%
  round(1)

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/e0_var_1750_onwards.html")


mean_e5_all %>%
  ggplot(data=.) + 
  geom_line(aes(y=mean_e5, x=year, group=sex, linetype=sex, col=sex)) + 
  labs(y="e5 (years)", x="Year") +
  theme(legend.justification = c(0, 1), legend.position=c(0,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/mean_e5_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)

tab <- mean_e5_all %>%
  arrange(year) %>%
  filter(year %in% c(1800, 1850, 1900, 1950, 2000)) %>%
  select(sex, year, mean_e5) %>% ungroup %>%
  spread(key=sex, value=mean_e5) %>%
  round(1)

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/e5_mean_1750_onwards.html")


vardeath_e5_all %>%
  ggplot(data=.) +
  geom_line(aes(y=var_death, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="Variance in e5 (Years squared)", x="Year") +
  theme(legend.justification = c(0, 0), legend.position=c(0,0), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/var_e5_1750_present.png", 
       units = "cm", dpi = 300, width=8, height=8)


tab <- vardeath_e5_all %>%
  arrange(year) %>%
  filter(year %in% c(1800, 1850, 1900, 1950, 2000)) %>%
  select(sex, year, var_death) %>% ungroup %>%
  spread(key=sex, value=var_death) %>%
  round(1)

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/e5_var_1750_onwards.html")

# Now to zoom in on 1950 onwards

# e0, mean, 1950+
vardeath_all %>%
  filter(year>=1950) %>%
  ggplot(data=.) + 
  geom_line(aes(y=mean_death, x=year, group=sex, col=sex, linetype=sex)) + 
  labs(y="e0 (Years)", x="Year") + 
  theme(legend.justification = c(1,0), legend.position=c(1,0), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed") 
ggsave(filename = "figures/mean_e0_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)


tab <- vardeath_all %>%
  arrange(year) %>%
  filter(year %in% c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) %>%
  select(sex, year, mean_death) %>% ungroup %>%
  spread(key=sex, value=mean_death) %>%
  round(1)

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/e0_mean_1950_onwards.html")

# e0, var, 1950+
vardeath_all %>%
  filter(year>=1950) %>%
  ggplot(data=.) + 
  geom_line(aes(y=var_death, x=year, group=sex, col=sex, linetype=sex)) + 
  labs(y="Variance in e0 (Years squared)", x="Year") + 
  theme(legend.justification = c(1,1), legend.position=c(1,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed") 
ggsave(filename = "figures/var_e0_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)


tab <- vardeath_all %>%
  arrange(year) %>%
  filter(year %in% c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) %>%
  select(sex, year, var_death) %>% ungroup %>%
  spread(key=sex, value=var_death) %>%
  round(1)

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/e0_var_1950_onwards.html")


# e5, mean, 1950+

vardeath_e5_all  %>% filter(year >=1950) %>%
  ggplot(data=.) +
  geom_line(aes(y=mean_e5, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="e5 in years", x="Year") +
  theme(legend.justification = c(0, 1), legend.position=c(0,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/mean_e5_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)


tab <- vardeath_e5_all %>%
  arrange(year) %>%
  filter(year %in% c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) %>%
  select(sex, year, mean_e5) %>% ungroup %>%
  spread(key=sex, value=mean_e5) %>%
  round(1)

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/e5_mean_1950_onwards.html")


# e5, var, 1950+

vardeath_e5_all  %>% filter(year >=1950) %>%
  ggplot(data=.) +
  geom_line(aes(y=var_death, x=year, col=sex, group=sex, linetype=sex)) +
  labs(y="Variance in e5 (Years squared)", x="Year") +
  theme(legend.justification = c(1, 1), legend.position=c(1,1), legend.key.size=unit(0.3, "cm")) + 
  scale_linetype_manual(values=c("solid", "dashed")) + 
  geom_vline(x=1950, linetype="dashed")
ggsave(filename = "figures/var_e5_1950_present.png", 
       units = "cm", dpi = 300, width=8, height=8)


tab <- vardeath_e5_all %>%
  arrange(year) %>%
  filter(year %in% c(1950, 1960, 1970, 1980, 1990, 2000, 2010)) %>%
  select(sex, year, var_death) %>% ungroup %>%
  spread(key=sex, value=var_death) %>%
  round(1)

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/e5_var_1950_onwards.html")


meandeath_all <- meandeath_all %>%
  mutate(country="all")

# Now, to do this for each country (e0)


#meandeath_each 
meandeath_each <- counts_eu %>%
  group_by(country, year, sex) %>%
  summarise(mean_death= sum(death_count *age) /sum(death_count))

varpar_each <- counts_eu %>%
  group_by(country, year, sex) %>%
  summarise(varpar = sum(age^2*death_count) / sum(death_count))

var_each <- meandeath_each %>%
  inner_join(varpar_each) %>%
  mutate(var_death =varpar - mean_death^2) %>%
  select(country, year, sex, mean_death, var_death)


# now to merge 
mnvar_merged <- vardeath_all %>%
  select(sex, year, mean_death_overall=mean_death, var_death_overall=var_death) %>%
  inner_join(var_each)

dif_mnvars <- mnvar_merged %>%
  mutate(
    dif_mean=mean_death-mean_death_overall, 
    dif_var=var_death-var_death_overall,
    country_code=as.character(country)
    )
dif_mnvars$country_code[dif_mnvars$country_code=="FRATNP"] <- "FRA"
dif_mnvars$country_code[dif_mnvars$country_code=="GBRTENW"] <- "GBR"
dif_mnvars$country_code[dif_mnvars$country_code=="GBR_NIR"] <- "GBR"
dif_mnvars$country_code[dif_mnvars$country_code=="GBR_SCO"] <- "GBR"
dif_mnvars$country_code[dif_mnvars$country_code=="DEUT"] <- "DEU"



tmp <- gdp %>%
  group_by(country_code) %>%
  filter(year==max(year)) %>%
  select(country_code, gdp=gdp_pc_ppp)

dif_mnvars <- dif_mnvars  %>%
  left_join(tmp)

rm(tmp)
dif_mnvars <- dif_mnvars %>%
  ungroup

dif_mnvars$country <- as.factor(dif_mnvars$country)
dif_mnvars$country <-  reorder(x=dif_mnvars$country, X=dif_mnvars$gdp)
levels(dif_mnvars$country) <- c(
  "Bulgaria", "Latvia", "Hungary", "Poland", "Lithuania", "Estonia", "Slovakia", "Portugal",
  "Slovenia", "Czech Republic",  "Spain", "Italy", "France", "England & Wales", 
  "Northern Ireland", "Scotland",
  "Finland", "Belgium", "Denmark", "Germany", "Sweden", "Austria", "Ireland", "Netherlands",
  "Norway", "Luxembourg"
  )

#figure 
dif_mnvars %>%
  filter(sex=="male" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + ylim(c(-10, 10)) + 
  labs(title="Males", x="Year", y="Difference in e0 from European average")
ggsave(filename="figures/dif_males_1950.png", width=20, height=20, dpi=300, unit="cm")

#table

tab <- dif_mnvars %>%
  select(country, sex, year, dif_mean) %>%
  filter(year %in% c(1950, 1960, 1970, 1980, 1990, 2000, 2010), sex=="male") 

tab$dif_mean <- round(tab$dif_mean, 1)

tab <- tab %>%
  spread(key=country, value=dif_mean) 

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/dif_e0_males.html")


dif_mnvars %>%
  filter(sex=="female" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + ylim(c(-10, 10)) +
  labs(title="Females", x="Year", y="Difference in e0 from European average")
ggsave(filename="figures/dif_females_1950.png", width=10, height=10, dpi=300)

tab <- dif_mnvars %>%
  select(country, sex, year, dif_mean) %>%
  filter(year %in% c(1950, 1960, 1970, 1980, 1990, 2000, 2010), sex=="female") 

tab$dif_mean <- round(tab$dif_mean, 1)

tab <- tab %>%
  spread(key=country, value=dif_mean) 

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/dif_e0_females.html")


# Now, as above but e5

#meandeath_each 
meandeath_each <- counts_eu %>%
  filter(age>=5) %>%
  group_by(country, year, sex) %>%
  summarise(mean_death= sum(death_count *age) /sum(death_count))

varpar_each <- counts_eu %>%
  filter(age >=5) %>%
  group_by(country, year, sex) %>%
  summarise(varpar = sum(age^2*death_count) / sum(death_count))

var_each <- meandeath_each %>%
  inner_join(varpar_each) %>%
  mutate(var_death =varpar - mean_death^2) %>%
  select(country, year, sex, mean_death, var_death)


# now to merge 
mnvar_merged <- vardeath_e5_all %>%
  select(sex, year, mean_death_overall=mean_e5, var_death_overall=var_death) %>%
  inner_join(var_each)

dif_mnvars <- mnvar_merged %>%
  mutate(
    dif_mean=mean_death-mean_death_overall, 
    dif_var=var_death-var_death_overall,
    country_code=as.character(country)
  )
dif_mnvars$country_code[dif_mnvars$country_code=="FRATNP"] <- "FRA"
dif_mnvars$country_code[dif_mnvars$country_code=="GBRTENW"] <- "GBR"
dif_mnvars$country_code[dif_mnvars$country_code=="GBR_NIR"] <- "GBR"
dif_mnvars$country_code[dif_mnvars$country_code=="GBR_SCO"] <- "GBR"
dif_mnvars$country_code[dif_mnvars$country_code=="DEUT"] <- "DEU"



tmp <- gdp %>%
  group_by(country_code) %>%
  filter(year==max(year)) %>%
  select(country_code, gdp=gdp_pc_ppp)

dif_mnvars <- dif_mnvars  %>%
  left_join(tmp)

rm(tmp)
dif_mnvars <- dif_mnvars %>%
  ungroup

dif_mnvars$country <- as.factor(dif_mnvars$country)
dif_mnvars$country <-  reorder(x=dif_mnvars$country, X=dif_mnvars$gdp)
levels(dif_mnvars$country) <- c(
  "Bulgaria", "Latvia", "Hungary", "Poland", "Lithuania", "Estonia", "Slovakia", "Portugal",
  "Slovenia", "Czech Republic",  "Spain", "Italy", "France", "England & Wales", 
  "Northern Ireland", "Scotland",
  "Finland", "Belgium", "Denmark", "Germany", "Sweden", "Austria", "Ireland", "Netherlands",
  "Norway", "Luxembourg"
)

dif_mnvars %>%
  filter(sex=="male" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + ylim(c(-10, 10)) + 
  labs(title="Males", x="Year", y="Difference in e5 from European average")
ggsave(filename="figures/dif_males_1950_e5.png", width=20, height=20, dpi=300, unit="cm")

tab <- dif_mnvars %>%
  select(country, sex, year, dif_mean) %>%
  filter(year %in% c(1950, 1960, 1970, 1980, 1990, 2000, 2010), sex=="male") 

tab$dif_mean <- round(tab$dif_mean, 1)

tab <- tab %>%
  spread(key=country, value=dif_mean) 

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/dif_e5_males.html")



dif_mnvars %>%
  filter(sex=="female" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + ylim(c(-10, 10)) +
  labs(title="Females", x="Year", y="Difference in e5 from European average")
ggsave(filename="figures/dif_females_1950_e5.png", width=10, height=10, dpi=300)

tab <- dif_mnvars %>%
  select(country, sex, year, dif_mean) %>%
  filter(year %in% c(1950, 1960, 1970, 1980, 1990, 2000, 2010), sex=="female") 

tab$dif_mean <- round(tab$dif_mean, 1)

tab <- tab %>%
  spread(key=country, value=dif_mean) 

class(tab) <- "data.frame"

print(xtable(tab), type="html", file="tables/dif_e5_females.html")



# Regression - dif explained by gdp pc ------------------------------------


# want a table with 
#country_code, year, gdp_pc, e0_male, e5_male, e0_female, e5_female

#meandeath_each 
e0_each <- counts_eu %>%
  group_by(country, year, sex) %>%
  summarise(e0= sum(death_count *age) /sum(death_count))

e5_each <- counts_eu %>%
  filter(age >=5) %>%
  group_by(country, year, sex) %>%
  summarise(e5= sum(death_count *age) /sum(death_count))

tmp <- vardeath_all  %>% 
  ungroup %>%
  select(year, sex, e0_avg=mean_death)

e0_each <- e0_each  %>% 
  filter(sex!="total") %>%
  left_join(tmp) %>%
  mutate(
    dif_e0 = e0 - e0_avg,
    country_code = as.character(country)
    )
  
e0_each$country_code[e0_each$country_code=="FRATNP"] <- "FRA"
e0_each$country_code[e0_each$country_code=="GBRTENW"] <- "GBR"
e0_each$country_code[e0_each$country_code=="GBR_NIR"] <- "GBR"
e0_each$country_code[e0_each$country_code=="GBR_SCO"] <- "GBR"
e0_each$country_code[e0_each$country_code=="DEUT"] <- "DEU"

e0_gdp <- e0_each %>%
  inner_join(gdp) %>%
  ungroup

mod_year <- dlply(e0_gdp, .(country, sex), lm, formula=dif_e0 ~ year)
mod_lin_noint <- dlply(e0_gdp, .(country, sex), lm, formula=dif_e0 ~ gdp_pc_ppp + year)
mod_lin_int <- dlply(e0_gdp, .(country, sex), lm, formula=dif_e0 ~ gdp_pc_ppp * year)
mod_log_noint <- dlply(e0_gdp, .(country, sex), lm, formula=dif_e0 ~ log(gdp_pc_ppp) + year)
mod_log_int <- dlply(e0_gdp, .(country, sex), lm, formula=dif_e0 ~ log(gdp_pc_ppp) * year)

aic_year <- sapply(mod_year, AIC)
aic_lin_noint <- sapply(mod_lin_noint, AIC)
aic_lin_int <- sapply(mod_lin_int, AIC)
aic_log_noint <- sapply(mod_log_noint, AIC)
aic_log_int <- sapply(mod_log_int, AIC)

aic_matrix <- rbind(aic_year, aic_lin_noint, aic_lin_int, aic_log_noint, aic_log_int)

aic_matrix_rank <- apply(aic_matrix, 2, rank)
## I'm not sure how useful this is...

# CLPS proper -------------------------------------------------------------

counts_eu_all <- counts_eu %>%
  group_by(year, age, sex) %>%
  dplyr::summarise(death_count=sum(death_count),
            population_count=sum(population_count), n_countries = n_distinct(country))

# 9) rates for all of Europe

rates_eu_all <- counts_eu_all  %>% mutate(death_rate_europe=death_count/population_count)

mort_eu <- rates_eu_all
mort_eu$death_count <- NULL
mort_eu$population_count <- NULL
mort_eu$n_countries <- NULL


mort_eu <- plyr::rename(mort_eu, replace=c("death_rate_europe"="europe"))

#write.csv(mort_eu, file="apps/clp_explorer/data/europe_overall.csv", row.names=F)


# Calculate difference in mort rate for 
# Scotland
# England & Wales
# France
# Norway

countries_to_keep <- c(
  "GBRTENW",
  "GBR_SCO",
  "FRATNP",
  "NOR", 
  "ITA"
)

counts_s <- subset(
  counts_eu,
  subset=country %in% countries_to_keep
)

rates_s <- counts_s  %>% 
  mutate(death_rate = death_count/population_count)
rates_s$death_count <- NULL
rates_s$population_count <- NULL

rates_s$country <- revalue(
  rates_s$country,
  replace=c(
    "GBRTENW"="England & Wales",
    "GBR_SCO"="Scotland",
    "FRATNP"="France",
    "NOR"="Norway",
    "ITA"="Italy"
    )                        
  )

rates_wide <- rates_s  %>% spread(key=country, value=death_rate)

rates_wide <- rates_wide %>%
  filter(year >=1950 & year <=2010 & age <=90)

rates_wide <- rates_wide  %>% left_join(mort_eu)

names(rates_wide)[6] <- "enw"

diffs <- rates_wide %>%
  mutate(
    France = France - europe,
    Scotland = Scotland - europe,
    enw = enw - europe,
    Norway = Norway - europe,
    Italy = Italy - europe
  )

dif_logs <- rates_wide  %>% mutate(
  France=log(France)-log(europe),
  Scotland=log(Scotland)-log(europe),
  enw=log(enw) - log(europe),
  Norway=log(Norway)-log(europe),
  Italy = log(Italy) - log(europe)
  )

# Borrowing from : 
# http://stackoverflow.com/questions/17022379/image-smoothing-in-r

# Want to do some smoothing to make the contour lines less busy
fn <- function(input, smooth_par=2){
  this_sex <- input$sex[1]
  this_country <- input$country[1]

  dta <- input %>%
    select(year, age, lmort) %>%
    spread(key=age, value=lmort) 
  ages <- names(dta)[-1]
  years <- dta$year
  dta$year <- NULL
  dta <- as.matrix(dta)
  rownames(dta) <- years
  colnames(dta) <- ages
  dta_blurred <- as.matrix(blur(as.im(dta), sigma=smooth_par))  
  rownames(dta_blurred) <- rownames(dta)
  colnames(dta_blurred) <- colnames(dta)
  output <- data.frame(
    year=years, 
    sex=this_sex,
    country=this_country,
    dta_blurred
    )
  output <- output %>%
    gather(key=age, value=lmort, -year, -sex, -country)
  
  output$age <- output$age %>%
    str_replace("X", "") %>%
    as.character %>%
    as.numeric
  
  return(output)
}


dif_logs_blurred <- ddply(dif_logs, .(sex, country), fn, smooth_par=1.5)


# ####################
# ##################
# tiff(
#   "figures/fig_02_europe.tiff",  
#   height=600, width=1200
# )
# 
# europe_log <- contourplot(
#   log(europe) ~ year * age | sex, 
#   data=subset(mort_eu, subset=sex!="total" & 
#                 age <=100 & year >=1950 & year <=2010 ), 
#   region=T, 
#   par.strip.text=list(cex=1.4, fontface="bold"),
#   ylab="Age in years",
#   xlab="Year",
#   cex=1.4,
#   cuts=50,
#   col.regions=rev(heat.colors(200)),
#   main=NULL
# )
# print(europe_log)
# dev.off()

dif_logs <- dif_logs %>%
  gather(key=country, value=lmort, -year, -age, -sex) %>%
  mutate(lmort = ifelse(lmort < -1.2, -1.2, lmort),
            lmort= ifelse(lmort > 1.2, 1.2, lmort)) 

dif_ident <- diffs %>%
  gather(key=country, value=mort, -year, -age, -sex)

dif_logs$country <- dif_logs$country  %>% mapvalues(from="enw", to="England & Wales" )
dif_ident$country <- dif_ident$country %>%
  mapvalues(from="enw", to="England & Wales")
##########################################################



tiff(
  "figures/clp_Scotland.tiff",  
  height=20, width=40,
  res=300,
  units="cm"
)

scot_lev <- dif_logs %>%
  filter(sex !="total" & country=="Scotland" & age <=90) %>%
  levelplot(
  lmort ~ year * age | sex, 
  data=.,
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab="Age in years",
  xlab="Year",
  scales=list(alternating=3),
  at = seq(from= -1.2, to = 1.2, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
  main=NULL
)
print(scot_lev)

dev.off()

tiff(
  "figures/clp_italy.tiff",  
  height=20, width=40,
  res=300,
  units="cm"
)

italy_lev <- dif_logs %>%
  filter(sex !="total" & country=="Italy" & age <=90) %>%
  levelplot(
    lmort ~ year * age | sex, 
    data=.,
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab="Age in years",
    xlab="Year",
    scales=list(alternating=3),
    at = seq(from= -1.2, to = 1.2, by=0.2),
    col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
    main=NULL
  )
print(italy_lev)

dev.off()


tiff(
  "figures/clp_eng_wales.tiff",  
  height=20, width=40,
  units="cm", res=300
)
eng_lev <- dif_logs  %>% 
  filter(sex!="total" & country=="England & Wales" & age <=90)  %>% 
  levelplot(
  lmort ~ year * age | sex , 
  data=.,
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  scales=list(alternating=3),
  ylab="Age in years",
  xlab="Year",
  cex=1.4,
  at = seq(from= -1.2, to = 1.2, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
  main=NULL
)
print(eng_lev)
dev.off()


###########################################################
###########################################################
tiff(
  "figures/clp_france.tiff",  
  height=20, width=40,
  units="cm", res=300
)

france_lev <- dif_logs  %>% 
  filter(sex!="total" & country =="France" & age <=90 )  %>% 
  levelplot(
  lmort ~ year * age | sex, 
  data=.,
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  scales=list(alternating=3),
  ylab="Age in years",
  xlab="Year",
  cex=1.4,
  at = seq(from= -1.2, to = 1.2, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
  main=NULL
)
print(france_lev)
dev.off()

tiff(
  "figures/clp_norway.tiff",  
  height=20, width=40,
  units="cm", res=300
)
norway_lev <- dif_logs  %>% 
  filter(sex!="total" & country=="Norway" & age <=90)  %>% 
  levelplot(
  lmort ~ year * age | sex , 
  data=.,
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab="Age in years",
  xlab="Year",
  cex=1.4,
  scales=list(alternating=3),
  at = seq(from= -1.2, to = 1.2, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
  main=NULL
)
print(norway_lev)
dev.off()

## Now all on plot
tiff(
  "figures/clp_all.tiff",  
  height=20, width=40,
  units="cm", res=300
)
all_lev <- dif_logs %>%
  filter(sex!="total" & country !="europe" & age <=90) %>%
  levelplot(
    lmort ~ year * age | country + sex,
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

all_cont <- dif_logs_blurred %>%
  filter(sex!="total" & country !="europe" & age <=80) %>%
  contourplot(
    lmort ~ year + age | country + sex, 
    data=.,
    region=F,
    ylab="",
    xlab="",
    scales=list(NULL),
    at=0,
    labels=F,
    main=NULL
  )

print(all_lev + all_cont)
dev.off()

tiff(
  "figures/clp_all_ident.tiff",  
  height=20, width=40,
  units="cm", res=300
)
ident_all_lev <- dif_ident %>%
  filter(sex!="total" & country !="europe" & age <=80) %>%
  levelplot(
    mort ~ year + age | country + sex, 
    data=.,
    region=T,
    ylab="Age in years",
    xlab="Year",
    scales=list(alternating=3),
    at=seq(from=-0.1, to=0.1, by=0.01),
    col.regions=colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
    main=NULL,
    par.settings=list(strip.background=list(col="lightgrey"))
    )

ident_all_cont <- dif_logs_blurred %>%
  filter(sex!="total" & country !="europe" & age <=80) %>%
  contourplot(
    lmort ~ year + age | country + sex, 
    data=.,
    region=F,
    ylab="",
    xlab="",
    scales=list(NULL),
    at=0,
    labels=F,
    main=NULL
    )


print(ident_all_lev + ident_all_cont)
dev.off()

#########################################################################################
########## ADDITIONAL - BATHTUB CURVES
#########################################################################################

# To do : bathtub curves for 1958 and 1970 cohorts
# Scot  & Eng/Wales
# Males and females
## -- compared with European average


# Using the rates_wide variable earlier

rates_wide <- mutate(rates_wide, cohort=year-age)

dta_cohorts <- subset(rates_wide, subset=cohort ==1958 | cohort == 1970)
dta_cohorts <- melt(
  dta_cohorts,
  id.vars=c("year", "age", "sex", "cohort"),
  measure.vars=c("france", "scotland" ,"england_and_wales", "norway", "europe"),
  variable.name="country", value.name="mortality_rate"
  )
dta_cohorts$lt <- "solid"
dta_cohorts$lt[dta_cohorts$country=="europe"] <- "longdash"
dta_cohorts$lw <- 1.1
dta_cohorts$lw[dta_cohorts$country=="europe"] <- 1

dta_cohorts$cohort <- as.factor(dta_cohorts$cohort)

tiff("figures/cohorts_Eng_wales.tiff", 600, 600)
g1 <- ggplot(data=subset(dta_cohorts, subset=(country=="england_and_wales" | country=="europe") & sex!="total")) +
  scale_linetype_identity() + scale_size_identity() + 
  geom_line(aes(x=age, y=mortality_rate, group=country, colour=country, linetype=lt, size=lw)) + 
  facet_grid(facets= cohort ~ sex) + 
  scale_y_log10()  +
  scale_colour_manual(values=c("blue", "red"), guide="none") +  
  labs(X="Age", y="Mortality rate")

g1
dev.off()


tiff("figures/cohorts_Scotland.tiff", 600, 600)
g1 <- ggplot(data=subset(dta_cohorts, subset=(country=="scotland" | country=="europe") & sex!="total")) +
  scale_linetype_identity() + scale_size_identity() + 
  geom_line(aes(x=age, y=mortality_rate, group=country, colour=country, linetype=lt, size=lw)) + 
  facet_grid(facets= cohort ~ sex) + 
  scale_y_log10()  +
  scale_colour_manual(values=c("blue", "red"), guide="none") +  
  labs(X="Age", y="Mortality rate")

g1
dev.off()


# Now using diffs variable calculated earlier to present plot diffs
dif_logs <- mutate(
  rates_wide,
  france=log(france)-log(europe),
  scotland=log(scotland)-log(europe),
  england_and_wales=log(england_and_wales)-log(europe),
  norway=log(norway)-log(europe)
)


tiff("figures/levelplots_engwales_cohorts_highlighted.tiff", 1200, 600)
# Finally, level plots as above, but with ablines to highlight the two cohorts of interest
engwales_lev <- levelplot(
  england_and_wales ~ year * age | sex, 
  data=subset(dif_logs, subset=sex!="total"),
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab="Age in years",
  xlab="Year",
  cex=1.4,
  at = seq(from= -1.2, to = 1.2, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
  main=NULL,
  panel=function(...){
    panel.levelplot(...)
    panel.abline(
      a=-1958, b=1, 
      lwd=2, lty="dashed"
    )
    panel.abline(
      a=-1970, b=1,
      lwd=2, lty="dashed"
    )
  }
)
print(engwales_lev)
dev.off()

tiff("figures/levelplots_scotland_cohorts_highlighted.tiff", 1200, 600)
# Finally, level plots as above, but with ablines to highlight the two cohorts of interest
scot_lev <- levelplot(
  scotland ~ year * age | sex, 
  data=subset(dif_logs, subset=sex!="total"),
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab="Age in years",
  xlab="Year",
  cex=1.4,
  at = seq(from= -1.2, to = 1.2, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
  main=NULL,
  panel=function(...){
    panel.levelplot(...)
    panel.abline(
      a=-1958, b=1, 
      lwd=2, lty="dashed"
      )
    panel.abline(
      a=-1970, b=1,
      lwd=2, lty="dashed"
    )
  }
)
print(scot_lev)
dev.off()


# Cumulative mort advantage/disadvantage by age ---------------------------

# using mrate_joined

comparisons <- mrate_joined %>%
  select(year, age, sex, country, specific=death_rate_specific, overall=death_rate_overall) %>%
  mutate(cohort = year - age)

comparisons <- comparisons %>%
  group_by(country, sex, year) %>%
  mutate(
    synth_cohort_specific = cumprod( 1 - specific),
    synth_cohort_overall = cumprod( 1 - overall),
    difference= synth_cohort_specific - synth_cohort_overall
)

comparisons %>%
  filter(country=="AUT" & sex !="total") %>%
  ggplot +
  geom_line(aes(x=age, y=difference, group=sex, col=sex)) + 
  facet_wrap(~ year) + 
  ggtitle("AUS")

fn <- function(x){
  this_country <- x$country[1]
  this_title <- paste("Year sections,", this_country)
  

  x %>%
    filter(sex !="total" & age <=90) %>%
    ggplot +
    geom_line(aes(x=age, y=difference, group=sex, col=sex)) + 
    facet_wrap( ~ year) + 
    ggtitle(this_title)
  
  ggsave(filename=paste0("figures/diffs/year_sections/", this_country, ".png"),
         height=10, width=10)
  NULL
}

d_ply(comparisons, .(country), fn, .progress="text")


comparisons <- mrate_joined %>%
  select(year, age, sex, country, specific=death_rate_specific, overall=death_rate_overall) %>%
  mutate(cohort = year - age)
comparisons <- comparisons %>%
  group_by(country, sex, cohort) %>%
  mutate(
    synth_cohort_specific = cumprod( 1 - specific),
    synth_cohort_overall = cumprod( 1 - overall),
    difference= synth_cohort_specific - synth_cohort_overall
  )


fn <- function(x){
  this_country <- x$country[1]
  this_title <- paste("Cohort sections,", this_country)
  
  
  x %>%
    filter(sex !="total" & age <=90) %>%
    ggplot +
    geom_line(aes(x=age, y=difference, group=sex, col=sex)) + 
    facet_wrap( ~ cohort) + 
    ggtitle(this_title)
  
  ggsave(filename=paste0("figures/diffs/cohort_sections/", this_country, ".png"),
         height=10, width=10)
  NULL
}

d_ply(comparisons, .(country), fn, .progress="text")



