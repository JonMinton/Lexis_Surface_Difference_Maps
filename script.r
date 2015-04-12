# Creating the series of images for use in the ASPHER poster

# Jon Minton
# 8 December 2014
# Current idea:

# Calculate difference in mort rate for 
# Scotland
# England & Wales
# France
# Norway

# Compared with European Average

# Within the European average include the combined East and West Germany as a single country

# i.e. DEUTE + DEUTW = DEUT 
# (Instead of DEUTNP)

# 1)  clear the workspace

rm(list=ls())

require(plyr)
require(tidyr)
require(dplyr)

require(ggplot2)
require(lattice)
require(latticeExtra)
require(RColorBrewer)




######################################################################################
# SOURCE DATA

# load counts with east and west germany combined 

country_codes <- read.csv("data/tidy/country_codes__new.csv", stringsAsFactors=F) %>%
  tbl_df

europe_codes <- country_codes$short[country_codes$europe==1]
counts <- read.csv("data/tidy/counts_germany_combined.csv")

counts_eu <- counts  %>% filter(country %in% europe_codes) %>%
  tbl_df

#write.csv(counts_eu, file="apps/clp_explorer/data/counts_eu.csv", row.names=F)

# 7) aggregate up count data from all available European nations
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

var_mrates %>%
  ungroup %>%
  filter(sex!="total" & 
           age %in% c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80) &
           year > 1950) %>%
  ggplot(data=.) +
  geom_line(aes(y=var_mrate, x=year)) +
  facet_grid(age~sex) + scale_y_log10()

## Hmm, not sure how illuminating this is...
# How about var contour plot
 contourplot(
   log(var_mrate) ~ year * age | sex, 
   data=subset(var_mrates, subset=sex!="total" & 
                 age <=80 & year >=1950 & year <=2010 ), 
   region=T, 
   par.strip.text=list(cex=1.4, fontface="bold"),
   ylab="Age in years",
   xlab="Year",
   cex=1.4,
   cuts=50,
   col.regions=rev(heat.colors(200)),
   main=NULL,
   labels=list(cex=1.2),
   col="grey"
)

# Seems to work - but seems too similar to mort

# Let's look at the mort rates only
contourplot(
  log(death_rate_overall) ~ year * age | sex, 
  data=subset(mrate_aggregated, subset=sex!="total" & 
                age <=80 & year >=1950 & year <=2010 ), 
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab="Age in years",
  xlab="Year",
  cex=1.4,
  cuts=50,
  col.regions=rev(heat.colors(200)),
  main=NULL,
  labels=list(cex=1.2),
  col="grey"
)



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

meandeath_all %>%
  ggplot(data=.) + 
  geom_line(aes(y=mean_death, x=year, col=sex, group=sex)) +
  labs(y="Mean age of death in years", x="Year")

varpar_all <- vitstat_all %>%
  group_by(sex, year) %>%
  summarise(var_par_death = sum(death_count * age^2)/sum(death_count))

vardeath_all <- varpar_all %>%
  inner_join(meandeath_all) %>%
  mutate(var_death = var_par_death - mean_death^2)

vardeath_all %>%
  ggplot(data=.) +
  geom_line(aes(y=var_death, x=year, col=sex, group=sex)) +
  labs(y="Variance in mean age of death in years", x="Year")

vardeath_all %>%
  filter(year >= 1900) %>%
  ggplot(data=.) +
  geom_line(aes(y=var_death, x=year, col=sex, group=sex)) +
  labs(y="Variance in mean age of death in years", x="Year")

meandeath_all %>%
  filter(year >= 1900) %>%
  ggplot(data=.) +
  geom_line(aes(y=mean_death, x=year, col=sex, group=sex)) +
  labs(y="Mean age of death in years", x="Year")

meandeath_all <- meandeath_all %>%
  mutate(country="all")

# Now, to do this for each country

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

var_each %>%
  filter(sex !="total") %>%
  ggplot(data=.) +
  geom_line(aes(y=mean_death, x=year, col=country, group=country)) + 
  facet_grid(. ~ sex)

var_each %>%
  filter(sex !="total") %>%
  ggplot(data=.) +
  geom_line(aes(y=var_death, x=year, col=country, group=country)) + 
  facet_grid(. ~ sex)


# now to merge 
mnvar_merged <- vardeath_all %>%
  select(sex, year, mean_death_overall=mean_death, var_death_overall=var_death) %>%
  inner_join(var_each)

dif_mnvars <- mnvar_merged %>%
  mutate(
    dif_mean=mean_death-mean_death_overall, 
    dif_var=var_death-var_death_overall
    )

dif_mnvars %>%
  filter(sex=="male" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + 
  ggtitle("Males")
ggsave(filename="dif_males_1950.png", width=10, height=10, dpi=300)

dif_mnvars %>%
  filter(sex=="female" & year >=1950) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country) + 
ggtitle("Females")
ggsave(filename="dif_females_1950.png", width=10, height=10, dpi=300)



dif_mnvars %>%
  filter(sex=="male" & year >=1970) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_var < 0, dif_var, 0),
                  ymax=ifelse(dif_var > 0, dif_var, 0)
  )) + 
  facet_wrap(~country)

dif_mnvars %>%
  filter(sex=="female" & year >=1970) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_var < 0, dif_var, 0),
                  ymax=ifelse(dif_var > 0, dif_var, 0)
  )) + 
  facet_wrap(~country)


dif_mnvars %>%
  filter(sex=="male" & year >=1970) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country, ncol=5) + 
  ggtitle("Males")
ggsave(filename="dif_males.png", width=10, height=10)

dif_mnvars %>%
  filter(sex=="female" & year >=1970) %>%
  ggplot(data=.) +
  geom_ribbon(aes(x=year, 
                  ymin=ifelse(dif_mean < 0, dif_mean, 0),
                  ymax=ifelse(dif_mean > 0, dif_mean, 0)
  )) + 
  facet_wrap(~country, ncol=5) + 
  ggtitle("Females")
ggsave(filename="dif_females.png", width=10, height=10)



# 8) want to produce a simple summary of this
counts_summaries <- ddply(
  tmp <- subset(counts_eu_all, subset=sex=="total"),
  .(year),
  summarise,
  n_countries=median(n_countries),
  population_count=sum(population_count)
)


# When was the earliest country's data available?
country_by_earliest_year <- ddply(counts_eu, .(country), summarise, earliest=min(year))
country_by_earliest_year <- arrange(country_by_earliest_year, earliest)

# 9) rates for all of Europe

rates_eu_all <- mutate(counts_eu_all, death_rate_europe=death_count/population_count)

mort_eu <- rates_eu_all
mort_eu$death_count <- NULL
mort_eu$population_count <- NULL
mort_eu$n_countries <- NULL


mort_eu <- rename(mort_eu, replace=c("death_rate_europe"="europe"))

write.csv(mort_eu, file="apps/clp_explorer/data/europe_overall.csv", row.names=F)


# Calculate difference in mort rate for 
# Scotland
# England & Wales
# France
# Norway

countries_to_keep <- c(
  "GBRTENW",
  "GBR_SCO",
  "FRATNP",
  "NOR"
)

counts_s <- subset(
  counts,
  subset=country %in% countries_to_keep
)

rates_s <- mutate(counts_s, death_rate = death_count/population_count)
rates_s$death_count <- NULL
rates_s$population_count <- NULL

rates_s$country <- revalue(
  rates_s$country,
  replace=c(
    "GBRTENW"="england_and_wales",
    "GBR_SCO"="scotland",
    "FRATNP"="france",
    "NOR"="norway"
    )                        
                          )

rates_wide <- melt(rates_s,
                     id.vars=c("year", "age", "sex", "country"),
                     measure.vars=c("death_rate")
                     )

rates_wide <- dcast(rates_wide,
                    year + age + sex ~ country
                    )

rates_wide <- subset(
  rates_wide,
  subset=year >=1950 & year <=2010 & age <=100 
  )

rates_wide <- join(rates_wide, mort_eu)

diffs <- mutate(
  rates_wide,
  france=france-europe,
  scotland=scotland-europe,
  england_and_wales=england_and_wales-europe,
  norway=norway-europe
                )

dif_logs <- mutate(
  rates_wide,
  france=log(france)-log(europe),
  scotland=log(scotland)-log(europe),
  england_and_wales=log(england_and_wales) - log(europe),
  norway=log(norway)-log(europe)  
  )

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


##########################################################

# replace values above thresholds by the threshold value for clearer visualisation
dif_logs$france[dif_logs$france < -1.2] <- -1.2
dif_logs$france[dif_logs$france >  1.2] <-  1.2

dif_logs$scotland[dif_logs$scotland < -1.2] <- -1.2
dif_logs$scotland[dif_logs$scotland >  1.2] <-  1.2

dif_logs$england_and_wales[dif_logs$england_and_wales < -1.2] <- -1.2
dif_logs$england_and_wales[dif_logs$england_and_wales >  1.2] <-  1.2

dif_logs$norway[dif_logs$norway < -1.2] <- -1.2
dif_logs$norway[dif_logs$norway >  1.2] <-  1.2



tiff(
  "figures/fig_01_scotland.tiff",  
  height=600, width=1200
)


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
  main=NULL
)
print(scot_lev)

dev.off()

tiff(
  "figures/fig_02_eng_wales.tiff",  
  height=600, width=1200
)
eng_lev <- levelplot(
  england_and_wales ~ year * age | sex , 
  data=subset(dif_logs, subset=sex!="total"),
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
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
  "figures/fig_03_france.tiff",  
  height=600, width=1200
)
france_lev <- levelplot(
  france ~ year * age | sex, 
  data=subset(dif_logs, subset=sex!="total"),
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
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
  "figures/fig_04_norway.tiff",  
  height=600, width=1200
)
norway_lev <- levelplot(
  norway ~ year * age | sex , 
  data=subset(dif_logs, subset=sex!="total"),
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab="Age in years",
  xlab="Year",
  cex=1.4,
  at = seq(from= -1.2, to = 1.2, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
  main=NULL
)
print(norway_lev)
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



