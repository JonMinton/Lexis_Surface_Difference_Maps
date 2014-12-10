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



# 1)  clear the workspace

rm(list=ls())

require(plyr)
require(reshape2)
require(ggplot2)
require(lattice)
require(RColorBrewer)






######################################################################################
# SOURCE DATA

# 4) load human mortality database (HMD) data on population counts and death counts
# in the 'tidy data' format suggested by Hadley Wickham 
counts <- read.csv("data/tidy/counts.csv")
# (For the code used to convert the existing files to the 'tidy' please contact me)

# 5) load a file which shows which of the HMD countries are part of Europe

country_codes <- read.csv("data/tidy/country_codes__new.csv", stringsAsFactors=F)

europe_codes <- country_codes$short[country_codes$europe==1]

#######################################################################################
#######################################################################################

# show changes in e0s for european countries since 1950 
e0 <- read.csv("data/tidy/e0_per.csv")

e0_ss <- subset(
  e0,
  country_code %in% tolower(europe_codes) & year >=1950 & country_code!="bel"
  )

e0_ss$total <- NULL

e0_ss <- melt(
  e0_ss,
  id.var=c("year", "country_code")
  )

# arrange by male life expectancy
tmp <- subset(e0_ss, year == 2010)
tmp2 <- dcast(tmp, year + country_code ~ variable)
arrange(tmp2, desc(male))
arrange(tmp2, desc(female))


g1 <- ggplot(e0_ss)

g2 <- g1 + geom_line(aes(x=year, y=value, group=variable, colour=variable)) + facet_wrap("country_code")

######################################################################################
# DERIVED DATA

# 6) find the subset of counts data which is of European countries


counts_eu <- subset(
  counts,
  subset=country %in% europe_codes                  
)

# 7) aggregate up count data from all available European nations
counts_eu_all <- ddply(
  counts_eu,
  .(sex, year, age),
  summarise,
  n_countries=length(death_count),
  death_count=sum(death_count),
  population_count=sum(population_count)
)


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

####################
##################
tiff(
  "figures/fig_02_europe.tiff",  
  height=600, width=1200
)

europe_log <- contourplot(
  log(europe) ~ year * age | sex, 
  data=subset(mort_eu, subset=sex!="total" & 
                age <=100 & year >=1950 & year <=2010 ), 
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab="Age in years",
  xlab="Year",
  cex=1.4,
  cuts=50,
  col.regions=rev(heat.colors(200)),
  main=NULL
)
print(europe_log)
dev.off()


##########################################################

tiff(
  "figures/fig_03_scotland.tiff",  
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
  at = seq(from= -1, to = 1, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
  main=NULL
)
print(scot_lev)
dev.off()

###########################################################
###########################################################
tiff(
  "figures/fig_04_france.tiff",  
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
  at = seq(from= -1, to = 1, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
  main=NULL
)
print(france_lev)
dev.off()

tiff(
  "figures/fig_05_norway.tiff",  
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
  at = seq(from= -1, to = 1, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
  main=NULL
)
print(norway_lev)
dev.off()

tiff(
  "figures/fig_06_eng_wales.tiff",  
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
  at = seq(from= -1, to = 1, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
  main=NULL
)
print(eng_lev)
dev.off()

