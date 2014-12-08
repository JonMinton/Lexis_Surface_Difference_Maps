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
  subset=year >=1950 & year <=2010 & age <=90 
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





###########################################################
###########################################################
# tiff(
#   "figures/france.tiff",  
#   height=1000, width=2000
# )
france_lev <- levelplot(
  france ~ year * age | sex, 
  data=subset(dif_logs, subset=sex!="total"),
  region=T, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="age",
  xlab="year",
  cex=1.4,
  at = seq(from= -1, to = 1, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
  main=NULL
)
print(france_lev)
# dev.off()

tiff(
  "figures/norway.tiff",  
  height=1000, width=2000
)
norway_lev <- levelplot(
  norway ~ year * age | sex , 
  data=subset(dif_logs, subset=sex!="total"),
  region=T, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="age",
  xlab="year",
  cex=1.4,
  at = seq(from= -1, to = 1, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
  main=NULL
)
print(norway_lev)
dev.off()

tiff(
  "figures/eng_wales.tiff",  
  height=1000, width=2000
)
eng_lev <- levelplot(
  england_and_wales ~ year * age | sex , 
  data=subset(dif_logs, subset=sex!="total"),
  region=T, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="age",
  xlab="year",
  cex=1.4,
  at = seq(from= -1, to = 1, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
  main=NULL
)
print(eng_lev)
dev.off()

tiff(
  "figures/scotland.tiff",  
  height=1500, width=3000
)
scot_lev <- levelplot(
  scotland ~ year * age | sex, 
  data=subset(dif_logs, subset=sex!="total"),
  region=T, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="age",
  xlab="year",
  cex=1.4,
  at = seq(from= -1, to = 1, by=0.2),
  col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
  main=NULL
)
print(scot_lev)
dev.off()

tiff(
  "figures/all_ident.tiff",  
  height=1500, width=3000
)
europe_ident <- contourplot(
  europe~ year * age | sex, 
  data=subset(mort_eu, subset=sex!="total" & 
                age <=80 & year >=1950 & year <=2010 ), 
  region=T, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="age",
  xlab="year",
  cex=1.4,
  cuts=50,
  col.regions=rev(heat.colors(200)),
  main=NULL
)
print(europe_ident)
dev.off()

tiff(
  "figures/all_log.tiff",  
  height=1500, width=3000
)

europe_log <- contourplot(
  log(europe) ~ year * age | sex, 
  data=subset(mort_eu, subset=sex!="total" & 
                age <=80 & year >=1950 & year <=2010 ), 
  region=T, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="age",
  xlab="year",
  cex=1.4,
  cuts=50,
  col.regions=rev(heat.colors(200)),
  main=NULL
)
print(europe_log)
dev.off()

##############################################################################
##############################################################################

# fig_france <- contourplot(
#   france ~ year * age | sex, 
#   data=subset(dif_logs, subset=sex!="total"),
#   region=T, 
#   par.strip.text=list(cex=1.2, fontface="bold"),
#   ylab="age",
#   xlab="year",
#   cex=1.4,
#   at = seq(from= -1, to = 1, by=0.2),
#   col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
#   main=NULL
# )
# print(fig_france)
# 
# fig_norway <- contourplot(
#   norway ~ year * age | sex , 
#   data=subset(dif_logs, subset=sex!="total"),
#   region=T, 
#   par.strip.text=list(cex=1.2, fontface="bold"),
#   ylab="age",
#   xlab="year",
#   cex=1.4,
#   at = seq(from= -1, to = 1, by=0.2),
#   col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
#   main=NULL
# )
# print(fig_norway)
# 
# fig_engwales <- contourplot(
#   england_and_wales ~ year * age | sex , 
#   data=subset(dif_logs, subset=sex!="total"),
#   region=T, 
#   par.strip.text=list(cex=1.2, fontface="bold"),
#   ylab="age",
#   xlab="year",
#   cex=1.4,
#   at = seq(from= -1, to = 1, by=0.2),
#   col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
#   main=NULL
# )
# print(fig_engwales)
# 
# fig_scot <- contourplot(
#   scotland ~ year * age | sex, 
#   data=subset(dif_logs, subset=sex!="total"),
#   region=T, 
#   par.strip.text=list(cex=1.2, fontface="bold"),
#   ylab="age",
#   xlab="year",
#   cex=1.4,
#   at = seq(from= -1, to = 1, by=0.2),
#   col.regions = colorRampPalette(rev(brewer.pal(5, "RdBu")))(64),
#   main=NULL
# )
# print(fig_scot)
# 
# 
# 








#####################################################################################
# SOURCE DATA



# 28 September 2014

# 4) load human mortality database (HMD) data on population counts and death counts
# in the 'tidy data' format suggested by Hadley Wickham 
counts <- read.csv("data/counts.csv")
# (For the code used to convert the existing files to the 'tidy' please contact me)

# 5) load a file which shows which of the HMD countries are part of Europe

country_codes <- read.csv("Data/country_codes__new.csv", stringsAsFactors=F)

europe_codes <- country_codes$short[country_codes$europe==1]
######################################################################################
# DERIVED DATA


# What is the first and last year observed for each county?

ddply(counts, .(country), summarise, first_year=min(year), last_year=max(year))


# Now to decide which countries to keep in

# countries_to_keep <- c(
#   "AUT",  "BEL",    "BGR",  "BLR",  
#   "CHE",  "CZE",    "DEUTE","DEUTNP", 
#   "DEUTW",  "DNK",    "ESP",    "EST",    
#   "FIN",    "FRATNP" ,  "GBR_NIR", "GBR_SCO",
#   "GBRTENW","HUN",  "IRL","ITA",
#   "LTU","LUX",  "LVA",  "NLD",
#   "NOR",  "POL",  "PRT",  "RUS",
#   "SVK",  "SVN",  "SWE",  "UKR"
#   )

# The UK and Ireland 

countries_to_keep <- c(
  "GBRTENW",
  "GBR_NIR",
  "GBR_SCO",
  "IRL",
  "BEL",
  "NLD"
  )

counts_s <- subset(
  counts,
  subset=country %in% countries_to_keep
  )


# 
# # to do : 
# #  - combine east and west germany from before 1990 to create germany then
# 
# 
# # 10   DEUTE       1956      2011
# # 11  DEUTNP       1990      2011
# # 12   DEUTW       1956      2011
# 
# tmp <- ddply(
#   counts_s,
#   .(year, age, sex),
#   summarise,
#   death_count=death_count[country=="DEUTE"] + death_count[country=="DEUTW"],
#   population_count = population_count[country=="DEUTE"] + population_count[country=="DEUTW"]
#   )
# 
# tmp <- data.frame(
#   country="Germany",
#   tmp
#   )
# 
# counts_s <- rbind(counts_s, tmp)
# rm(tmp)
# counts_s <- subset(counts_s, subset=(country!="DEUTE") & (country!="DEUTW"))
# 
# counts_s <- merge(
#   x=counts_s,
#   y=country_codes, by.x="country", by.y="short", all.x=T
#   )
# 
# counts_s$country <- NULL
# counts_s$europe <- NULL
# counts_s <- rename(counts_s, c("long"= "country"))

counts_s <- subset(counts_s, subset=sex!="total")
counts_s <- subset(counts_s, year >= 1930)
counts_s <- subset(counts_s, subset=age <=80)

rates <- mutate(counts_s, death_rate=death_count / population_count)


rates$country <- revalue(
  rates$country,
  replace=c(
    "GBR_NIR"="Northern Ireland",
    "GBR_SCO"="Scotland",
    "GBRTENW"="England & Wales",
    "IRL"="Ireland",
    "BEL"="Belgium",
    "NLD"="The Netherlands"
    )
  )




# I want all european countries from 

# 
# # For each of eight countries 
# 
# Scotland
# England & Wales
# Northern Ireland
# Ireland
# Netherlands
# Belgium
# Germany
# France
# Denmark
# Norway


# What I want
# For Each of Ireland, Northern Ireland
# England & Wales
# Scotland
# France
# Holland
# Belgium



# From 1930 to 2010
# for ages 0 to 80

# Using a common scale
# Plot males and females side by side



# Range should be from 0 to 0.2

lims <- seq(0,0.2, length=50)


# 
# # Figure 1: Contour plot
# #lattice.options(default.theme = standard.theme(color = FALSE))
# 
# # MONOCHROME VERSION OF GRAPH, FOR PRINT
  tiff(
    "figures/male_ireland.tiff",  
    height=1000, width=1000
  )

g1 <- contourplot(
  death_rate ~ year * age , 
  data=subset(rates, subset= country=="Ireland" & sex=="male"), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  at=lims,
  xlim=c(1930, 2010),
  ylab="age",
  xlab="year",
  colorkey=NULL,
  main=NULL
  )
print(g1)
# 
dev.off()
# 

tiff(
  "figures/female_ireland.tiff",  
  height=1000, width=1000
)

g1 <- contourplot(
  death_rate ~ year * age , 
  data=subset(rates, subset= country=="Ireland" & sex=="female"), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  xlim=c(1930, 2010),
  ylab="age",
  xlab="year",
  cex=1.4,
  main=NULL
)
print(g1)
# 
dev.off()

# # Figure 1: Contour plot
# #lattice.options(default.theme = standard.theme(color = FALSE))
# 
# # MONOCHROME VERSION OF GRAPH, FOR PRINT
tiff(
  "figures/male_northern_ireland.tiff",  
  height=1000, width=1000
)

g1 <- contourplot(
  death_rate ~ year * age , 
  data=subset(rates, subset= country=="Northern Ireland" & sex=="male"), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  xlim=c(1930, 2010),
  ylab="age",
  xlab="year",
  cex=1.4,
  main=NULL
)
print(g1)
# 
dev.off()
# 
tiff(
  "figures/female_northern_ireland.tiff",  
  height=1000, width=1000
)

g1 <- contourplot(
  death_rate ~ year * age , 
  data=subset(rates, subset= country=="Northern Ireland" & sex=="female"), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  xlim=c(1930, 2010),
  ylab="age",
  xlab="year",
  cex=1.4,
  main=NULL
)
print(g1)
# 
dev.off()
# 
tiff(
  "figures/female_ireland.tiff",  
  height=1000, width=1000
)

g1 <- contourplot(
  death_rate ~ year * age , 
  data=subset(rates, subset= country=="Ireland" & sex=="female"), 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  xlim=c(1930, 2010),
  ylab="age",
  xlab="year",
  cex=1.4,
  main=NULL
)
print(g1)
# 
dev.off()

# COLOUR VERSION OF GRAPH, FOR ONLINE VERSION OF PAPER
trellis.device(
  tiff(
    "figures/fig_01COLOUR__contour_all_europe.tiff",  
    height=1000, width=2000
  ),
  color = TRUE
)
g1 <- contourplot(
  death_rate ~ year * age | sex, 
  data=rates, 
  region=T, 
  col.regions=rev(heat.colors(200)), 
  #  col.regions=rev(gray(0:199/199)),
  cuts=50, 
  par.strip.text=list(cex=1.2, fontface="bold"),
  ylab="single age of death",
  xlab="single year in which population and death counts were enumerated",
  cex=1.4,
  
  main=NULL)
print(g1)

dev.off()

# # Mo
