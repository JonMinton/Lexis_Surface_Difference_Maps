

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
# contour plot, all Europe, log mortality----------------------------------


png(filename="figures/mort_all_europe_reds.png", 
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
  col.regions=colorRampPalette(brewer.pal(6, "Reds"))(200),
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

png(filename="figures/mort_all_europe_bupu.png", 
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
  col.regions=colorRampPalette(brewer.pal(6, "BuPu"))(200),
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

png(filename="figures/mort_all_europe_spectral.png", 
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
  col.regions=colorRampPalette(brewer.pal(6, "Spectral"))(200),
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

png(filename="figures/mort_all_europe_rdylbu.png", 
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
  col.regions=colorRampPalette(brewer.pal(6, "RdYlBu"))(200),
  main=NULL,
  labels=list(cex=1.2),
  col="darkgreen",
  scales=list(
    x=list(cex=1.4), 
    y=list(cex=1.4),
    alternating=3
  )
)
dev.off()

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
  dta[is.infinite(dta) & dta < 0] <- min(dta[is.finite(dta)]) # correct for infinities
  dta[is.infinite(dta) & dta > 0] <- max(dta[is.finite(dta)])
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

dif_logs_blurred <- dif_logs %>%
   select(-europe) %>%
  gather(key=country, value=lmort, -year, -age, -sex) %>%  
  ddply(.data = ., .(sex, country), fn, smooth_par=1.5) %>%
  tbl_df 



dif_logs <- dif_logs %>%
  gather(key=country, value=lmort, -year, -age, -sex) %>%
  mutate(lmort = ifelse(lmort < -1.2, -1.2, lmort),
         lmort= ifelse(lmort > 1.2, 1.2, lmort)) 

dif_ident <- diffs %>%
  gather(key=country, value=mort, -year, -age, -sex)

dif_logs$country <- dif_logs$country  %>% mapvalues(from="enw", to="England & Wales" )
dif_ident$country <- dif_ident$country %>%
  mapvalues(from="enw", to="England & Wales")


# clp - composite plot, log -----------------------------------------------

png(
  "figures/clp_all.png",  
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
  filter(sex!="total" & country !="europe" & age <=90) %>%
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



# clp - composite plot, identity ------------------------------------------


png(
  "figures/clp_all_ident.png",  
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
  filter(sex!="total" & country !="europe" & age <=90) %>%
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



