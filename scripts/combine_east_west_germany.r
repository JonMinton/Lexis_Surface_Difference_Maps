

# code for combining counts from east and west germany

# 4) load human mortality database (HMD) data on population counts and death counts
# in the 'tidy data' format suggested by Hadley Wickham 
counts <- read.csv("data/tidy/counts.csv")
# (For the code used to convert the existing files to the 'tidy' please contact me)

counts_germany <- subset(counts,
                         subset=country %in% c("DEUTE", "DEUTW")
)
d_deaths <- recast(
  counts_germany,
  year + age + sex ~ country,
  id.var=c("country", "year", "age", "sex"),
  measure.var=c("death_count")
)
d_deaths <- transform(d_deaths, year=year, age=age, sex=sex, death_count=DEUTE + DEUTW)
d_deaths$DEUTE <- NULL
d_deaths$DEUTW <- NULL

d_populations <- recast(
  counts_germany,
  year + age + sex ~ country,
  id.var=c("country", "year", "age", "sex"),
  measure.var=c("population_count")
)
d_populations <- transform(d_populations, year=year, age=age, sex=sex, population_count=DEUTE + DEUTW)
d_populations$DEUTE <- NULL
d_populations$DEUTW <- NULL

d_combined <- join(d_populations, d_deaths)
d_combined$country <- "DEUT"

d_combined <- d_combined[c("country", "year", "age", "sex" ,"death_count", "population_count")] 

counts <- rbind(
  counts, 
  d_combined
)

write.csv(counts, file="data/tidy/counts_germany_combined.csv", row.names=F)

rm(d_deaths, d_populations, d_combined, counts_germany)

head(d_combined)
