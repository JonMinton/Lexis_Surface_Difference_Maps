# Raw Data  ---------------------------------------------------------------


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


# derived data ------------------------------------------------------------

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


# using mrate_aggregated and mrate_each_country
make_evx <- function(counts, ex=0, gdp=gdp){
  
  vitstat_all <- counts %>%
    filter(sex != "total") %>%
    group_by(sex, year, age) %>%
    summarise(
      n_countries=length(death_count),
      death_count=sum(death_count),
      population_count=sum(population_count)
    )
  
  meandeath_ex <- vitstat_all %>%
    filter(age >= ex) %>%
    group_by(sex, year) %>%
    summarise(mean_death = sum(age * death_count)/sum(death_count)) 
  
  varpar_ex <- vitstat_all %>%
    filter(age >= ex) %>%
    group_by(sex, year) %>%
    summarise(var_par_death = sum(death_count * age^2)/sum(death_count))
  
  vardeath_ex <- varpar_ex %>%
    inner_join(meandeath_ex) %>%
    mutate(var_death = var_par_death - mean_death^2)
  
  
  meandeath_all_ex <- meandeath_ex %>%
    mutate(country= "all")
  
  meandeath_each_ex <- counts %>%
    filter(sex != "total" & age >= ex) %>%
    group_by(country, year, sex) %>%
    summarise(mean_death= sum(death_count *age) /sum(death_count))
  
  varpar_each_ex <- counts %>%
    filter(sex != "total" & age >= ex) %>%
    group_by(country, year, sex) %>%
    summarise(varpar = sum(age^2*death_count) / sum(death_count))
  
  var_each_ex <- meandeath_each_ex %>%
    inner_join(varpar_each_ex) %>%
    mutate(var_death =varpar - mean_death^2) %>%
    select(country, year, sex, mean_death, var_death)
  
  mnvar_merged_ex <- vardeath_ex %>%
    select(sex, year, mean_death_overall=mean_death, var_death_overall=var_death) %>%
    inner_join(var_each_ex)
  
  dif_mnvars_ex <- mnvar_merged_ex %>%
    mutate(
      dif_mean=mean_death-mean_death_overall, 
      dif_var=var_death-var_death_overall,
      country_code=as.character(country)
    )
  
  dif_mnvars_ex$country_code[dif_mnvars_ex$country_code=="FRATNP"] <- "FRA"
  dif_mnvars_ex$country_code[dif_mnvars_ex$country_code=="GBRTENW"] <- "GBR"
  dif_mnvars_ex$country_code[dif_mnvars_ex$country_code=="GBR_NIR"] <- "GBR"
  dif_mnvars_ex$country_code[dif_mnvars_ex$country_code=="GBR_SCO"] <- "GBR"
  dif_mnvars_ex$country_code[dif_mnvars_ex$country_code=="DEUT"] <- "DEU"
  
  tmp <- gdp %>%
    group_by(country_code) %>%
    filter(year==max(year)) %>%
    select(country_code, gdp=gdp_pc_ppp)
  
  dif_mnvars_ex <- dif_mnvars_ex  %>%
    left_join(tmp) %>%
    ungroup
  
  dif_mnvars_ex$country <- as.factor(dif_mnvars_ex$country)
  dif_mnvars_ex$country <-  reorder(x=dif_mnvars_ex$country, X=dif_mnvars_ex$gdp)
  dif_mnvars_ex$country <- droplevels(dif_mnvars_ex$country)
  levels(dif_mnvars_ex$country) <- c(
    "Bulgaria", "Latvia", "Hungary", "Poland", "Lithuania", "Estonia", "Slovakia", "Portugal",
    "Slovenia", "Czech Republic",  "Spain", "Italy", "France", "England & Wales", 
    "Northern Ireland", "Scotland",
    "Finland", "Belgium", "Denmark", "Germany", "Sweden", "Austria", "Ireland", "Netherlands",
    "Norway", "Luxembourg"
  )
  
  dif_mnvarx_ex <- dif_mnvars_ex %>%
    arrange(country, sex, year)
  
  return(dif_mnvars_ex)
}



dif_mnvars_e0 <- make_evx(counts_eu, ex=0, gdp)
dif_mnvars_e5 <- make_evx(counts_eu, ex=5, gdp)
dif_mnvars_e65 <- make_evx(counts_eu, ex=65, gdp)


rms_e0 <- dif_mnvars_e0 %>%
  filter(year >= 1950) %>%
  select(country, sex, year, dif_mean) %>%
  group_by(sex, year) %>%
  summarise(rms = (mean(dif_mean^2, na.rm=T)^0.5))


rms_e5 <- dif_mnvars_e5 %>%
  filter(year >= 1950) %>%
  select(country, sex, year, dif_mean) %>%
  group_by(sex, year) %>%
  summarise(rms = (mean(dif_mean^2, na.rm=T)^0.5))

rms_e65 <- dif_mnvars_e65 %>%
  filter(year >= 1950) %>%
  select(country, sex, year, dif_mean) %>%
  group_by(sex, year) %>%
  summarise(rms = (mean(dif_mean^2, na.rm=T)^0.5))



rm(mrate_each_country) # mrate_joined includes all these vars and more
