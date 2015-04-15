## Code for working with GDP per capita data

require(stringr)
require(tidyr)
require(dplyr)


dta <- read.csv(
  file="data/gdp/ny.gdp.pcap.pp.cd_Indicator_en_csv_v2.csv", 
                header=T, skip=2) %>%
  tbl_df


dta_tidy <- dta %>%
  gather(key = "year", value="gdp_pc_ppp", 
         -Country.Name, -Country.Code, -Indicator.Name, -Indicator.Code
         ) %>%
  select(country_name=Country.Name, country_code=Country.Code, 
         year, gdp_pc_ppp, -Indicator.Name, -Indicator.Code) %>%
  filter(!is.na(gdp_pc_ppp)) 

dta_tidy$year <- str_replace(dta_tidy$year, "^X", "")
dta_tidy$year <- dta_tidy$year %>%
  as.character %>%
  as.numeric

write.csv(dta_tidy, file="data/gdp/gdp_tidy.csv", row.names=FALSE)
