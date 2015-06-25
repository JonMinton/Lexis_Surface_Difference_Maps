# Scotland in Context Helper functions


make_clp_lattice <- function(DTA, DTA_overall, CODES){
  
  tmp1 <- DTA  %>% 
    mutate(cmr = death_count/ population_count)  %>% 
    select(country, year, age, sex, cmr)
  
  tmp2 <- DTA_overall %>%  
    select(year, age, sex, overall_cmr = cmr)
  
  tmp3 <- tmp1 %>% left_join(tmp2)
  
  dif_dta <- tmp3 %>% 
    mutate(dif_lg_cmr = log(cmr, base = 10) - log(overall_cmr, base = 10))
  rm(tmp1, tmp2, tmp3)
  
  dif_dta_blurred <- dif_dta %>% smooth_var(
    dta=.,
    group_vars= c("country", "sex"),
    smooth_var="dif_lg_cmr",
    smooth_par=1.4
  )
  
  
  lev_part <- dif_dta %>% filter(
    sex!="total"
    & age <=90 &
      year >=1900
  ) %>% 
    mutate(
      country = mapvalues(
        country,
        from=CODES,
        to=names(CODES)
      )
    ) %>%
    levelplot(
      dif_lg_cmr ~ year * age | country + sex,
      data=., 
      region=T,
      ylab="Age in years",
      xlab="Year",
      at = seq(from= -1.2, to = 1.2, by=0.2),
      col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
      scales=list(alternating=3),
      main=NULL,
      xlim=c(1900, 2010),
      par.settings=list(strip.background=list(col="lightgrey"))
    )
  
  
  zero_part <- dif_dta_blurred %>%
    filter(sex!="total" & age <=90) %>%
    mutate(
      country = mapvalues(
        country,
        from=CODES,
        to=names(CODES)
      )
    ) %>% 
    contourplot(
      dif_lg_cmr ~ year + age | country + sex, 
      data=.,
      region=F,
      ylab="",
      xlab="",
      scales=list(NULL),
      at=0,
      lwd=1,
      labels=F,
      xlim=c(1900, 2010),
      main=NULL
    )
  
  output <- lev_part + zero_part
  
  return(output)
  
}



make_single_clp <- function(DTA, DTA_overall, SELECTION){
  
  tmp1 <- DTA  %>% 
    filter(country == SELECTION)  %>% 
    mutate(cmr = death_count/ population_count)  %>% 
    mutate(country = "specific") %>% 
    select(country, year, age, sex, cmr)
  
  tmp2 <- DTA_overall  %>% 
    mutate(country = "overall" )  %>%  
    select(country, year, age, sex, cmr)
  
  tmp3 <- bind_rows(tmp1, tmp2)
  rm(tmp1, tmp2)
  
  dif_to_overall  <- tmp3 %>% 
    mutate(lg_cmr = log(cmr, base=10))  %>% 
    select(-cmr)  %>% 
    spread(key=country, value=lg_cmr)  %>% 
    filter(!is.na(specific))  %>% 
    mutate(dif = specific  - overall) %>% 
    select(year, age, sex, dif)
  
  lev_part <- dif_to_overall %>% filter(
    sex!="total"
    & age <=90 &
      year >=1900
  ) %>% 
    levelplot(
      dif ~ year * age | sex,
      data=., 
      region=T,
      xlim=c(1900, 2010),
      ylab="Age in years",
      xlab="Year",
      at = seq(from= -1.2, to = 1.2, by=0.2),
      col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
      scales=list(alternating=3),
      main=NULL,
      par.settings=list(strip.background=list(col="lightgrey"))
    )
  
  dif_blurred <- dif_to_overall %>% smooth_var(
    dta=.,
    group_vars= "sex",
    smooth_var="dif",
    smooth_par=1.4
  )
  
  
  zero_part <- dif_blurred %>%
    filter(sex!="total" & age <=90) %>%
    contourplot(
      dif ~ year + age | sex, 
      data=.,
      region=F,
      ylab="",
      xlab="",
      scales=list(NULL),
      at=0,
      lwd=1,
      labels=F,
      xlim=c(1900, 2010),
      main=NULL
    )
  
  quarter_part <- dif_blurred %>% 
    filter(sex != "total" & age <=90) %>% 
    contourplot(
      dif ~ year + age | sex, 
      data = . , 
      region = F,
      ylab = "", 
      xlab = "", 
      scales = list (NULL),
      at = c(-0.25, 0.25),
      lwd=1.5, 
      labels = F,
      xlim=c(1900, 2010),
      main = NULL
    )
  
  half_part <- dif_blurred %>% 
    filter(sex!="total" & age <=90) %>% 
    contourplot(
      dif ~ year + age | sex,
      data = . ,
      region = F,
      ylab="", 
      xlab="",
      scales = list (NULL),
      at =c(-0.5, 0.5),
      lwd=2.0,
      labels =F,
      xlim=c(1900, 2010),
      main=NULL
    )
  
  output <- lev_part + zero_part + quarter_part + half_part
  
  return(output)
}



make_scp_lattice <- function(DTA, DTA_smoothed, CODES){
  
  shade_part <- DTA %>%
    filter(
      year >= 1900 & year <= 2010 &
        age <= 90 &
        sex != "total"
    ) %>%
    mutate(
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base=10),
      country = mapvalues(
        country,
        from = CODES, 
        to   = names(CODES)
      )
    ) %>%
    levelplot(
      lg_cmr ~ year * age | country + sex, 
      data=., 
      region=T, 
      par.strip.text=list(cex=1.4, fontface="bold"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      col.regions=colorRampPalette(brewer.pal(6, "Reds"))(200),
      main=NULL,
      xlim=c(1900, 2010),
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      ),
      par.settings=list(strip.background=list(col="lightgrey"))
    )
  
  contour_part <- DTA_smoothed  %>%  
    filter(
      year >= 1900 & year <= 2008 &
        age <= 90
    ) %>%
    mutate(
      country = mapvalues(
        country,
        from = CODES, 
        to   = names(CODES)
      )
    ) %>%
    contourplot(
      lg_cmr ~ year + age | country + sex, 
      data=.,
      region=F,
      ylab="",
      xlab="",
      scales=list(NULL),
      cuts=25,    
      col="black",
      labels=list(
        cex=1.2
      ),
      main=NULL
    )
  
  output <- shade_part + contour_part
  
  return(output)
}



make_scp_overall <- function(DTA_unsmoothed, DTA_smoothed){
  shade_part <- DTA_unsmoothed %>%
    filter(
      year >= 1900 & year <= 2010 &
        age <= 90 &
        sex != "total"
    ) %>%
    mutate(
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base=10)
    ) %>%
    levelplot(
      lg_cmr ~ year * age | sex, 
      data=., 
      region=T, 
      par.strip.text=list(cex=1.4, fontface="bold"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      col.regions=colorRampPalette(brewer.pal(6, "Reds"))(200),
      main=NULL,
      xlim=c(1900, 2010),
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      ),
      par.settings=list(strip.background=list(col="lightgrey"))
    )
  
  contour_part <- DTA_smoothed  %>%  
    filter(
      year >= 1900 & year <= 2008 &
        age <= 90 
    ) %>%
    contourplot(
      lg_cmr ~ year + age | sex, 
      data=.,
      region=F,
      ylab="",
      xlab="",
      xlim=c(1900, 2010),
      scales=list(NULL),
      cuts=25,
      col="black",
      labels=list(
        cex=1.2
      ),
      main=NULL
    )
  
  output <- shade_part + contour_part
}



make_scp <- function(DTA_unsmoothed, DTA_smoothed, COUNTRY){
  shade_part <- DTA_unsmoothed %>%
    filter(
      country == COUNTRY & 
        year >= 1900 & year <= 2010 &
        age <= 90 &
        sex != "total"
    ) %>%
    mutate(
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base=10)
    ) %>%
    levelplot(
      lg_cmr ~ year * age | sex, 
      data=., 
      region=T, 
      par.strip.text=list(cex=1.4, fontface="bold"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      col.regions=colorRampPalette(brewer.pal(6, "Reds"))(200),
      main=NULL,
      xlim=c(1900, 2010),
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      ),
      par.settings=list(strip.background=list(col="lightgrey"))
    )
  
  contour_part <- DTA_smoothed  %>%  
    filter(
      country == COUNTRY & 
        year >= 1900 & year <= 2008 &
        age <= 90 
    ) %>%
    contourplot(
      lg_cmr ~ year + age | sex, 
      data=.,
      region=F,
      ylab="",
      xlab="",
      xlim=c(1900, 2010),
      scales=list(NULL),
      cuts=25,
      col="black",
      labels=list(
        cex=1.2
      ),
      main=NULL
    )
  
  output <- shade_part + contour_part
}


grouper <- function(DTA){
  output <- DTA %>% 
    group_by(age, sex, year) %>%
    summarise(
      death_count = sum(death_count),
      population_count = sum(population_count)
    ) %>%
    mutate(
      cmr = death_count / population_count,
      lg_cmr = log(cmr, base=10)     
    )
  
  return(output)  
}



make_two_country_clp <- function(DTA, GROUP_A, GROUP_B, 
                                 YEAR_RANGE = c(1900, 2010), 
                                 AGE_RANGE = c(0, 90),
                                 ASPECT = "iso",
                                 SMOOTH_PAR= 1.4
                                 ){
  
  tmp1 <- DTA  %>% 
    filter(country == GROUP_A)  %>% 
    mutate(cmr = death_count/ population_count)  %>% 
    mutate(country = "group_a") %>% 
    select(country, year, age, sex, cmr)
  
  tmp2 <- DTA  %>% 
    filter(country == GROUP_B)  %>% 
    mutate(cmr = death_count/ population_count)  %>% 
    mutate(country = "group_b") %>% 
    select(country, year, age, sex, cmr)
  
  tmp3 <- bind_rows(tmp1, tmp2)
  rm(tmp1, tmp2)
  
  dif_b_to_a  <- tmp3 %>% 
    mutate(lg_cmr = log(cmr, base=10))  %>% 
    select(-cmr)  %>% 
    spread(key=country, value=lg_cmr)  %>% 
    filter(!is.na(group_a) & !is.na(group_b))  %>% 
    mutate(dif = group_b  - group_a) %>% 
    select(year, age, sex, dif)
  
  lev_part <- dif_b_to_a %>% filter(
    sex!="total"
    & age >= AGE_RANGE[1] & age <= AGE_RANGE[2] &
      year >= YEAR_RANGE[1] & year <= YEAR_RANGE[2]
  ) %>% 
    levelplot(
      dif ~ year * age | sex,
      data=., 
      region=T,
      xlim=YEAR_RANGE,
      ylab="Age in years",
      xlab="Year",
      aspect=ASPECT,
      at = seq(from= -1.2, to = 1.2, by=0.2),
      col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
      scales=list(alternating=3),
      main=NULL,
      par.settings=list(strip.background=list(col="lightgrey"))
    )
  
  dif_blurred <- dif_b_to_a %>% smooth_var(
    dta=.,
    group_vars= "sex",
    smooth_var="dif",
    smooth_par=SMOOTH_PAR
  )
  
  
  zero_part <- dif_blurred %>%
    filter(sex!="total" 
           & age >= AGE_RANGE[1] & age <= AGE_RANGE[2] &
             year >= YEAR_RANGE[1] & year <= YEAR_RANGE[2]
           ) %>%
    contourplot(
      dif ~ year + age | sex, 
      data=.,
      region=F,
      ylab="",
      xlab="",
      scales=list(NULL),
      at=0,
      lwd=1,
      labels=F,
      xlim=YEAR_RANGE,
      aspect=ASPECT,
      main=NULL
    )
  

  output <- lev_part + zero_part 
  
  return(output)
}

