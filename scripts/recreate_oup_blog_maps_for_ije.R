# Quick script replicating and improving the figures used in the blogs

#http://blog.oup.com/2013/09/demographic-landscape-good-news/

rm(list = ls())


# To do: England & Wales, Females, highlighted 0.01 mort risk



require(readr)
require(dplyr)
require(tidyr)
require(stringr)

require(car)

require(lattice)
require(latticeExtra)



dta <- read_csv("data/tidy/counts.csv")

p1 <- dta  %>% 
  filter(country == "GBRTENW")   %>% 
  filter(sex != "total")  %>%
  filter(age <= 80) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  contourplot(
  death_rate  ~ year * age | sex, 
  data= . , 
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab=list(label="Age in years", cex=1.4),
  xlab=list(label="Year", cex=1.4),
  cex=1.4,
  at = seq(from = 0.000, to = 0.20, by = 0.005),
  col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
  main=NULL,
  labels=NULL,
  col="grey",
  scales=list(
    x=list(cex=1.4), 
    y=list(cex=1.4),
    alternating=3
  )
)

p2 <- dta  %>% 
  filter(country == "GBRTENW")   %>% 
  filter(sex != "total")  %>%
  filter(age <= 80) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  contourplot(
    death_rate  ~ year * age | sex, 
    data= . , 
    region=F, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    at = c(0.00001, 0.01, 1),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=NULL,
    labels=NULL,
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )

png(
  filename = "figures/ije_blog_update_blog01.png",
  res = 300, 
  width=40, height=20, units="cm"
    )

print(p1 + p2) 

dev.off()



# blog 2
#http://blog.oup.com/2013/09/demographic-landscape-bad-news/


dta <- read_csv("data/tidy/counts.csv")


# fig a

dta <- read_csv("data/tidy/counts.csv")

p1 <- dta  %>% 
  filter(country == "GBRTENW")   %>% 
  filter(sex != "total")  %>%
  filter(age <= 80) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  contourplot(
    death_rate  ~ year * age | sex, 
    data= . , 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    at = seq(from = 0.000, to = 0.20, by = 0.005),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=NULL,
    labels=NULL,
    col="grey",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    panel = function(x, y, z, ...){
      panel.contourplot(x, y, z, ...)
      panel.rect(xleft = 1914, xright = 1921, ybottom = 17, ytop = 45, border = "darkred", lty="dashed")
      panel.rect(xleft = 1938, xright = 1946, ybottom = 17, ytop = 45, border = "darkgreen", lty="dashed")
      
    }
  )

p2 <- dta  %>% 
  filter(country == "GBRTENW")   %>% 
  filter(sex != "total")  %>%
  filter(age <= 80) %>% 
  mutate(birth_year = year - age) %>% 
  filter(
    (year >= 1914 & year <= 1921 & age >= 17 & age <= 45)  
      ) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  contourplot(
    death_rate  ~ year * age | sex, 
    data= . , 
    region=F, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    at = seq(from = 0.000, to = 0.20, by = 0.005),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=NULL,
    labels=NULL,
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )

p3 <- dta  %>% 
  filter(country == "GBRTENW")   %>% 
  filter(sex != "total")  %>%
  filter(age <= 80) %>% 
  mutate(birth_year = year - age) %>% 
  filter(
        (year >= 1938 & year <= 1946 & age >= 17 & age <= 45)
  ) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  contourplot(
    death_rate  ~ year * age | sex, 
    data= . , 
    region=F, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    at = seq(from = 0.000, to = 0.20, by = 0.005),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=NULL,
    labels=NULL,
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )

png(
  filename = "figures/ije_blog_update_blog02a.png",
  res = 300, 
  width=40, height=20, units="cm"
)

print(p1 + p2 + p3) 

dev.off()

# fig b


p1 <- dta  %>% 
  filter(country == "GBRTENW")   %>% 
  filter(sex != "total")  %>%
  filter(age <= 80) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  contourplot(
    death_rate  ~ year * age | sex, 
    data= . , 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    at = seq(from = 0.000, to = 0.20, by = 0.005),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=NULL,
    labels=NULL,
    col="grey",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    ),
    panel = function(x, y, z, ...){
      panel.contourplot(x, y, z, ...)
      panel.abline(a = -1914, b = 1, col = "red", lty ="dashed")
      panel.abline(a = -1921, b = 1, col = "blue", lty = "dashed")
      panel.abline(v = 1914, col = "red", lty = "dashed")
      panel.abline(v = 1921, col = "blue", lty = "dashed")
    }
  )

p2 <- dta  %>% 
  filter(country == "GBRTENW")   %>% 
  filter(sex != "total")  %>%
  filter(age <= 80) %>% 
  mutate(birth_year = year - age) %>% 
  filter((year >= 1914 & year <= 1921) | (birth_year >= 1914 & birth_year <= 1921)) %>% 
  mutate(death_rate = death_count / population_count) %>% 
  contourplot(
    death_rate  ~ year * age | sex, 
    data= . , 
    region=F, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    at = seq(from = 0.000, to = 0.20, by = 0.005),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=NULL,
    labels=NULL,
    col="black",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )

png(
  filename = "figures/ije_blog_update_blog02b.png",
  res = 300, 
  width=40, height=20, units="cm"
)

print(p1 + p2) 

dev.off()
