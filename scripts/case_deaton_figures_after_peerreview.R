# Final figures for Case & Deaton exploration paper

# Prereqs -----------------------------------------------------------------

rm(list=ls())

# data management
require(readr)

require(plyr)
require(tidyr)
require(stringr)
require(dplyr)


#graphics
require(lattice)
require(latticeExtra)
require(ggplot2) 
require(RColorBrewer)
require(grid)

# for smoothing
require(fields) 
require(spatstat)

#tables
require(xtable)


# functions ---------------------------------------------------------------
source("scripts/smoother_function.R")
source("scripts/scotland_in_context__helper_functions.R")



# Case Deaton with CDC figures --------------------------------------------

dta <- read_csv("data/usa_multiple_cause/tidied_and_simplified.csv")


# Complete list of figures required ---------------------------------------


# ALL AS TIFFS 


# 1) all cause, log 10, wnh only, colour *
# 2) all cause, log 10, wnh only, grey * 

# 3) all cause, log 10, all groups, colour * 
# 4) all cause, log 10, all groups, grey * 

# 5) suicides, ident, all groups, colour * 
# 6) suicides, ident, all groups, grey * 

# 7) drugs, ident, all groups, colour * 
# 8) drugs, ident, all groups, grey * 

# 9) vehicles, ident, all groups, colour
# 10) vehicles, ident, all groups, grey



# 1) all cause, log 10, whn only, colour 

tiff(
  filename = "figures/case_deaton/revision/cdc_all_cause_wnhonly_colour.tiff",
  width = 15, height = 35, res = 300, units = "cm"
     )

dta %>% 
  filter(group == "White Non-Hispanic") %>% 
  mutate(all_cause_rate = all_cause / population,
         lmr = log(all_cause_rate, 10)
  ) %>% 
  contourplot(
    lmr ~ year * age | sex,
    data = . ,
    region=T, 
    ylab=list(label="Age in Years", cex=1.4),
    xlab=list(label="Year", cex=1.4 ),
    cex=1.4,
    cuts =20,
    aspect="iso",
    # col.regions = rev(gray(1:100/100)),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label = "All-cause mortality (log base 10)\nWhite-non Hispanics (WNH) only", cex = 1.4),
    xlim=c(1999, 2013),
    ylim=c(0, 80), 
    scales=list(
      x=list(cex=1.4, rot =90 ), 
      y=list(cex=1.4, at = seq(10, 70, by = 10)),
      alternating=1
    ),
    colorkey = list(labels = list(cex = 1.4)),
    horizontal= T,
    strip.left = T,
    strip = F,
    par.strip.text = list(cex = 1.4),
    par.settings=list(
      strip.background=list(col="lightgrey")
    ),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )

dev.off()


# 2) all cause, log 10, whn only, grey 

tiff(
  filename = "figures/case_deaton/revision/cdc_all_cause_wnhonly_grey.tiff",
  width = 15, height = 35, res = 300, units = "cm"
)

dta %>% 
  filter(group == "White Non-Hispanic") %>% 
  mutate(all_cause_rate = all_cause / population,
         lmr = log(all_cause_rate, 10)
  ) %>% 
  contourplot(
    lmr ~ year * age | sex,
    data = . ,
    region=T, 
    ylab=list(label="Age in Years", cex=1.4),
    xlab=list(label="Year", cex=1.4 ),
    cex=1.4,
    cuts =20,
    aspect="iso",
    col.regions = rev(gray(1:100/100)),
    #col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label = "All-cause mortality (log base 10)\nWhite-non Hispanics (WNH) only", cex = 1.4),
    xlim=c(1999, 2013),
    ylim=c(0, 80), 
    scales=list(
      x=list(cex=1.4, rot =90 ), 
      y=list(cex=1.4, at = seq(10, 70, by = 10)),
      alternating=1
    ),
    colorkey = list(labels = list(cex = 1.4)),
    horizontal= T,
    strip.left = T,
    strip = F,
    par.strip.text = list(cex = 1.4),
    par.settings=list(
      strip.background=list(col="lightgrey")
    ),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )

dev.off()



# 3) all cause, log 10, all groups, colour

tiff(filename="figures/case_deaton/revision/cdc_all_cause_colour.tiff", 
    width=15, height=35, res=300, units="cm"
)

dta %>% 
  #  filter(group == "White Non-Hispanic") %>% 
  mutate(all_cause_rate = all_cause / population,
         lmr = log(all_cause_rate, 10)
  ) %>% 
  contourplot(
    lmr ~ year * age | sex + group,
    data = . ,
    region=T, 
    ylab=list(label="Age in Years", cex=1.4),
    xlab=list(label="Year", cex=1.4 ),
    cex=1.4,
    cuts =20,
    aspect="iso",
    # col.regions = rev(gray(1:100/100)),
      col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label = "All cause (log base 10)", cex = 1.4),
    xlim=c(1999, 2013),
    layout = c(2, 3), 
    ylim=c(0, 80), 
    scales=list(
      x=list(cex=1.4, rot =90 ), 
      y=list(cex=1.4, at = seq(10, 70, by = 10)),
      alternating=1
    ),
    colorkey = list(labels = list(cex = 1.4)),
    horizontal= T,
    strip.left = T,
    strip = F,
    par.settings=list(
      strip.background=list(col="lightgrey")
    ),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )
dev.off()

# 4) all cause, log 10, all groups, grey

tiff(filename="figures/case_deaton/revision/cdc_all_cause_grey.tiff", 
    width=15, height=35, res=300, units="cm"
)

dta %>% 
  #  filter(group == "White Non-Hispanic") %>% 
  mutate(all_cause_rate = all_cause / population,
         lmr = log(all_cause_rate, 10)
  ) %>% 
  contourplot(
    lmr ~ year * age | sex + group,
    data = . ,
    region=T, 
    ylab=list(label="Age in Years", cex=1.4),
    xlab=list(label="Year", cex=1.4 ),
    cex=1.4,
    cuts =20,
    aspect="iso",
    col.regions = rev(gray(1:100/100)),
    #col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label = "All cause (log base 10)", cex = 1.4),
    xlim=c(1999, 2013),
    layout = c(2, 3), 
    ylim=c(0, 80), 
    scales=list(
      x=list(cex=1.4, rot =90 ), 
      y=list(cex=1.4, at = seq(10, 70, by = 10)),
      alternating=1
    ),
    colorkey = list(labels = list(cex = 1.4)),
    horizontal= T,
    strip.left = T,
    strip = F,
    par.settings=list(
      strip.background=list(col="lightgrey")
    ),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )
dev.off()



# 5) suicides, ident, all groups, colour
tiff(filename="figures/case_deaton/revision/suicides_colour.tiff", 
    width=15, height=35, res=300, units="cm"
)

dta %>% 
  mutate(suicide_rate = 100000 * suicides / population) %>% 
  select(year, age, sex, group, suicide_rate) %>% 
  smooth_var(group_vars = c("sex", "group"), smooth_par = 0.7,
             smooth_var = "suicide_rate") %>% 
  levelplot(
    suicide_rate ~ year * age | sex + group,
    data = . ,
    region=T, 
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    layout = c(2,3),
    at = seq(0, 50, by = 2),
    aspect="iso",
    #col.regions = rev(gray(1:100/100)),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label="Suicides", cex = 1.4),
    xlim=c(1999, 2013),
    ylim=c(0, 80), 
    strip = F, 
    strip.left = T,
    colorkey = list(label = list(cex=1.4)),
    scales=list(
      y=list(cex=1.4, at = seq(10, 70, by =10)), 
      x=list(cex=1.4, rot = 90),
      alternating=1
    ),
    par.settings=list(strip.background=list(col="lightgrey")),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )
dev.off()


# 6) suicides, ident, all groups, grey
tiff(filename="figures/case_deaton/revision/suicides_grey.tiff", 
    width=15, height=35, res=300, units="cm"
)

dta %>% 
  mutate(suicide_rate = 100000 * suicides / population) %>% 
  select(year, age, sex, group, suicide_rate) %>% 
  smooth_var(group_vars = c("sex", "group"), smooth_par = 0.7,
             smooth_var = "suicide_rate") %>% 
  levelplot(
    suicide_rate ~ year * age | sex + group,
    data = . ,
    region=T, 
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    layout = c(2,3),
    at = seq(0, 50, by = 2),
    aspect="iso",
    col.regions = rev(gray(1:100/100)),
    #col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label="Suicides", cex = 1.4),
    xlim=c(1999, 2013),
    ylim=c(0, 80), 
    strip = F, 
    strip.left = T,
    colorkey = list(label = list(cex=1.4)),
    scales=list(
      y=list(cex=1.4, at = seq(10, 70, by =10)), 
      x=list(cex=1.4, rot = 90),
      alternating=1
    ),
    par.settings=list(strip.background=list(col="lightgrey")),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )
dev.off()

# 7) drugs, ident, all groups, colour

tiff(filename="figures/case_deaton/revision/drug_colour.tiff", 
    width=15, height=35, res=300, units="cm"
)

dta %>% 
  mutate(drug_induced_rate = 100000 * drug_induced / population
  ) %>% 
  smooth_var(., group_vars = c("sex", "group"), smooth_var = "drug_induced_rate", smooth_par = 0.7) %>% 
  levelplot(
    drug_induced_rate ~ year * age | sex + group,
    data = . ,
    region=T, 
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts =15,
    aspect="iso",
    #col.regions = rev(gray(1:100/100)),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label="Drugs", cex = 1.4),
    xlim=c(1999, 2013),
    layout = c(2, 3),
    ylim=c(0, 80), 
    scales=list(
      y=list(cex=1.4, at = seq(10, 70, by =10)), 
      x=list(cex=1.4, rot = 90),
      alternating=1
    ),
    strip.left = T,
    strip = F,
    colorkey = list(label = list(cex = 1.4)),
    par.settings=list(strip.background=list(col="lightgrey")),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )
dev.off()
# 8) drugs, ident, all groups, grey
tiff(filename="figures/case_deaton/revision/drug_grey.tiff", 
     width=15, height=35, res=300, units="cm"
)

dta %>% 
  mutate(drug_induced_rate = 100000 * drug_induced / population
  ) %>% 
  smooth_var(., group_vars = c("sex", "group"), smooth_var = "drug_induced_rate", smooth_par = 0.7) %>% 
  levelplot(
    drug_induced_rate ~ year * age | sex + group,
    data = . ,
    region=T, 
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts =15,
    aspect="iso",
    col.regions = rev(gray(1:100/100)),
    #col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label="Drugs", cex = 1.4),
    xlim=c(1999, 2013),
    layout = c(2, 3),
    ylim=c(0, 80), 
    scales=list(
      y=list(cex=1.4, at = seq(10, 70, by =10)), 
      x=list(cex=1.4, rot = 90),
      alternating=1
    ),
    strip.left = T,
    strip = F,
    colorkey = list(label = list(cex = 1.4)),
    par.settings=list(strip.background=list(col="lightgrey")),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )
dev.off()



# 9) vehicles, ident, all groups, colour

tiff(filename="figures/case_deaton/revision/vehicle_colour.tiff", 
    width=15, height=35, res=300, units="cm"
)

dta %>% 
  mutate(cardeath_rate = 100000 * vehicle_transport / population
  ) %>% 
  smooth_var(., group_vars = c("sex", "group"), smooth_var = "cardeath_rate", smooth_par = 0.7) %>% 
  levelplot(
    cardeath_rate ~ year * age | sex + group,
    data = . ,
    region=T, 
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts =15,
    aspect="iso",
    #col.regions = rev(gray(1:100/100)),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label = "Vehicles", cex = 1.4),
    xlim=c(1999, 2013),
    layout = c(2, 3),
    ylim=c(0, 80), 
    scales=list(
      y=list(cex=1.4, at = seq(10, 70, by =10)), 
      x=list(cex=1.4, rot = 90),
      alternating=1
    ),
    strip.left = T,
    colorkey = list(label = list(cex = 1.4)),
    strip = F,
    par.settings=list(strip.background=list(col="lightgrey")),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )
dev.off()

# 10) vehicles, ident, all groups, grey

tiff(filename="figures/case_deaton/revision/vehicle_grey.tiff", 
    width=15, height=35, res=300, units="cm"
)

dta %>% 
  mutate(cardeath_rate = 100000 * vehicle_transport / population
  ) %>% 
  smooth_var(., group_vars = c("sex", "group"), smooth_var = "cardeath_rate", smooth_par = 0.7) %>% 
  levelplot(
    cardeath_rate ~ year * age | sex + group,
    data = . ,
    region=T, 
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts =15,
    aspect="iso",
    col.regions = rev(gray(1:100/100)),
    #col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label = "Vehicles", cex = 1.4),
    xlim=c(1999, 2013),
    layout = c(2, 3),
    ylim=c(0, 80), 
    scales=list(
      y=list(cex=1.4, at = seq(10, 70, by =10)), 
      x=list(cex=1.4, rot = 90),
      alternating=1
    ),
    strip.left = T,
    colorkey = list(label = list(cex = 1.4)),
    strip = F,
    par.settings=list(strip.background=list(col="lightgrey")),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )
dev.off()


# Assault/Homicide, colour

# 9) vehicles, ident, all groups, colour

png(filename="figures/case_deaton/revision/assault_colour.png", 
     width=15, height=35, res=300, units="cm"
)

dta %>% 
  mutate(assault_rate = 100000 * assault_homicide / population
  ) %>% 
  smooth_var(., group_vars = c("sex", "group"), smooth_var = "assault_rate", smooth_par = 0.7) %>% 
  levelplot(
    assault_rate ~ year * age | sex + group,
    data = . ,
    region=T, 
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts =15,
    aspect="iso",
    #col.regions = rev(gray(1:100/100)),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label = "Assault/Homicide", cex = 1.4),
    xlim=c(1999, 2013),
    layout = c(2, 3),
    ylim=c(0, 80), 
    scales=list(
      y=list(cex=1.4, at = seq(10, 70, by =10)), 
      x=list(cex=1.4, rot = 90),
      alternating=1
    ),
    strip.left = T,
    colorkey = list(label = list(cex = 1.4)),
    strip = F,
    par.settings=list(strip.background=list(col="lightgrey")),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )
dev.off()


# Legal Intervention, colour

png(filename="figures/case_deaton/revision/legal_intervention.png", 
     width=15, height=35, res=300, units="cm"
)

dta %>% 
  mutate(legal_rate = 100000 * legal_intervention / population
  ) %>% 
  smooth_var(., group_vars = c("sex", "group"), smooth_var = "legal_rate", smooth_par = 0.7) %>% 
  levelplot(
    legal_rate ~ year * age | sex + group,
    data = . ,
    region=T, 
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts =15,
    aspect="iso",
    #col.regions = rev(gray(1:100/100)),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label = "Legal Intervention", cex = 1.4),
    xlim=c(1999, 2013),
    layout = c(2, 3),
    ylim=c(0, 80), 
    scales=list(
      y=list(cex=1.4, at = seq(10, 70, by =10)), 
      x=list(cex=1.4, rot = 90),
      alternating=1
    ),
    strip.left = T,
    colorkey = list(label = list(cex = 1.4)),
    strip = F,
    par.settings=list(strip.background=list(col="lightgrey")),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )
dev.off()


# Liver-related, colour

png(filename="figures/case_deaton/revision/liver.png", 
    width=15, height=35, res=300, units="cm"
)

dta %>% 
  mutate(liver_rate = 100000 * chronic_liver_disease / population
  ) %>% 
  smooth_var(., group_vars = c("sex", "group"), smooth_var = "liver_rate", smooth_par = 0.7) %>% 
  levelplot(
    liver_rate ~ year * age | sex + group,
    data = . ,
    region=T, 
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts =15,
    aspect="iso",
    #col.regions = rev(gray(1:100/100)),
    col.regions=rev(colorRampPalette(brewer.pal(6, "Spectral"))(200)),
    main=list(label = "Chronic Liver Disease", cex = 1.4),
    xlim=c(1999, 2013),
    layout = c(2, 3),
    ylim=c(0, 80), 
    scales=list(
      y=list(cex=1.4, at = seq(10, 70, by =10)), 
      x=list(cex=1.4, rot = 90),
      alternating=1
    ),
    strip.left = T,
    colorkey = list(label = list(cex = 1.4)),
    strip = F,
    par.settings=list(strip.background=list(col="lightgrey")),
    panel = function(x, y, z, ...){
      panel.levelplot(x, y, z, ...)
      #        panel.rect(xleft = 1990, xright = 2013, ybottom = 45, ytop = 54, lty="dashed")
    }
  )
dev.off()