# Server for comparative Level Plot (CLP) Explorer

# Compared with European Average

# To do
# 1) use longer series of data for Germany
#  


# 1)  clear the workspace

rm(list=ls())

require(plyr)
require(reshape2)
require(lattice)
require(latticeExtra)
require(RColorBrewer)



######################################################################################
# SOURCE DATA

# 4) load human mortality database (HMD) data on population counts and death counts
# in the 'tidy data' format suggested by Hadley Wickham 

#setwd("E:/repos/Lexis_Surface_Difference_Maps/apps/clp_explorer")
counts <- read.csv("data/counts_eu.csv")
counts$X <- NULL
print("loaded count data")
rates <- mutate(counts, specific=death_count/population_count)
rates$death_count <- NULL
rates$population_count <- NULL
print("calculated rates")
overall <- read.csv("data/europe_overall.csv")
print("loaded overall rates data")
country_codes <- read.csv("data/country_codes__new.csv", stringsAsFactors=F)
europe_labels <- country_codes$short[country_codes$europe==1]
names(europe_labels) <- country_codes$long[country_codes$europe==1]

shinyServer(function(input, output){
  print("entered main shiny server")
  #select specific country
  get_country_selection <- reactive({
    tmp <- input$country_selection
    if (tmp!=""){
      out <- europe_labels[names(europe_labels)==tmp]      
    } else (out <- NULL)
    return(out)    
  })
  
  load_country_selection <- reactive({
    tmp <- get_country_selection()
    if (!is.null(tmp)){
      out <- subset(
        rates, 
        subset=country==tmp
        )
      min_year <- max(
        min(out$year),
        input$year_range[1]
      )
      max_year <- min(
        max(out$year),
        input$year_range[2]
      )
      
      min_age <- max(
        min(out$age),
        input$age_range[1] 
      )
      max_age <- min(
        max(out$age),
        input$age_range[2]
      )
      
      out <- subset(
        out,
        subset = age >=min_age & age <= max_age &
          year >= min_year & year <= max_year
        )
      out$country <- NULL
      out <- join(out, overall, type="left")
    } else {
      out <- NULL
    }
    
    return(out)
  })
  
  calc_log_dif <- reactive({
    dta <- load_country_selection()
    if (!is.null(dta)){
      out <- mutate(dta,
                    log_dif = log(specific) - log(europe)
                    )      
    } else {out <- NULL}
    return(out)
  })
  
  show_sc_plot <- reactive({
    tmp <- input$select_sc_plot
    
    if (tmp==T){
      out <- TRUE
    } else {out <- FALSE}
    return(out)
  })


  output$text_clp_title <- renderText({
    ctry_selection <- input$country_selection
    dta <- load_country_selection()
    
    if (ctry_selection!="" & !is.null(dta)){
      min_year <- min(dta$year)
      max_year <- max(dta$year)
      min_age <- min(dta$age)
      max_age <- max(dta$age)
      out <- paste0("Comparative Level Plot for ", ctry_selection, " ", min_year, "-", max_year, 
                   " (Ages ", min_age, "-", max_age, ")")
    } else {
      out <- ""
    }
      return(out)
  })



  output$plot_overall <- renderPlot({
    tag <- show_sc_plot()
    if (tag==T){
      dta <- load_country_selection()
      out <- contourplot(
        log(europe) ~ year * age | sex,
        data=subset(dta, subset=sex!="total"),
        region=T,
        par.strip.text=list(cex=1.4, fontface="bold"),
        ylab="Age in years",
        xlab="Year",
        cex=1.4,
        cuts=50,
        col.regions=rev(heat.colors(200)),
        main=NULL
        )
    } else {out <- NULL}
    return(out)
  }, height=800, width=1600)

  output$plot_clp <- renderPlot({
    tmp <- calc_log_dif()
    
    if (!is.null(tmp)){
    out  <- levelplot(
        log_dif ~ year * age | sex, 
        data=subset(tmp, subset=sex!="total"),
        region=T, 
        par.strip.text=list(cex=1.4, fontface="bold"),
        ylab="Age in years",
        xlab="Year",
        cex=1.4,
        at = seq(from= -1.2, to = 1.2, by=0.2),
        col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
        main=NULL
      )
    } else {out <- NULL}
    return(out)
  }, height=800, width=1600)
  
  output$plot_composite <- renderPlot({
    # From levelplot
    dta <- calc_log_dif()
    tag <- input$select_composite_plot
    if (!is.null(dta) & tag==T){
      clp  <- levelplot(
        log_dif ~ year * age | sex, 
        data=subset(dta, subset=sex!="total"),
        region=T, 
        par.strip.text=list(cex=1.4, fontface="bold"),
        ylab="Age in years",
        xlab="Year",
        cex=1.4,
        at = seq(from= -1.2, to = 1.2, by=0.2),
        col.regions = colorRampPalette(rev(brewer.pal(6, "RdBu")))(64),
        main=NULL
      )
      
     cp <- contourplot(
        log(europe) ~ year * age | sex,
        data=dta,
        region=F,
        cuts=50,
        main=NULL
      )
      out <- clp + cp
    } else {out <- NULL}
    return(out)
    
  }, height=800, width=1600)

})

