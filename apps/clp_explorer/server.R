# Server for comparative Level Plot (CLP) Explorer

# Compared with European Average

# To do
# 1) use longer series of data for Germany
#  


# 1)  clear the workspace

rm(list=ls())


require(plyr)
require(tidyr)
require(dplyr)

require(lattice)
require(latticeExtra)
require(RColorBrewer)
require(ggplot2)


# Helper functions --------------------------------------------------------


# Function: blurrer -------------------------------------------------------


blur <- function(input, smooth_par=2){
  
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





# load data ---------------------------------------------------------------

print("loading data")
counts <- read.csv("data/counts_eu.csv")
overall <- read.csv("data/europe_overall.csv")
country_codes <- read.csv("data/country_codes__new.csv", stringsAsFactors=F)


# create derived data ---------------------------------------------------


rates <- counts %>%
  mutate(specific = death_count/population_count) %>%
  select(country, sex, age, year, country, specific) %>%
  filter(sex!="total")

europe_labels <- country_codes$short[country_codes$europe==1]
names(europe_labels) <- country_codes$long[country_codes$europe==1]


# Server itself -----------------------------------------------------------


shinyServer(function(input, output){
  print("entered main shiny server")  
  
  # reactive functions ------------------------------------------------------
  
  # reactive:cohort_line_on
  cohort_line_on <- reactive({
    print("server:reactive:cohort_line_on")
    out <- input$show_cohort_line
  })
  
  # reactive:get_blurrer_value
  
  get_blurrer_value <- reactive({
    print("server:reactive:get_blurrer_value")
    out <- input$blurrer_value
  })
  
  # reactive:get_country_selection
  get_country_selection <- reactive({
    print("server:get_country_selection")
    tmp <- input$country_selection
    if (tmp!=""){
      out <- europe_labels[names(europe_labels)==tmp]      
    } else (out <- NULL)
    return(out)    
  })
  
  get_apply_blurrer_status <- reactive({
    print("server:reactive:get_apply_blurrer_status")
    out <- input$apply_blurrer
    return(out)
  })
  
  # reactive:load_country_selection
  load_country_selection <- reactive({
    print("server:load_country_selection")
    tmp <- get_country_selection()
    if (!is.null(tmp)){
      out <- subset(
        rates, 
        subset=country==tmp
        )
      min_year <- max( min(out$year), input$year_range[1]  )
      max_year <- min( max(out$year), input$year_range[2]  )
      
      min_age <- max(  min(out$age),  input$age_range[1]   )
      max_age <- min(  max(out$age),  input$age_range[2]   )
      
      out <- out %>%
        filter(
          age >= min_age & 
          age <= max_age &
          year >= min_year &
          year <= max_year
          ) %>%
        select(-country) %>%
        left_join(overall)
    } else {
      out <- NULL
    }
    
    return(out)
  })
  
  calc_log_dif <- reactive({
    print("server:calc_log_dif")
    dta <- load_country_selection()
    if (!is.null(dta)){
      out <- dta  %>% 
        mutate(
          log_dif = log(specific) - log(europe),
          log_dif = max(log_dif, -1.2),
          log_dif = min(log_dif, 1.2)
                    )            
      
    } else {out <- NULL}
    return(out)
  })
  # reactive:show_sc_plot ---------------------------------------------------  
  show_sc_plot <- reactive({
    print("server:show_sc_plot")
    tmp <- input$select_sc_plot
    
    if (tmp==T){
      out <- TRUE
    } else {out <- FALSE}
    return(out)
  })





# output:title_clp_title (renderText) -------------------------------------


  output$text_clp_title <- renderText({
    print("server:output$text_clp_title")
    dta <- load_country_selection()
    
    if (!is.null(dta)){
      
      out <- paste0(
        "Comparative Level Plot for ", 
        input$country_selection, 
        " ", 
        min(dta$year), 
        "-", 
        max(dta$year), 
        " (Ages ", 
        min(dta$age), 
        "-", 
        max(dta$age), 
        ")"
        )
    } else {
      out <- ""
    }
      return(out)
  })



# Output:plot_scp (renderPlot) --------------------------------------------


  output$plot_scp <- renderPlot({
    print("server:output$plot_scp")
    tag <- show_sc_plot()
    show_cohort <- cohort_line_on()
    if (tag==T){
      dta <- load_country_selection()
      
      # Let's look at the mort rates only
      out <- dta  %>% 
          filter(sex!="total") %>%
            contourplot(
              log(europe) ~ year * age | sex, 
              data=.,
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
              ),
              panel=function(...){
                panel.contourplot(...)
                if (show_cohort){
                  panel.abline(
                    a= - input$select_cohort_year,
                    b=1, 
                    lwd=2, lty="dashed"
                  )
                }
              }
            )
    } else {out <- NULL}
    return(out)
  },
  width=1200, height=600
  )


  output$show_blur_value <- renderText({
    print("server:output$show_blur_value")
    tmp <- get_blurrer_value()
    tmp2 <- get_apply_blurrer_status()
    output <- paste(
      "The blurrer value is",
      tmp,
      "and the value of apply_blurrer is",
      tmp2
      )
    
    return(output)
  })
  
  
  output$plot_clp <- renderPlot({
    print("server:output$plot_clp")
    
    show_cohort <- cohort_line_on()
    
    if (tag==T){
      dta <- load_country_selection()
      

      all_level <- dif_logs %>%
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
        filter(sex!="total" & country !="europe" & age <=80) %>%
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
      
      # Let's look at the mort rates only
      out <- dta  %>% 
        filter(sex!="total") %>%
        contourplot(
          log(europe) ~ year * age | sex, 
          data=.,
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
          ),
          panel=function(...){
            panel.contourplot(...)
            if (show_cohort){
              panel.abline(
                a= - input$select_cohort_year,
                b=1, 
                lwd=2, lty="dashed"
              )
            }
          }
        )
    } else {out <- NULL}
    return(out)
  },
  width=1200, height=600
  )
  

})

