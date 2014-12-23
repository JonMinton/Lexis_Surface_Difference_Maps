

country_codes <- read.csv("data/country_codes__new.csv", stringsAsFactors=F)
europe_labels <- country_codes$short[country_codes$europe==1]
names(europe_labels) <- country_codes$long[country_codes$europe==1]

shinyUI(fluidPage( 
  titlePanel("Comparative Level Plot Explorer"),
  hr(),
  h2("Information and instructions"),
  em("Once the data are loaded, please select a country."),
  em("Once a selection is made, the comparative level plot (CLP) will be generated."),br(),
  em("Reds on the CLP indicate worse-than-average mortality, and blues indicated better-than-average mortality rates."),br(),
  em("The darkness of the shade indicates how far the mortality rates differ from the European average."),
  br(),
  em("If the box is checked, then a shaded contour map showing mortality rates for Europe as a whole will be shown."),
  br(),
  hr(),
  fluidRow(
    column(
      4,
      h3("Select country"),
      selectInput(
        "country_selection", "select a country",
        choices=c("", names(europe_labels)),
        selected=""
      ),
      checkboxInput(
        "select_sc_plot",
        "Check to show overall shaded contour plot",
        value=FALSE
      ),
      checkboxInput(
        "select_composite_plot",
        "Check to show composite plot",
        value=FALSE
      )
      
    ),
    column(
      4,
      h3("Select range"),
      sliderInput(
        inputId = "year_range", 
        label = "select range of years",
        min=1900, max=2011,
        value=c(1950, 2010), step=1,
        format="####"
      ),
      sliderInput(
        inputId = "age_range",
        label = "select range of ages",
        min=0, max=100,
        value=c(0, 80), step=1
      )      
    ),
    column(
      3,
      h3("Happy Christmas!")
    )
  ),
  hr(),
  h2(textOutput("text_clp_title")),
  plotOutput("plot_clp", height="100%"),
  br(),
  h2("Shaded Contour Plot"),
  plotOutput("plot_overall", height="100%"),
  h2("Composite Plot"),
  plotOutput("plot_composite", height="100%")
  


  


))
