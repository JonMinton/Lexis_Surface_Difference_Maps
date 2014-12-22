

country_codes <- read.csv("data/country_codes__new.csv", stringsAsFactors=F)
europe_labels <- country_codes$short[country_codes$europe==1]
names(europe_labels) <- country_codes$long[country_codes$europe==1]

shinyUI(fluidPage(
  titlePanel("Comparative Level Plot Explorer"),
  sidebarLayout(
    sidebarPanel(
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
      sliderInput(
        inputId = "year_range", 
        label = "select range of years",
        min=1900, max=2011,
        value=c(1950, 2010), step=1
      ),
      sliderInput(
        inputId = "age_range",
        label = "select range of ages",
        min=0, max=100,
        value=c(0, 80), step=1
        )
    ),
    mainPanel(
      textOutput("text01"),
      br(),
      plotOutput("plot01"),
      br(),
      plotOutput("plot02")
    )
  )
)
)