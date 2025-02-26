library(shiny)
library(bslib)

DISEASES <- c(
  "cholera",
  "diphteria",
  "dysentery",
  "influenza",
  "malaria",
  "measles",
  "scarletfever",
  "smallpox",
  "tuberculosis",
  "typhus"
)

page_sidebar(
  title = "Disease database viewer",
  sidebar = sidebar(
    selectInput("disease", "Disease", choices = DISEASES),
    p("Mentions"),
    plotOutput("spark", height = 80),
    sliderInput(
      "year",
      "Year",
      value = 1866,
      min = 1830,
      max = 1939,
      step = 1,
      sep = "",
    ),
    checkboxInput("monthly", "Select month"),
    conditionalPanel(
      "input.monthly",
      sliderInput(
        "month",
        "Month",
        value = 1,
        min = 1,
        max = 12,
        step = 1
      )
    )
  ),
  
  card(plotOutput("map")),
  p(
    span("ODISSEI SoDa Team, 2025."),
    a("Disease database version 1.0.0", href = "https://github.com/sodascience/disease_database/releases/tag/v1.0.0"),
    style = "font-size:10pt;text-align:right;"
  )
  
)