print("Loading packages")
library(shiny)
library(duckdb)
library(sf)
library(tidyverse)
library(glue)

rootfolder <- getwd()

if (!dir.exists(file.path(rootfolder, "disease_database"))) {
  print("Perform first time setup")
  tf <- tempfile(fileext = ".zip")
  url <- "https://github.com/sodascience/disease_database/releases/download/v1.0.0/disease_database_v1.0.zip"
  download.file(url = url, destfile = tf)
  unzip(tf, exdir = file.path(rootfolder, "disease_database"))
  unlink(tf)
}


print("Connecting to data")
drv <- duckdb()
con <- dbConnect(drv)
db  <- tbl(
  con,
  glue(
    "read_parquet('{rootfolder}/disease_database/**/*.parquet', hive_partitioning = true)"
  )
)

print("Downloading map info")
map <- st_read("https://nlgis.nl/api/maps?year=1869", crs = "EPSG:4326")

print("Get scale limits per disease")
limits <- db |> group_by(disease) |> summarize(
  lower = quantile(normalized_mentions, 0.001, na.rm = TRUE),
  upper = quantile(normalized_mentions, 0.999, na.rm = TRUE)
)

print("Ready.")

DEBOUNCE_MS <- 500

# Define server logic required to draw a histogram
function(input, output, session) {
  sparkdata <- reactive({
    db |> filter(disease == local(input$disease)) |>
      group_by(year) |>
      summarize(mentions = mean(normalized_mentions, na.rm = TRUE))
  }) |>
    debounce(DEBOUNCE_MS)
  
  sparkyear <- reactive({
    input$year
  }) |> debounce(DEBOUNCE_MS)
  
  mapdata <- reactive({
    if (input$monthly) {
      dat <- db |>
        filter(
          year == local(input$year),
          month == local(input$month),
          disease == local(input$disease)
        ) |>
        group_by(cbscode) |>
        summarize(across(
          c(normalized_mentions, lower, upper),
          \(x) mean(x, na.rm = TRUE)
        )) |> collect()
    } else {
      dat <- db |>
        filter(year == local(input$year), disease == local(input$disease)) |>
        group_by(cbscode) |>
        summarize(across(
          c(normalized_mentions, lower, upper),
          \(x) mean(x, na.rm = TRUE)
        )) |> collect()
    }
    map |> left_join(dat, by = "cbscode")
  }) |> debounce(DEBOUNCE_MS)
  
  titletext <- reactive({
    if (input$monthly) {
      glue(
        "{str_to_title(input$disease)} mentions in {month(input$month, label = TRUE)} {input$year}"
      )
    } else {
      glue("{str_to_title(input$disease)} mentions in {input$year}")
    }
  }) |> debounce(DEBOUNCE_MS)
  
  upperlimit <- reactive({
    limits |> filter(disease == local(input$disease)) |> pull(upper)
  }) |> debounce(DEBOUNCE_MS)
  
  output$spark <- renderPlot({
    sparkdata() |>
      ggplot(aes(x = year, y = mentions)) +
      labs(y = "", x = "") +
      scale_x_continuous(breaks = c(1830, 1850, 1870, 1890, 1910, 1930)) +
      geom_line(colour = "darkgrey") +
      geom_vline(xintercept = sparkyear(), colour = "#08306b") +
      theme_minimal() +
      theme(
        #axis.text.x = element_text(angle = 90),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, -.1), "npc")
      )
  }, res = 90)
  
  output$map <- renderPlot({
    mapdata() |>
      ggplot(aes(fill = pmin(upperlimit(), normalized_mentions))) +
      scale_fill_gradient(
        na.value = "#ffffcc",
        low = "#f7fbff",
        high = "#08306b",
        limits = c(0, upperlimit()),
        transform = scales::transform_pseudo_log(sigma = upperlimit() /
                                                   10)
      ) +
      geom_sf(color = "transparent", size = 0.3) +
      theme_minimal() +
      labs(fill = "Mention rate", title = titletext())
  }, res = 100)
}
