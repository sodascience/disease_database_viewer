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
db  <- tbl(con, glue("read_parquet('{rootfolder}/disease_database/**/*.parquet', hive_partitioning = true)"))

print("Downloading map info")
map <- st_read("https://nlgis.nl/api/maps?year=1869", crs = "EPSG:4326")

print("Get scale limits per disease")
limits <- db |> group_by(disease) |> summarize(
  lower = quantile(normalized_mentions, 0.005, na.rm = TRUE),
  upper = quantile(normalized_mentions, 0.995, na.rm = TRUE)
)

print("Ready.")


# Define server logic required to draw a histogram
function(input, output, session) {
  output$map <- renderPlot({
    if (local(input$monthly)) {
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
        filter(year == local(input$year),
               disease == local(input$disease)) |>
        group_by(cbscode) |>
        summarize(across(
          c(normalized_mentions, lower, upper),
          \(x) mean(x, na.rm = TRUE)
        )) |> collect()
    }
    
    titletext <- if (local(input$monthly)) {
      glue(
        "{str_to_title(input$disease)} mentions in {month(input$month, label = TRUE)} {input$year}"
      )
    } else {
      glue("{str_to_title(input$disease)} mentions in {input$year}")
    }
    
    upperlimit <- limits |> filter(disease == local(input$disease)) |> pull(upper)
    
    map |>
      left_join(dat, by = "cbscode") |>
      ggplot(aes(fill = pmin(upperlimit, normalized_mentions))) +
      scale_fill_gradient(na.value = "#ffffcc", low = "#f7fbff", high = "#08306b", limits = c(0, upperlimit)) +
      geom_sf(color = "transparent", size = 0.3) +
      theme_minimal() +
      labs(fill = "Mention rate", title = titletext)
  })
}
