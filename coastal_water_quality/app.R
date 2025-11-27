# Coastal Water Quality App

# Import packages
library(shiny)
library(shinyMobile)
library(tidyverse)
library(httr2)
library(jsonlite)
library(leaflet)
library(bslib)
library(scales)
library(DT)

# Set city colours
city_blue <- "#0098c5"
city_green <- "#bacf00"
city_pink <- "#c8006f"
city_teal <- "#005870"
city_dark_green <- "#446414"
city_red <- "#9d2235"
city_brown <- "#47292e"
city_tan <- "#98871f"

# Water Quality Colours
c("TFD" = "grey", "Poor" = "#D73027", "Sufficient" = "#FECC5C", "Good" = "#A6D96A", "Excellent" = "#3288BD")

# Add public key to environment variables
Sys.setenv(apikey = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InducnZkdmVzb3Zua21xYmhraGp6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NDM4NjM4MjYsImV4cCI6MjA1OTQzOTgyNn0.21jkGF09gCaxGXAzKX0VaHCYty76NCYB0heMyWGfe2c")

# Connect to REST API
sites <- request("https://wnrvdvesovnkmqbhkhjz.supabase.co/rest/v1") |>
  req_url_path_append("sites_view") |>
  req_headers("apikey" = Sys.getenv("apikey"), "Accept-Profile" = "coastal") |>
  req_perform() |>
  resp_body_string() |>
  fromJSON() |>
  as_tibble() |>
  arrange(site_description)

results <- request("https://wnrvdvesovnkmqbhkhjz.supabase.co/rest/v1") |>
  req_url_path_append("results_view") |>
  req_headers("apikey" = Sys.getenv("apikey"), "Accept-Profile" = "coastal") |>
  req_perform() |>
  resp_body_string() |>
  fromJSON() |>
  as_tibble() |>
  select(site_id, sample_date, monitoring_group, censored_value, numeric_value) |>
  mutate(sample_date = ymd(sample_date))

# Join sites and results
data <- sites |>
  inner_join(results, by = "site_id")

# Check the data
data <- data |>
  mutate(
    across(c(site_description, site_id, category, coastline), fct),
    monitoring_group = monitoring_group |> str_replace("_", " ") |> str_to_title() |> fct()
  )

# Create the user interface
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  # Top card with controls
  layout_column_wrap(
    card(
      card_header("Coastal Water Quality"),
      selectInput(inputId = "site_id", label = "Select location:", choices = set_names(sites$site_id, sites$site_description), selected = NULL),
      selectInput(inputId = "monitoring_group", label = "Data set:", choices = distinct(data, monitoring_group), multiple = T, selected = "Routine"),
      dateRangeInput(inputId = "date_range", label = "Time period:", start = ymd("2025-01-01"), end = "2025-12-31"),
      ),
    card(leafletOutput(outputId = "site_map"))),
  
  # Results figure
  card(
    card_header("Enterococci (cfu per 100 mL)"),
    plotOutput(outputId = "plot")),
  
  # Data table output
    dataTableOutput(
      outputId = "results"
  ))
  

# Create the server function
server <- function(input, output, session) {
  
  # Data table output
  output$results <- renderDataTable(
    data |> filter(
      site_id == req(input$site_id),
      monitoring_group == input$monitoring_group,
      sample_date |> between(input$date_range[1], input$date_range[2])) |>
      select(sample_date, censored_value, numeric_value) |>
      set_names(c("Sample Date", "Censored Value", "Numeric Value"))
    )
  
  # Map output
  output$site_map <- renderLeaflet({
    data |>
      distinct(site_id, long, lat) |>
      filter(site_id == input$site_id) |>
      leaflet() |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addCircleMarkers()
  })
  
  # Plot output
  output$plot <- renderPlot({
    data |>
      filter(
        site_id == input$site_id,
        monitoring_group == input$monitoring_group,
        sample_date |> between(input$date_range[1], input$date_range[2])
      ) |>
      ggplot(aes(x = sample_date, y = numeric_value, colour = monitoring_group)) +
      geom_line(aes(group = monitoring_group)) +
      geom_point()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
