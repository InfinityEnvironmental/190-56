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

poor <- "#D73027"
sufficient <- "#FECC5C"
good <- "#A6D96A"
excellent <- "#3288BD"

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
  arrange(site_description) |>
  filter(active)

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
  inner_join(results, by = "site_id") |>
  filter(active)

# Check the data
data <- data |>
  mutate(
    across(c(site_description, site_id, category, coastline), fct),
    monitoring_group = monitoring_group |> str_replace("_", " ") |> str_to_title() |> fct()
  )

data |> distinct(site_id, site_description)

# Create the user interface
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  titlePanel(title = "Coastal Water Quality"),
  layout_column_wrap(
  # Top card with controls
    card(
      card_header("Dashboard Options"),
      selectInput(inputId = "site_id", label = "Select location:", choices = set_names(sites$site_id, sites$site_description), selected = NULL),
      selectInput(inputId = "monitoring_group", label = "Data set:", choices = distinct(data, monitoring_group), multiple = T, selected = "Routine"),
      dateRangeInput(inputId = "date_range", label = "Time period:", start = ymd("2025-01-01"), end = "2025-12-31"),
    ),
    card(uiOutput("category"), uiOutput("status"), uiOutput("compliance"), uiOutput("most_recent_failure")),
    card(card_header("Site Location"), leafletOutput(outputId = "site_map"))
),
  # Results figure
  layout_column_wrap(
    card(
      card_header("Enterococci (cfu per 100 mL)"),
      plotOutput(outputId = "plot")
    ),

    # Data table output
    card(
      card_header("Results"),
      tableOutput(
        outputId = "results"
      ), height = 250
    )
  )
)


# Create the server function
server <- function(input, output, session) {
  
  thematic::thematic_shiny()
  
  # Calculate Hazen water quality category
  category <- reactive(data |>
    filter(
      site_id == input$site_id,
      monitoring_group %in% input$monitoring_group,
      sample_date |> between(input$date_range[1], input$date_range[2])
    ) |>
    summarise(
      min_date = min(sample_date),
      max_date = max(sample_date),
      n = sum(!is.na(numeric_value)),
      hazen95 = quantile(numeric_value, 0.95, type = 5, na.rm = TRUE),
      hazen90 = quantile(numeric_value, 0.9, type = 5, na.rm = TRUE),
      hazen_category = case_when(
        n < 10 ~ "TFD",
        hazen95 <= 100 ~ "Excellent",
        hazen95 <= 200 ~ "Good",
        hazen95 > 200 & hazen90 > 185 ~ "Poor",
        hazen95 > 200 & hazen90 < 185 ~ "Sufficient"
      )) |>
      pull(hazen_category))
  
  # Value boxes
  output$category <- renderUI(value_box(title = "Water Quality Category", value = category(), theme = "red"))
  output$status <- renderUI(value_box(title = "Current Status", value = "Green", theme = "orange"))
  output$compliance <- renderUI(value_box(title = "Percentage Compliance", value = "99%", theme = "green"))
  output$most_recent_failure <- renderUI(value_box(title = "Most Recent Failure", value = "2025-11-01"))
  
  # Data table output
  output$results <- renderTable(
    data |> filter(
      site_id == req(input$site_id),
      monitoring_group == input$monitoring_group,
      sample_date |> between(input$date_range[1], input$date_range[2])
    ) |>
      select(monitoring_group, sample_date, censored_value) |>
      mutate(sample_date = as.character(sample_date)) |>
      set_names(c("Data Set", "Sample Date", "Enterococci")),
    spacing = "m"
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
      ggplot(aes(x = sample_date, y = numeric_value)) +
      geom_line(aes(colour = monitoring_group)) +
      geom_point(aes(colour = monitoring_group))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
