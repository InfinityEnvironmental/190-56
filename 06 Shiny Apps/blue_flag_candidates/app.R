# Create an app that interactively shows the candidate sites for Blue Flag beaches

# Load packages
library(shiny)
library(tidyverse)
library(dbplyr)
library(DBI)
library(sf)
library(keyring)
library(kableExtra)
library(extrafont)
library(httr2)
library(jsonlite)

# Parameters
Sys.setenv(apikey = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InducnZkdmVzb3Zua21xYmhraGp6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NDM4NjM4MjYsImV4cCI6MjA1OTQzOTgyNn0.21jkGF09gCaxGXAzKX0VaHCYty76NCYB0heMyWGfe2c")

# Connect to REST API
response_results <- request("https://wnrvdvesovnkmqbhkhjz.supabase.co/rest/v1") |>
  req_url_path_append("results_view") |>
  req_headers("apikey" = Sys.getenv("apikey"), "Accept-Profile" = "coastal") |>
  req_perform()

# Access JSON data from response
results <- response_results |>
  resp_body_string() |>
  fromJSON() |>
  as_tibble() |>
  mutate(monitoring_group = str_replace_all(monitoring_group, "_", " ") |> str_to_title()) |>
  mutate(sample_date = as_date(sample_date))

# Do the same for the sites table
response_sites <- request("https://wnrvdvesovnkmqbhkhjz.supabase.co/rest/v1") |>
  req_url_path_append("sites_view") |>
  req_headers("apikey" = Sys.getenv("apikey"), "Accept-Profile" = "coastal") |>
  req_perform()

sites <- response_sites |>
  resp_body_string() |>
  fromJSON() |>
  as_tibble()

# Join sites and results
blue_flag <- sites |>
  inner_join(results, by = "site_id")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Blue Flag Candidate Sites"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "monitoring_group", label = "Data Set", multiple = T, choices = set_names(blue_flag$monitoring_group, blue_flag$monitoring_group), selected = "Routine"),
            dateRangeInput(inputId = "date_range", label = "Date Range", start = ymd("2024-12-01"), end = ymd("2025-02-28"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput(outputId = "candidate_sites")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$candidate_sites <- renderTable({
      blue_flag |>
        filter(
          monitoring_group %in% input$monitoring_group,
          sample_date |> between(ymd(input$date_range[1]), ymd(input$date_range[2]))
               ) |>
        group_by(site_id, site_description) |>
        summarise(
          min_date = min(sample_date) |> as_date(),
          max_date = max(sample_date) |> as_date(),
          n = sum(!is.na(numeric_value)),
          hazen95 = quantile(numeric_value, 0.95, type = 5, na.rm = TRUE),
          hazen90 = quantile(numeric_value, 0.9, type = 5, na.rm = TRUE),
          water_quality_category = case_when(
            n < 10 ~ "TFD*",
            hazen95 <= 100 ~ "Excellent",
            hazen95 <= 200 ~ "Good",
            hazen95 > 200 & hazen90 > 185 ~ "Poor",
            hazen95 > 200 & hazen90 < 185 ~ "Sufficient"
          )) |>
        filter(water_quality_category == "Excellent") |>
        mutate(min_date = as.character(min_date), max_date = as.character(max_date)) |>
        set_names(c("Site ID", "Site Description", "Earliest Sample", "Latest Sample", "Number of samples", "Hazen 95th Percentile", "Hazen 90th Percentile", "Water Quality Category"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
