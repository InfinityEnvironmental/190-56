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

# Connect to database
request()

# Load data from database
blue_flag <- tbl(con, I("coastal.sites_view")) |>
  inner_join(tbl(con, I("coastal.results_view"))) |>
  filter(sample_date > "2024-01-01") |>
  select(site_id, site_description, long, lat, sample_date, censored_value, numeric_value, filename, lab_code, monitoring_group) |>
  collect()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Blue Flag Candidate Sites"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "monitoring_group", label = "Data Set", multiple = T, choices = set_names(blue_flag$monitoring_group, blue_flag$monitoring_group |> str_to_title()), selected = "Routine"),
            dateRangeInput(inputId = "date_range", label = "Date Range")
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
          min_date = min(sample_date),
          max_date = max(sample_date),
          n = sum(!is.na(numeric_value)),
          hazen95 = quantile(numeric_value, 0.95, type = 5, na.rm = TRUE),
          hazen90 = quantile(numeric_value, 0.9, type = 5, na.rm = TRUE),
          water_quality_category = case_when(
            n < 10 ~ "TFD*",
            hazen95 <= 100 ~ "Excellent",
            hazen95 <= 200 ~ "Good",
            hazen95 > 200 & hazen90 > 185 ~ "Poor",
            hazen95 > 200 & hazen90 < 185 ~ "Sufficient"
          ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
