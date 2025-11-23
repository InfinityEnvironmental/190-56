# Coastal Water Quality App

library(shiny)
library(shinyMobile)
library(tidyverse)
library(httr2)
library(jsonlite)
library(leaflet)
library(bslib)
library(scales)

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
c("TFD*" = "grey", "Poor" = "#D73027", "Sufficient" = "#FECC5C", "Good" = "#A6D96A", "Excellent" = "#3288BD")

# Add public key to environment variables
Sys.setenv(apikey = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InducnZkdmVzb3Zua21xYmhraGp6Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NDM4NjM4MjYsImV4cCI6MjA1OTQzOTgyNn0.21jkGF09gCaxGXAzKX0VaHCYty76NCYB0heMyWGfe2c")

# Connect to REST API
response_sites <- request("https://wnrvdvesovnkmqbhkhjz.supabase.co/rest/v1") |>
  req_url_path_append("sites_view") |>
  req_headers("apikey" = Sys.getenv("apikey"), "Accept-Profile" = "coastal") |>
  req_perform()

# Access JSON data from response
sites <- response_sites |>
  resp_body_string() |>
  fromJSON() |>
  as_tibble() |>
  filter(active) |>
  arrange(site_description)

days_since_failure <- request("https://wnrvdvesovnkmqbhkhjz.supabase.co/rest/v1") |>
  req_url_path_append("days_since_failure") |>
  req_headers("apikey" = Sys.getenv("apikey"), "Accept-Profile" = "coastal") |>
  req_perform() |>
  resp_body_string() |>
  fromJSON() |>
  as_tibble()

days_since_failure |>
  filter(site_description |> str_detect("lencai"))

water_quality_category <- request("https://wnrvdvesovnkmqbhkhjz.supabase.co/rest/v1") |>
  req_url_path_append("rolling365day") |>
  req_headers("apikey" = Sys.getenv("apikey"), "Accept-Profile" = "coastal") |>
  req_perform() |>
  resp_body_string() |>
  fromJSON() |>
  as_tibble()

water_quality_compliance <- request("https://wnrvdvesovnkmqbhkhjz.supabase.co/rest/v1") |>
  req_url_path_append("percentage_exceedances_5years") |>
  req_headers("apikey" = Sys.getenv("apikey"), "Accept-Profile" = "coastal") |>
  req_perform() |>
  resp_body_string() |>
  fromJSON() |>
  as_tibble()

water_quality_results <- request("https://wnrvdvesovnkmqbhkhjz.supabase.co/rest/v1") |>
  req_url_path_append("results_view") |>
  req_headers("apikey" = Sys.getenv("apikey"), "Accept-Profile" = "coastal") |>
  req_perform() |>
  resp_body_string() |>
  fromJSON() |>
  as_tibble() |>
  select(site_id, sample_date, monitoring_group, censored_value) |>
  mutate(sample_date = ymd(sample_date))


# Create the user interface
ui <- page_navbar(
  theme = bslib::bs_theme(base_font = "Century Gothic", bg = "#005870", fg = "white"),
  title = "Coastal Water Quality",
  nav_panel(
    title = "Site Overview",
    fluidRow(
      column(4,
        offset = 4,
        selectizeInput("search", label = "Search", choices = set_names(sites$site_id, sites$site_description)),
        textOutput("text")
      )
    ),
    layout_column_wrap(
      fillable = T,
      value_box(
        title = "Days since last exceedance",
        value = textOutput(outputId = "days_since_failure"),
        theme = value_box_theme(bg = city_green, fg = "white")
      ),
      value_box(
        title = "365-day Water Quality",
        value = textOutput(outputId = "water_quality_category"),
        theme = value_box_theme(bg = city_pink, fg = "white")
      ),
      value_box(
        title = "5-year Compliance",
        fill = T,
        value = textOutput(outputId = "compliance"),
        theme = value_box_theme(bg = city_blue, fg = "white"),
        p(textOutput(outputId = "sample_size"))
      ),
    ),
    layout_column_wrap(
      card(leafletOutput(outputId = "site_map")),
      card(plotOutput(outputId = "effort"))
    )
  ),
  nav_panel(
    title = "All Data",
    card(
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          selectInput(
            inputId = "site_id",
            label = "Monitoring Site",
            choices = set_names(sites$site_id, sites$site_description)
          ),
          selectInput(
            inputId = "monitoring_category",
            label = "Data Set",
            choices = c("Blue Flag", "Routine", "Daily"),
            multiple = T,
            selected = "Routine"
          ),
          dateRangeInput(
            inputId = "timeperiod",
            label = "Select dates",
            start = ymd("2025-01-01"),
            end = ymd("2025-12-31")
          )
        ),
        card(DT::DTOutput("all_data"))
      )
    )
  )
)

# Create the server function
server <- function(input, output, session) {

  output$text <- renderText(sites |> filter(site_id == input$search) |> pull(category))

  output$site_map <- renderLeaflet({
    sites |>
      filter(site_id == input$search) |>
      leaflet() |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addCircleMarkers(color = city_pink)
  })

  output$days_since_failure <- renderText({
    days <- days_since_failure |>
      filter(site_id == input$search) |>
      pull(days_since_exceedance) |>
      str_c(" days")
  })

  output$water_quality_category <- renderText({
    water_quality_category |>
      filter(site_id == input$search) |>
      pull(water_quality_category)
  })

  output$compliance <- renderText({
    water_quality_compliance |>
      filter(site_id == input$search) |>
      mutate(percentage_compliance = 100 - percentage_exceedances) |>
      pull(percentage_compliance) |>
      str_c(" %")
  })

  output$sample_size <- renderText({
    water_quality_compliance |>
      filter(site_id == input$search) |>
      pull(total_samples) |>
      str_c(" samples")
  })

  output$samples_this_year <- renderText({
    water_quality_results |>
      filter(site_id == input$search) |>
      pull(total_samples) |>
      str_c(" samples")
  })

  output$effort <- renderPlot({
    water_quality_results |>
      filter(site_id == input$search) |>
      ggplot(aes(x = sample_date, fill = monitoring_group)) +
      geom_density(aes(y = after_stat(count)), adjust = 0.5, alpha = 0.3) +
      # geom_histogram(aes(y = after_stat(count)), linewidth = 0.8) +
      theme(
        legend.position = "bottom",
        axis.title = element_blank()
      )
  })

  output$all_data <- DT::renderDataTable({
    df <- sites |>
      inner_join(water_quality_results, by = "site_id")
    df |>
      mutate(monitoring_group = monitoring_group |> str_replace("_", " ") |> str_to_title()) |>
      filter(monitoring_group %in% input$monitoring_category, site_id %in% req(input$site_id), sample_date > input$timeperiod[1], sample_date <= input$timeperiod[2]) |>
      select(site_id, site_description, sample_date, censored_value) |>
      set_names(c("Site ID", "Description", "Sample Date", "Enterococci (cfu per 100 mL)"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
