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
  ) |>
  filter(monitoring_group %in% c("Daily", "Routine"))

data |> distinct(site_id, site_description)

# Create the user interface
ui <- page_navbar(
  id = "tabs",
  theme = bs_theme(bootswatch = "flatly", base_font = "Century Gothic"),
  title = "Coastal Water Quality",
  fillable = F,
  navbar_options = navbar_options(position = "static-top", collapsible = T),
  position = "fixed-top",
  nav_panel(
    title = "All sites",
    card(
      card_header("Site Locations"),
      leafletOutput(outputId = "all_sites_map"),
      textOutput("map_description")
    ),
    card(
      card_header("Summary"),
      navset_pill(
        nav_panel(
          title = "Water Quality",
          DTOutput("water_quality_table")
        ),
        nav_panel(
          title = "Current Status",
          DTOutput("status_table")
        ),
        nav_panel(
          title = "Compliance",
          DTOutput("compliance_table")
        )
      )
    )
  ),
  nav_panel(
    title = "By site",
    titlePanel(title = textOutput("site_name")),
    selectInput(inputId = "site_id", label = "Select location:", choices = set_names(sites$site_id, sites$site_description), selected = NULL),
    layout_column_wrap(
      # Top card with controls
      card(card_header("Site Location"), leafletOutput(outputId = "site_map")),
      card(uiOutput("status"), uiOutput("category"), uiOutput("compliance"), uiOutput("most_recent_failure"))
    ),
    # Results figure
    layout_column_wrap(
      card(
        card_header("Enterococci (cfu per 100 mL)"),
        plotOutput(outputId = "plot"),
        textOutput("plot_text")
      ),

      # Data table output
      card(
        card_header("Enterococci (cfu per 100 mL)"),
        DTOutput(
          outputId = "results",
        ),
        height = 250
      )
    ),
    fillable = T
  ),
  nav_item(popover(
    bsicons::bs_icon("gear", title = "Dashboard Settings"),
    title = "Dashboard Settings",
    placement = "right",
    selectInput(inputId = "monitoring_group", label = "Data set:", choices = distinct(data, monitoring_group), multiple = T, selected = "Routine"),
    selectInput(inputId = "category", label = "Category", choices = distinct(data, category), multiple = T, selected = c("Recreational Node", "Coastal Monitoring Point")),
    dateRangeInput(inputId = "date_range", label = "Time period:", start = now() - duration("1 year"), end = now())
  )),
  textOutput("footer")
)

# Create the server function
server <- function(input, output, session) {
  thematic::thematic_shiny(font = "Century Gothic")

  # Calculate Hazen water quality category
  category <- reactive(data |>
    filter(
      site_id == input$site_id,
      monitoring_group %in% input$monitoring_group,
      category %in% input$category,
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
      )
    ) |>
    pull(hazen_category))

  # Calculate the percentage compliance
  compliance <- reactive(data |>
    filter(
      site_id == input$site_id,
      monitoring_group %in% input$monitoring_group,
      category %in% input$category,
      sample_date |> between(input$date_range[1], input$date_range[2])
    ) |>
    summarise(
      all_samples = n(),
      samples_exceed = sum(numeric_value > 240),
      samples_exceed_pct = 100 - round(sum(numeric_value > 240) / n() * 100, 0)
    ) |>
    pull(samples_exceed_pct))

  # Calculate current status
  status <- reactive(data |>
    filter(
      site_id == input$site_id,
      monitoring_group == input$monitoring_group,
      category %in% input$category
    ) |>
    group_by(site_id) |>
    arrange(desc(sample_date)) |>
    slice_head(n = 2) |>
    summarise(status = case_when(
      all(numeric_value > 380) ~ "Red",
      first(numeric_value) > 240 ~ "Amber",
      .default = "Green"
    )) |>
    mutate(colour = status |> str_to_lower()) |>
    pull(status, colour))

  # Calculate the most recent failure
  most_recent_failure <- reactive(data |>
    filter(
      site_id == input$site_id,
      monitoring_group %in% input$monitoring_group,
      numeric_value > 240
    ) |>
    arrange(desc(sample_date)) |>
    slice(1) |>
    mutate(sample_date = format(sample_date, "%d %b %Y")) |>
    pull(sample_date))

  # Render site name
  output$site_name <- renderText(sites |>
    filter(site_id == input$site_id) |>
    pull(site_description))

  # Value boxes
  output$status <- renderUI(value_box(title = "Current Status", value = status()[1], theme = case_when(status() == "Green" ~ "green", status() == "Amber" ~ "orange", status() == "Red" ~ "red")))
  output$category <- renderUI(value_box(title = "Water Quality Category", value = category(), theme = case_when(category() == "Excellent" ~ "blue", category() == "Good" ~ "green", category() == "Sufficient" ~ "orange", category() == "Poor" ~ "red", category() == "TFD" ~ "grey")))
  output$compliance <- renderUI(value_box(title = "Percentage Compliance", value = str_c(compliance(), "%"), theme = case_when(compliance() > 75 ~ "green", between(compliance(), 50, 75) ~ "orange", compliance() < 50 ~ "red")))
  output$most_recent_failure <- renderUI(value_box(title = "Most Recent Failure", value = if (length(most_recent_failure()) == 1) most_recent_failure() else "No failures"))

  # Data table output
  output$results <- renderDT(
    datatable(
      data |> filter(
        site_id == req(input$site_id),
        monitoring_group == input$monitoring_group,
        sample_date |> between(input$date_range[1], input$date_range[2])
      ) |>
        select(monitoring_group, sample_date, censored_value, numeric_value) |>
        mutate(sample_date = as.character(format(sample_date, "%d %b %Y"))) |>
        set_names(c("Data Set", "Date", "Result", "Numeric Result")),
      options = list(paging = F, filtering = F, searching = F, columnDefs = list(list(visible = FALSE, targets = 4)))
    ) |>
      formatStyle(
        columns = "Result",
        valueColumns = "Numeric Result",
        backgroundColor = styleInterval(240, c("green", "red")),
        color = "white"
      )
  )

  # Map output
  output$site_map <- renderLeaflet({
    water_quality() |>
      distinct(site_id, long, lat) |>
      filter(site_id == input$site_id) |>
      leaflet() |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addCircleMarkers()
  })

  # Map output
  col <- colorFactor(palette = c(excellent, good, sufficient, poor), domain = c("Excellent", "Good", "Sufficient", "Poor"), ordered = T)

  output$all_sites_map <- renderLeaflet({
    water_quality() |>
      distinct(site_id, site_description, long, lat, hazen_category) |>
      leaflet() |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addCircleMarkers(
        label = ~site_description,
        stroke = F,
        layerId = ~site_id,
        color = ~ col(hazen_category),
        fillOpacity = 0.75,
        radius = 5
      ) |>
      addLegend(
        "bottomright",
        pal = col,
        values = ~hazen_category,
        title = "Water Quality",
        opacity = 1
      )
  })
  
  output$map_description <- renderText("Click on a site for more details")

  # Plot output
  output$plot <- renderPlot({
    data |>
      filter(
        site_id == input$site_id,
        monitoring_group %in% input$monitoring_group,
        sample_date |> between(input$date_range[1], input$date_range[2])
      ) |>
      ggplot(aes(x = sample_date, y = numeric_value)) +
      geom_line(aes(colour = monitoring_group)) +
      geom_hline(aes(yintercept = 240, linetype = "240 cfu per 100 mL")) +
      geom_point(aes(colour = monitoring_group)) +
      coord_flip() +
      scale_x_date(name = "Sample Date", date_breaks = "1 month", date_labels = "%b %Y") +
      scale_y_continuous(name = "Result") +
      scale_colour_discrete(name = "Data Set") +
      scale_linetype_manual(name = "Single-sample threshold", values = c("240 cfu per 100 mL" = 2)) +
      theme(
        legend.position = "bottom",
        axis.title.y = element_blank(),
        legend.direction = "vertical",
        panel.background = element_rect(fill = NA, colour = "lightgrey"),
        panel.grid = element_blank()
      )
  })

  # Plot explanation output
  output$plot_text <- renderText("According to the National Water Quality Guidelines, the threshold for a single water sample to be considered safe for recreational use is 240 cfu per 100 mL for Enterococci.")

  # All water quality category data
  water_quality <- reactive(
    data |>
      filter(
        monitoring_group %in% input$monitoring_group,
        category %in% input$category,
        sample_date |> between(input$date_range[1], input$date_range[2])
      ) |>
      group_by(site_id, site_description, long, lat) |>
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
        )
      )
  )
  
  output$water_quality_table <- renderDT({
    datatable(water_quality() |>
      ungroup() |>
      select(site_description, hazen_category) |>
      set_names("Site Description", "Water Quality Category"),
      options = list(paging = F, filtering = F, searching = F),
      selection = "single") |>
      formatStyle(
        columns = "Water Quality Category",
        backgroundColor = styleEqual(levels = c("Excellent", "Good", "Sufficient", "Poor"), values = c(excellent, good, sufficient, poor)),
        color = "white"
      )
  })
  
  observeEvent(input$water_quality_table_rows_selected, {
    click <- input$water_quality_table_rows_selected
    site_id <- water_quality() |>
      ungroup() |>
      slice(click) |>
      pull(site_id)
    nav_select(session, id = "tabs", selected = "By site")
    updateSelectInput(session, inputId = "site_id", selected = site_id)
  })

  # All status data
  status_table <- reactive(
    data |>
      filter(
        monitoring_group %in% input$monitoring_group,
        category %in% input$category,
      ) |>
      group_by(site_id, site_description) |>
      arrange(desc(sample_date)) |>
      slice_head(n = 2) |>
      summarise(status = case_when(
        all(numeric_value > 380) ~ "Red",
        first(numeric_value) > 240 ~ "Amber",
        .default = "Green"
      ))
  )
  
  output$status_table <- renderDT(
    datatable(status_table() |>
                ungroup() |>
      select(site_description, status) |>
      set_names(c("Site Description", "Current Status")),
      options = list(paging = F, filtering = F, searching = F),
      selection = "single") |>
      formatStyle(
        columns = "Current Status",
        backgroundColor = styleEqual(levels = c("Green", "Amber", "Red"), values = c(good, sufficient, poor)),
        color = "white"
      )
  )

  observeEvent(input$status_table_rows_selected, {
    click <- input$status_table_rows_selected
    site_id <- status_table() |>
      ungroup() |>
      slice(click) |>
      pull(site_id)
    nav_select(session, id = "tabs", selected = "By site")
    updateSelectInput(session, inputId = "site_id", selected = site_id)
  })
  
  # All compliance data
  compliance_table <- reactive(
    data |>
      filter(
        monitoring_group %in% input$monitoring_group,
        category %in% input$category,
        sample_date |> between(input$date_range[1], input$date_range[2])
      ) |>
      group_by(site_id, site_description) |>
      summarise(
        all_samples = n(),
        samples_compliant = sum(numeric_value > 240),
        samples_compliant_pct = 100 - round(sum(numeric_value > 240) / n() * 100, 0)
      )
  )
  
  output$compliance_table <- renderDT(
    datatable(compliance_table() |>
                ungroup() |>
      select(site_description, samples_compliant_pct) |>
      mutate(compliance = str_c(samples_compliant_pct, "%")) |>
      set_names("Site Description", "Compliance Numeric", "Compliance"),
      options = list(paging = F, filtering = F, searching = F, columnDefs = list(list(visible = FALSE, targets = 2))),
      selection = "single") |>
      formatStyle(
        columns = "Compliance",
        valueColumns = "Compliance Numeric",
        backgroundColor = styleInterval(cuts = c(50, 75), values = c(poor, sufficient, good)),
        color = "white"
      )
  )
  
  observeEvent(input$compliance_table_rows_selected, {
    click <- input$compliance_table_rows_selected
    site_id <- compliance_table() |>
      ungroup() |>
      slice(click) |>
      pull(site_id)
    nav_select(session, id = "tabs", selected = "By site")
    updateSelectInput(session, inputId = "site_id", selected = site_id)
  })

  # Create summary table
  summary <- reactive(
    water_quality_category() |>
      inner_join(water_quality_status(), by = "site_id") |>
      inner_join(water_quality_compliance(), by = "site_id")
  )

  output$summary <- renderDT({
    datatable(
      summary() |>
        ungroup() |>
        select(site_id, site_description, status, hazen_category, samples_compliant_pct) |>
        set_names(c("Site ID", "Description", "Current Status", "Water Quality", "Compliance")),
      selection = "single"
    )
  })

  observeEvent(input$all_sites_map_marker_click, {
    click <- input$all_sites_map_marker_click
    output$text <- renderText(click$id)
    nav_select(session, id = "tabs", selected = "By site")
    updateSelectInput(session, inputId = "site_id", selected = click$id)
  })

  observeEvent(input$summary_rows_selected, {
    click <- input$summary_rows_selected
    nav_select(session, id = "tabs", selected = "By site")
    updateSelectInput(session, inputId = "site_id", selected = click$site_id)
  })
  
  # Create footer
  output$footer <- renderText(str_c("Showing only data from samples collected between ", input$daterange[1], " and ", input$daterange[2], ". Please click the gear icon for other settings.", sep = ""))
}

# Run the application
shinyApp(ui = ui, server = server)
