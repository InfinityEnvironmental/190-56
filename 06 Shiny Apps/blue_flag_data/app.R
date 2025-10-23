# This is a Shiny app to investigate Blue Flag data from 2013 to 2025 for all sites

library(shiny)
library(tidyverse)
library(dbplyr)
library(DBI)
library(sf)
library(keyring)
library(kableExtra)
library(extrafont)
library(bslib)

# Create a database connection
con <- dbConnect(RPostgres::Postgres(),
                 host = "aws-0-eu-central-1.pooler.supabase.com",
                 port = 5432,
                 user = str_c(key_list(service = "supabase")$username, "wnrvdvesovnkmqbhkhjz", sep = "."),
                 password = key_get(service = "supabase", username = key_list(service = "supabase")$username),
                 dbname = "postgres"
)

blue_flag <- tbl(con, I("coastal.blue_flag")) |>
  collect() |>
  mutate(season = if_else(month(week) %in% c(10, 11, 12), year(week), year(week) - 1)) |>
  mutate(season = str_c(season, season + 1, sep = "/")) |>
  mutate(numeric_value = as.numeric(censored_value)) |>
  arrange(site_description, desc(season))

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(base_font = "Century Gothic", bg = "#005870", fg = "white"),

    # Application title
    titlePanel("City of Cape Town Blue Flag Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("site_id", label = "Select a beach:", choices = set_names(blue_flag$site_id, blue_flag$site_description), selected = "Camps Bay South"),
            selectInput("season", label = "Select a season:", choices = blue_flag$season, multiple = T, selected = "2024/2025"),
            tableOutput("exceedances"),
            DT::dataTableOutput("table")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot", height = "60%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  thematic::thematic_shiny()

    output$plot <- renderPlot(res = 144, {
        blue_flag |>
        filter(site_id == input$site_id, season %in% input$season) |>
        ggplot(aes(x = week, y = numeric_value, group = site_id)) +
        labs(x = "Week", y = "Enterococci (cfu per 100 mL)") +
        geom_point() +
        geom_line() +
        facet_wrap(season~., scales = "free_x") +
        theme(
          text = element_text(family = "Century Gothic")
        )
    })
    
    table <- reactive({
      blue_flag |>
        filter(site_id == input$site_id, season %in% input$season) |>
        select(week, censored_value) |>
        arrange(week) |>
        mutate(week = as.character(week)) |>
        set_names(c("Week", "Enterococci (cfu per 100 mL)"))
    })
    output$table <- DT::renderDataTable(table() |> DT::datatable(options = list(scrollY = T, dom = "t")))
    
    exceedances <- reactive({
      blue_flag |>
        filter(site_id == input$site_id, season %in% input$season, numeric_value > 100) |>
        select(week, numeric_value) |>
        mutate(numeric_value = round(numeric_value)) |>
        mutate(week = as.character(week)) |>
        set_names(c("Week", "Enterococci (cfu per 100 mL)"))
    })
    output$exceedances <- renderTable(exceedances(), caption = "Samples that exceed the Blue Flag threshold for Enterococci of 100 cfu per 100 mL")
}

# Run the application 
shinyApp(ui = ui, server = server)