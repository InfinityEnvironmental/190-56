library(shiny)
library(tidyverse)
library(dbplyr)
library(DBI)
library(sf)
library(keyring)
library(kableExtra)
library(extrafont)

# Connect to database
con <- dbConnect(RPostgres::Postgres(),
                 host = "aws-0-eu-central-1.pooler.supabase.com",
                 port = 5432,
                 user = str_c(key_list(service = "supabase")$username, "wnrvdvesovnkmqbhkhjz", sep = "."),
                 password = key_get(service = "supabase", username = key_list(service = "supabase")$username),
                 dbname = "postgres"
)

# Load data from database
blue_flag <- tbl(con, I("coastal.sites_view")) |>
  inner_join(tbl(con, I("coastal.results_view"))) |>
  filter(monitoring_group == "blue_flag") |>
  select(site_id, site_description, long, lat, sample_date, censored_value, numeric_value, filename, lab_code) |>
  collect()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
