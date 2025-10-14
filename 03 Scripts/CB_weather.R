#CB High frequency paper
#CB vs Strand
#Robyn Daniels

###############################Rainfall###########################################################################################
library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(lubridate)
library(janitor)

# Set a wroking directory
setwd("C:/Users/RobynDaniels/Desktop/Weather/All months")

# Read and Combine All Files
# list.files() gets the names of all files in the directory ending with .csv
# map_dfr() reads each file and automatically combines them into a single data frame by row.
combined_data <- list.files(
  path = "C:/Users/RobynDaniels/Desktop/Weather/All months", 
  pattern = "\\.(xlsx|xls)$", 
  full.names = TRUE
)

read_excel_file <- function(path) {
  tryCatch({
    # 1. Read the entire spreadsheet. The first row (headers) is used for column names.
    data <- read_excel(path)
    
    # 2. 💡 CLEANING STEP: Remove the second row of the data frame
    # This row contains the unwanted header description/units.
    if (nrow(data) > 1) {
      # The [-2, ] syntax tells R to keep all rows EXCEPT the one at index 2
      data <- data[-1, ] 
    }
    
    return(data)
    
  }, error = function(e) {
    # Print a message for the file that failed and return NULL to skip it
    message(paste("Skipping file due to error:", basename(path)))
    # message(paste("Error:", e$message)) # Uncomment if you want detailed errors
    return(NULL)
  })
}

# Combine All Files
# map_dfr iterates through the list of files, reads each one, and binds 
# the resulting data frames into a single object.
combined_data <- combined_data %>% 
  map_dfr(read_excel_file)


# Save the combined data
#write_csv(combined_data, "combined_5min_weather_data.csv")

#####Filter needed weather stations and aggregate to daily data

target_stations <- c("LOUR06BRS", "CITY11BR", "DIEP05ER", "STRA01RS")

# Run the aggregation pipeline
# Run the aggregation pipeline
daily_weather_data <- combined_data %>%
  
  # === 1. Ensure Weather Columns are Numeric (Necessary for aggregation) ===
  # This converts any remaining text in the weather columns to NA, enabling mean()
  mutate(
    across(
      .cols = all_of(target_stations),
      .fns = as.numeric
    )
  ) %>%
  
  # === 2. Data Cleaning and Conversion (Base R only) ===
  # Convert DATE to numeric. This turns Excel numbers into numeric values and text strings into NA.
  mutate(DATE_Numeric = as.numeric(as.character(DATE))) %>%
  
  # Convert the numeric date using base R, which assumes the 1900 origin.
  # This is the step that will create the 4-year offset (e.g., 2020 instead of 2024).
  mutate(DATE_BaseR = as_datetime(DATE_Numeric * 86400, origin = "1899-12-30", tz = "UTC")) %>%
  
  # Use coalesce to combine the Base R calculated date-time (for numbers) with 
  # the lubridate conversion (for text strings).
  mutate(DATE_Time = coalesce(
    DATE_BaseR,                # Calculated date (will be 4 years off for numeric cells)
    as_datetime(as.character(DATE)) # Directly convert the text string dates
  )) %>%
  
  # Create the final Date column by stripping the time
  mutate(Date_Only = as_date(DATE_Time)) %>%
  
  # === 3. Filter Columns and Aggregate ===
  select(Date_Only, all_of(target_stations)) %>%
  group_by(Date_Only) %>%
  
  # Aggregate (take the mean) for the selected weather stations.
  summarise(
    across(
      .cols = all_of(target_stations), 
      .fns = \(x) sum(x, na.rm = TRUE), 
      .names = "{.col}_DailySum"
    )
  ) %>%
  ungroup()






























