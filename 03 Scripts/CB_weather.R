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
library(tidyr)
library(ggplot2)

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


monthly_data <- daily_weather_data %>%
  mutate(Date_Only = as.Date(Date_Only)) %>%
  mutate(
    Year_Month = floor_date(Date_Only, "month")
  ) %>%
  group_by(Year_Month) %>%
  summarise(
    LOUR06BRS_MonthlySum = sum(LOUR06BRS_DailySum, na.rm = TRUE),
    CITY11BR_MonthlySum  = sum(CITY11BR_DailySum, na.rm = TRUE),
    DIEP05ER_MonthlySum  = sum(DIEP05ER_DailySum, na.rm = TRUE),
    STRA01RS_MonthlySum  = sum(STRA01RS_DailySum, na.rm = TRUE),
    .groups = 'drop' # Ungroup after summarising
  )
dates_to_exclude <- as.Date()

# Filter the monthly data to exclude these dates
monthly_data <- monthly_data %>%
  filter(!Year_Month %in% c("2024-04-01", "2025-09-01", "2025-10-01"))

#########Plotting daily/monthly time series######################################################################
###Strand
# --- Plot 1: LOUR06BRS and STRA01RS ---

# 1. Select the relevant columns and calculate the average
plot1_data <- monthly_data %>%
  select(Year_Month, LOUR06BRS_MonthlySum, STRA01RS_MonthlySum) %>%
  # Convert Date_Only to Date type if it isn't already
  mutate(Year_Month = as.Date(Year_Month)) %>%
  # Calculate the average
  mutate(
    Average_strand = (LOUR06BRS_MonthlySum + STRA01RS_MonthlySum) / 2
  )

# 2. Reshape the data for plotting all three lines (2 stations + 1 average)
plot1_long <- plot1_data %>%
  pivot_longer(
    cols = c(LOUR06BRS_MonthlySum, STRA01RS_MonthlySum, Average_strand),
    names_to = "Station",
    values_to = "MonthlySum"
  ) %>%
  mutate(
    Series = factor(Station, levels = c("LOUR06BRS_MonthlySum", "STRA01RS_MonthlySum", "Average_strand"))
  )

# 3. Create the plot
plot1 <- ggplot(plot1_long, aes(x = Year_Month, y = MonthlySum, color = Station)) +
  geom_line(aes(
    # Make the Average line thicker and maybe dashed
    linetype = ifelse(Series == "Average_strand", "Average_strand", "Station"),
    size = ifelse(Series == "Average_strand", 0.6, 0.6)
  )) +
  scale_linetype_manual(values = c("Average_strand" = "dashed", "Station" = "solid"), guide = "none") +
  scale_size_identity(guide = "none") +
  labs(
    title = "Total monthly rainfall for Strand",
    x = "Date",
    y = "Total monthly rainfall (mm)",
    color = "Station"
  ) +
  theme_bw()

print(plot1)


# --- Plot 2: CITY11BR and DIEP05ER ---

# 1. Select the relevant columns and calculate the average

plot2_data <- monthly_data %>%
  select(Year_Month, CITY11BR_MonthlySum, DIEP05ER_MonthlySum) %>%
  mutate(Year_Month = as.Date(Year_Month)) %>%
  # Calculate the average
  mutate(
    Average_cb = (CITY11BR_MonthlySum + DIEP05ER_MonthlySum) / 2
  )

# 2. Reshape the data for plotting all three lines
plot2_long <- plot2_data %>%
  pivot_longer(
    cols = c(CITY11BR_MonthlySum, DIEP05ER_MonthlySum, Average_cb),
    names_to = "Station",
    values_to = "MonthlySum"
  ) %>%
  mutate(
    Series = factor(Station, levels = c("CITY11BR_MonthlySum", "DIEP05ER_MonthlySum", "Average_cb"))
  )

# 3. Create the plot
plot2 <- ggplot(plot2_long, aes(x = Year_Month, y = MonthlySum, color = Station)) +
  geom_line(aes(
    linetype = ifelse(Series == "Average_cb", "Average_cb", "Station"),
    size = ifelse(Series == "Average_cb", 0.6, 0.6)
  )) +
  scale_linetype_manual(values = c("Average_cb" = "dashed", "Station" = "solid"), guide = "none") +
  scale_size_identity(guide = "none") +
  labs(
    title = "Total monthly rainfall for Camps Bay",
    x = "Date",
    y = "Total monthly rainfall (mm)",
    color = "Station"
  ) +
  theme_bw()

print(plot2)

#####Monthly bar plots#############################

# --- Plot 1: Strand Stations (LOUR06BRS & STRA01RS) ---
plot1_data <- monthly_data %>%
  select(Year_Month, LOUR06BRS_MonthlySum, STRA01RS_MonthlySum) %>%
  # Convert Date_Only to Date type if it isn't already
  mutate(Year_Month = as.Date(Year_Month)) %>%
  # Calculate the average
  mutate(
    Average_strand = (LOUR06BRS_MonthlySum + STRA01RS_MonthlySum) / 2
  )

# 2. Reshape the data for plotting only the two station bars
plot1_long_bar <- plot1_data %>%
  # Pivot only the two stations we want to plot as bars
  pivot_longer(
    cols = c(LOUR06BRS_MonthlySum, STRA01RS_MonthlySum),
    names_to = "Station",
    values_to = "MonthlySum"
  ) %>%
  # Create cleaner labels for the legend
  mutate(
    Station_Label = case_when(
      Station == "LOUR06BRS_MonthlySum" ~ "LOUR06BRS",
      Station == "STRA01RS_MonthlySum" ~ "STRA01RS",
      .default = Station # Fallback
    )
  )

# 3. Create the grouped bar plot
plot1_bar <- ggplot(plot1_long_bar, aes(x = Year_Month, y = MonthlySum, fill = Station_Label)) +
  geom_col(
    aes(y = MonthlySum, fill = Station_Label), 
    position = "dodge", # This is crucial: it puts bars side-by-side per month
    color = "black",    # Adds a black border to the bars
    width = 25          # Adjust width to make space for the dodge and gap between groups
  ) +
  geom_line(
    aes(y = Average_strand, color = "Average_strand"), 
    linewidth = 0.6
  ) +
  scale_fill_manual(
    values = c("LOUR06BRS" = "black", "STRA01RS" = "white"),
    name = "Station"
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Average_strand" = "darkgrey"),
    labels = c("Average_strand" = "Average")
  ) +
  # Format the x-axis to show month/year clearly
  scale_x_date(breaks = unique(plot1_long_bar$Year_Month), labels = format(unique(plot1_long_bar$Year_Month), "%b %Y")) +
  labs(
    title = "Total Monthly Rainfall for Strand Stations",
    x = "Month",
    y = "Total Monthly Rainfall (mm)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

print(plot1_bar)
ggsave(filename = "Strand_Monthly_Rainfall.png",
       plot = plot1_bar, width = 170, height = 140, units = "mm", dpi = 600)

# --- Plot 2: Camps Bay Stations (CITY11BR & DIEP05ER) ---

# 1. Select the relevant columns and calculate the average
plot2_data <- monthly_data %>%
  select(Year_Month, CITY11BR_MonthlySum, DIEP05ER_MonthlySum) %>%
  mutate(Year_Month = as.Date(Year_Month)) %>%
  mutate(
    Average_cb = (CITY11BR_MonthlySum + DIEP05ER_MonthlySum) / 2
  )

# 2. Reshape the data for plotting only the two station bars
plot2_long_bar <- plot2_data %>%
  pivot_longer(
    cols = c(CITY11BR_MonthlySum, DIEP05ER_MonthlySum),
    names_to = "Station",
    values_to = "MonthlySum"
  ) %>%
  mutate(
    Station_Label = case_when(
      Station == "CITY11BR_MonthlySum" ~ "CITY11BR",
      Station == "DIEP05ER_MonthlySum" ~ "DIEP05ER",
      .default = Station
    )
  )

# 3. Create the grouped bar plot
plot2_bar <- ggplot(plot2_long_bar, aes(x = Year_Month, y = MonthlySum, fill = Station_Label)) +
  geom_col(
    aes(y = MonthlySum, fill = Station_Label), 
    position = "dodge",
    color = "black",
    width = 25
  ) +
  geom_line(
    aes(y = Average_cb, color = "Average_cb"), 
    linewidth = 0.6
  ) +
  scale_fill_manual(
    values = c("CITY11BR" = "black", "DIEP05ER" = "white"), # Custom colors
    name = "Station"
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Average_cb" = "darkgrey"),
    labels = c("Average_cb" = "Average")
  ) +
  scale_x_date(breaks = unique(plot1_long_bar$Year_Month), labels = format(unique(plot1_long_bar$Year_Month), "%b %Y")) +
  labs(
    title = "Total Monthly Rainfall for Camps Bay Stations",
    x = "Month",
    y = "Total Monthly Rainfall (mm)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

print(plot2_bar)
ggsave(filename = "Camps_Bay_Monthly_Rainfall.png",
  plot = plot2_bar, width = 170, height = 140, units = "mm", dpi = 600)






















