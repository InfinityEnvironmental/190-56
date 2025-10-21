#CB High frequency paper
#CB vs Strand
#Robyn Daniels

library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(lubridate)
library(janitor)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(DBI)
library(readODS)
library(pdftools)
library(sf)
library(gpx)
library(units)
#install.packages("gtable")
library(ggpattern)
library(extrafont)
library(stringr)
loadfonts()
library(scales)
library(patchwork)

###############################Rainfall###########################################################################################
# Set a working directory
setwd("C:/Users/RobynDaniels/Desktop/Weather/All months")

###### Read and Combine All Weather Files ######
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

#####Filter needed weather stations and aggregate to daily data #####

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
  
  # Aggregate (take the sum) for the selected weather stations.
  summarise(
    across(
      .cols = all_of(target_stations), 
      .fns = \(x) sum(x, na.rm = TRUE), 
      .names = "{.col}_DailySum"
    )
  ) %>%
  ungroup()

hourly_weather_data <- combined_data %>%
  
  # === 1. Ensure Weather Columns are Numeric ===
  mutate(
    across(
      .cols = all_of(target_stations),
      .fns = as.numeric
    )
  ) %>%
  
  # === 2. Data Cleaning and Conversion (Ensures DATE_Time is accurate) ===
  # (Steps for creating DATE_Time column remain the same)
  mutate(DATE_Numeric = as.numeric(as.character(DATE))) %>%
  mutate(DATE_BaseR = as_datetime(DATE_Numeric * 86400, origin = "1899-12-30", tz = "UTC")) %>%
  mutate(DATE_Time = coalesce(
    DATE_BaseR,              
    as_datetime(as.character(DATE)) 
  )) %>%
  
  # 🔴 CRITICAL FIX: Create a new column with the time floored to the nearest hour
  mutate(Hourly_Timestamp = floor_date(DATE_Time, unit = "hour")) %>%
  
  # === 3. Filter Columns and Aggregate ===
  # Select the new hourly timestamp and the target columns
  select(Hourly_Timestamp, all_of(target_stations)) %>%
  
  # 🔴 FIX: Group by the new Hourly_Timestamp
  # This groups all 5-minute entries (e.g., 10:00, 10:05, ... 10:55) under the 10:00 timestamp.
  group_by(Hourly_Timestamp) %>%
  
  # Aggregate (take the sum) for the selected weather stations.
  summarise(
    across(
      .cols = all_of(target_stations), 
      .fns = \(x) sum(x, na.rm = TRUE), 
      .names = "{.col}_HourlySum"
    )
  ) %>%
  ungroup()


weekly_rainfall <- daily_weather_data %>%
  mutate(Week_Start = floor_date(Date_Only, "week")) %>%
  group_by(Week_Start) %>%
  summarise(
    LOUR06BRS_WeeklySum = sum(LOUR06BRS_DailySum, na.rm = TRUE),
    CITY11BR_WeeklySum  = sum(CITY11BR_DailySum, na.rm = TRUE),
    DIEP05ER_WeeklySum  = sum(DIEP05ER_DailySum, na.rm = TRUE),
    STRA01RS_WeeklySum  = sum(STRA01RS_DailySum, na.rm = TRUE),
    .groups = 'drop' 
  )

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

#########Plotting daily/monthly rainfall time series######################################################################
###Strand
# --- Plot 1: LOUR06BRS and STRA01RS ---

# 1. Select the relevant columns and calculate the average
plot1_data <- weekly_rainfall %>%
  select(Week_Start, LOUR06BRS_WeeklySum, STRA01RS_WeeklySum) %>%
  mutate(Week_Start = as.Date(Week_Start)) %>%
  # Calculate the average
  mutate(
    Average_strand = (LOUR06BRS_WeeklySum + STRA01RS_WeeklySum) / 2
  )

# 2. Reshape the data for plotting all three lines (2 stations + 1 average)
plot1_long <- plot1_data %>%
  pivot_longer(
    cols = c(LOUR06BRS_WeeklySum, STRA01RS_WeeklySum, Average_strand),
    names_to = "Station",
    values_to = "WeeklySum"
  ) %>%
  mutate(
    Series = factor(Station, levels = c("LOUR06BRS_WeeklySum", "STRA01RS_WeeklySum", "Average_strand"))
  ) %>%
  # Create cleaner labels for the legend
  mutate(
    Station_Label = case_when(
      Station == "LOUR06BRS_WeeklySum" ~ "LOUR06BRS",
      Station == "STRA01RS_WeeklySum" ~ "STRA01RS",
      Station == "Average_strand" ~ "Average",
      .default = Station # Fallback
    )
  )

# 3. Create the plot
plot1 <- ggplot(plot1_long, aes(x = Week_Start, y = WeeklySum, color = Station_Label)) +
  geom_line(aes(
    # Make the Average line thicker and maybe dashed
    linetype = ifelse(Series == "Average_strand", "Average_strand", "Station"),
    size = ifelse(Series == "Average_strand", 0.4, 0.4)
  )) +
  scale_linetype_manual(values = c("Average_strand" = "dashed", "Station" = "solid"), guide = "none") +
  scale_size_identity(guide = "none") +
  labs(
    title = "Total weekly rainfall for Strand",
    x = "Date",
    y = "Total weekly rainfall (mm)",
    color = "Station"
  ) +
  scale_x_date(
    date_labels = "%d %b %Y",
    limits = as.Date(c("2024-04-28", "2025-08-31")),
    # Manually define breaks every 3 months, starting May 1st, 2024
    breaks = seq(from = as.Date("2024-04-28"), to = as.Date("2025-09-14"), by = "3 weeks"),
    oob = scales::oob_keep  
  ) +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

print(plot1)
ggsave(filename = "Strand_Weekly_Rainfall.png",
       plot = plot1, width = 170, height = 90, units = "mm", dpi = 600)


# --- Plot 2: CITY11BR and DIEP05ER ---

# 1. Select the relevant columns and calculate the average

plot2_data <- weekly_rainfall %>%
  select(Week_Start, CITY11BR_WeeklySum, DIEP05ER_WeeklySum) %>%
  mutate(Week_Start = as.Date(Week_Start)) %>%
  # Calculate the average
  mutate(
    Average_cb = (CITY11BR_WeeklySum + DIEP05ER_WeeklySum) / 2
  )

# 2. Reshape the data for plotting all three lines
plot2_long <- plot2_data %>%
  pivot_longer(
    cols = c(CITY11BR_WeeklySum, DIEP05ER_WeeklySum, Average_cb),
    names_to = "Station",
    values_to = "WeeklySum"
  ) %>%
  mutate(
    Series = factor(Station, levels = c("CITY11BR_WeeklySum", "DIEP05ER_WeeklySum", "Average_cb"))
  )%>%
  # Create cleaner labels for the legend
  mutate(
    Station_Label = case_when(
      Station == "CITY11BR_WeeklySum" ~ "CITY11BR",
      Station == "DIEP05ER_WeeklySum" ~ "DIEP05ER",
      Station == "Average_cb" ~ "Average",
      .default = Station # Fallback
    )
  )

# 3. Create the plot
plot2 <- ggplot(plot2_long, aes(x = Week_Start, y = WeeklySum, color = Station_Label)) +
  geom_line(aes(
    linetype = ifelse(Series == "Average_cb", "Average_cb", "Station"),
    size = ifelse(Series == "Average_cb", 0.4, 0.4)
  )) +
  scale_linetype_manual(values = c("Average_cb" = "dashed", "Station" = "solid"), guide = "none") +
  scale_size_identity(guide = "none") +
  labs(
    title = "Total weekly rainfall for Camps Bay",
    x = "Date",
    y = "Total weekly rainfall (mm)",
    color = "Station"
  ) +
  scale_x_date(
    date_labels = "%d %b %Y",
    limits = as.Date(c("2024-04-28", "2025-08-31")),
    # Manually define breaks every 3 months, starting May 1st, 2024
    breaks = seq(from = as.Date("2024-04-28"), to = as.Date("2025-09-14"), by = "3 weeks"),
    oob = scales::oob_keep  
  ) +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

print(plot2)
ggsave(filename = "CB_Weekly_Rainfall.png",
       plot = plot2, width = 170, height = 90, units = "mm", dpi = 600)

#####Monthly rainfall bar plots#############################

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



###############################Camps Bay bacteria ###############################################################
#Connect to the database ####
con <- dbConnect(RPostgres::Postgres(),
                 host = "aws-0-eu-central-1.pooler.supabase.com",
                 port = 5432,
                 user = str_c(rstudioapi::askForPassword(prompt = "Username: "), "wnrvdvesovnkmqbhkhjz", sep = "."),
                 password = rstudioapi::askForPassword(),
                 dbname = "postgres"
)

#### Data preparation ####
cb_high <- tbl(con, I("coastal.high_frequency_sampling_view")) %>%
  collect()

coastal <- tbl(con, I("coastal.results_view")) %>%
  collect()

coastal_sites <- tbl(con, I("coastal.sites_view")) %>%
  collect()

coastal_data <- left_join(
  x = coastal, 
  y = coastal_sites, 
  by = "site_id"
)

coastal_data <- coastal_data %>%
  mutate(
    sample_date = ymd(sample_date)
  ) %>%
  filter(
    site_id %in% c("CN11", "CN31", "CN41", "CN10", "XCS34", "XCS26") &
      monitoring_group == "daily" &
      sample_date >= as.Date("2024-06-01") &
      sample_date <= as.Date("2025-08-30")
  ) %>%
  mutate(
    site_group = case_when(
      str_detect(site_id, "^CN") ~ "Camps Bay",
      str_detect(site_id, "^XCS") ~ "Strand",
      TRUE ~ "Other Site" # This catches any other sites if they exist
    )
  )
#write.csv(coastal_data, "filtered_coastal_data.csv", row.names = FALSE)
#write.csv(cb_high, "cb_high_freq_data.csv", row.names = FALSE)

#### Yearly Box plots #########################################################################
p <- ggplot(data = coastal_data, aes(x = site_id, y = numeric_value + 1, fill = site_group)) +
  geom_boxplot(
    colour = "black"
  ) +
  scale_y_log10(
    labels = scales::label_comma(), # Formats labels (e.g., 1000 instead of 1e+03)
    breaks = c(1, 10, 100, 1000, 10000) # Sets specific log intervals for clarity
  ) +
  # Manually set the patterns for the two groups
  scale_fill_manual(
    values = c(
      "Camps Bay" = "grey",
      "Strand" = "white"
    )
  ) +
  # Add titles and labels for clarity
  labs(
    title = "Yearly Distribution of Enterococci Counts \n (June 2024 - August 2025)",
    x = "Site ID",
    y = "log(Enterococci Counts (CFU per 100 ml) + 1)",
    fill = "Site Group"
  ) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
  )
p
ggsave(filename = "Yearly_boxplots.png",
       plot = p, width = 170, height = 90, units = "mm", dpi = 600)

######Monthly Boxplots####################################
#Filter only for Camps Bay
coastal_cb <- coastal_data %>%
  filter(
    site_id %in% c("CN11", "CN31", "CN41", "CN10") 
  ) %>%
  mutate(
    sample_date = ymd(sample_date), 
    sample_month_abbr = month(sample_date, label = TRUE, abbr = TRUE),
    sample_year = year(sample_date)
  ) %>%
    arrange(sample_date) %>%
  mutate(
    sample_month_year = factor(
      paste(sample_month_abbr, sample_year),
      levels = unique(paste(sample_month_abbr, sample_year))
    )
  )


#Create the monthly boxplots for Camps Bay
p <- ggplot(data = coastal_cb, aes(x = sample_month_year, y = numeric_value + 1, fill = site_group)
) +
  # Use geom_boxplot for the distribution
  geom_boxplot(colour = "black", linewidth = 0.3
  ) +
  # Use facet_wrap to create a separate plot for each site_id
  facet_wrap(~ site_id, scales = "free_x", ncol = 1) + 
  # Log scale for y-axis (using +1 to handle zeros)
  scale_y_log10(
    labels = scales::label_comma(),
    breaks = c(1, 10, 100, 1000, 10000)
  ) +
  # Manual Fill Colors
  scale_fill_manual(
    values = c(
      "Camps Bay" = "grey50"   
    )
  ) +
  labs(
    title = "Monthly Distribution of Enterococci Counts \n (Camps Bay June 2024 - August 2025)",
    x = "Month",
    y = "log(Enterococci Counts (CFU per 100 ml) + 1)",
 #   fill = "Site Group" 
  ) +
  theme_bw() +
  theme(
    # Apply Century Gothic font
    text = element_text(family = "Century Gothic", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", family = "Century Gothic", size = 9) 
  )
p
ggsave(filename = "Monthly_boxplots_CB.png",
       plot = p, width = 170, height = 120, units = "mm", dpi = 600)

#Filter only for Strand
coastal_strand <- coastal_data %>%
  filter(
    site_id %in% c("XCS34", "XCS26") 
  ) %>%
  mutate(
    sample_date = ymd(sample_date), 
    sample_month_abbr = month(sample_date, label = TRUE, abbr = TRUE),
    sample_year = year(sample_date)
  ) %>%
  arrange(sample_date) %>%
  mutate(
    sample_month_year = factor(
      paste(sample_month_abbr, sample_year),
      levels = unique(paste(sample_month_abbr, sample_year))
    )
  )

#Create the monthly boxplots for Camps Bay
p <- ggplot(data = coastal_strand, aes(x = sample_month_year, y = numeric_value + 1, fill = site_group)
) +
  # Use geom_boxplot for the distribution
  geom_boxplot(colour = "black", linewidth = 0.3
  ) +
  # Use facet_wrap to create a separate plot for each site_id
  facet_wrap(~ site_id, scales = "free_x", ncol = 1) + 
  # Log scale for y-axis (using +1 to handle zeros)
  scale_y_log10(
    labels = scales::label_comma(),
    breaks = c(1, 10, 100, 1000, 10000)
  ) +
  # Manual Fill Colors
  scale_fill_manual(
    values = c(
      "Strand" = "white"     
    )
  ) +
  labs(
    title = "Monthly Distribution of Enterococci Counts \n (Strand September 2024 - August 2025)",
    x = "Month",
    y = "log(Enterococci Counts (CFU per 100 ml) + 1)",
    #   fill = "Site Group" 
  ) +
  theme_bw() +
  theme(
    # Apply Century Gothic font
    text = element_text(family = "Century Gothic", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", family = "Century Gothic", size = 9)  
  )
p
ggsave(filename = "Monthly_boxplots_strand.png",
       plot = p, width = 170, height = 120, units = "mm", dpi = 600)

#######Seasonal box plots###########################
coastal_cb_seasonal <- coastal_cb %>%
  mutate(
    season = case_when(
      sample_month_abbr %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sept") ~ "Wet Season",
      sample_month_abbr %in% c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar") ~ "Dry Season",
      TRUE ~ "Unknown"
    )
  ) %>%
  arrange(sample_date) %>%
  mutate(
    season = factor(season, levels = c("Wet Season", "Dry Season"), ordered = TRUE)
  )


#Create the monthly boxplots for Camps Bay
p <- ggplot(data = coastal_cb_seasonal, aes(x = season, y = numeric_value + 1, fill = site_group)
) +
  # Use geom_boxplot for the distribution
  geom_boxplot(colour = "black", linewidth = 0.3
  ) +
  # Use facet_wrap to create a separate plot for each site_id
  facet_wrap(~ site_id, scales = "free_x", ncol = 3) + 
  # Log scale for y-axis (using +1 to handle zeros)
  scale_y_log10(
    labels = scales::label_comma(),
    breaks = c(1, 10, 100, 1000, 10000)
  ) +
  # Manual Fill Colors
  scale_fill_manual(
    values = c(
      "Camps Bay" = "grey50"     
    )
  ) +
  labs(
    title = "Seasonal Distribution of Enterococci Counts \n (Camps Bay June 2024 - August 2025)",
    x = "Season",
    y = "log(Enterococci Counts (CFU per 100 ml) + 1)",
    #   fill = "Site Group" 
  ) +
  theme_bw() +
  theme(
    # Apply Century Gothic font
    text = element_text(family = "Century Gothic", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", family = "Century Gothic", size = 9) 
  )
p
ggsave(filename = "Seasonal_boxplots_CB.png",
       plot = p, width = 170, height = 120, units = "mm", dpi = 600)

#Filter only for Strand
coastal_strand_seasonal <- coastal_strand %>%
  mutate(
    season = case_when(
      sample_month_abbr %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sept") ~ "Wet Season",
      sample_month_abbr %in% c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar") ~ "Dry Season",
      TRUE ~ "Unknown"
    )
  ) %>%
  arrange(sample_date) %>%
  mutate(
    season = factor(season, levels = c("Wet Season", "Dry Season"), ordered = TRUE)
  )

#Create the monthly boxplots for Camps Bay
p <- ggplot(data = coastal_strand_seasonal, aes(x = season, y = numeric_value + 1, fill = site_group)
) +
  # Use geom_boxplot for the distribution
  geom_boxplot(colour = "black", linewidth = 0.3
  ) +
  # Use facet_wrap to create a separate plot for each site_id
  facet_wrap(~ site_id, scales = "free_y", ncol = 3) + 
  # Log scale for y-axis (using +1 to handle zeros)
  scale_y_log10(
    trans = scales::log10_trans(add = 1),
    labels = scales::label_comma(),
    breaks = c(1, 10, 100, 1000, 10000)
  ) +
  # Manual Fill Colors
  scale_fill_manual(
    values = c(
      "Strand" = "white"     
    )
  ) +
  labs(
    title = "Seasonal Distribution of Enterococci Counts \n (Strand September 2024 - August 2025)",
    x = "Season",
    y = "log(Enterococci Counts (CFU per 100 ml) + 1)",
    #   fill = "Site Group" 
  ) +
  theme_bw() +
  theme(
    # Apply Century Gothic font
    text = element_text(family = "Century Gothic", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", family = "Century Gothic", size = 9)  
  )
p
ggsave(filename = "Seasonal_boxplots_strand.png",
       plot = p, width = 170, height = 120, units = "mm", dpi = 600)

########Camps Bay High Frequency Data Plotting ###############################################
cb_high_prepared <- cb_high %>%
  mutate(tag = replace_na(tag, "single")) %>%
  mutate(sample_date = ymd(sample_date)) %>%
  mutate(
    date_time = as.POSIXct(paste(sample_date, sample_time), format = "%Y-%m-%d %H:%M:%S")
  ) %>%
  mutate(tag = as.factor(tag))

#Filtering hourly rainfall data
target_hourly_cols <- c("LOUR06BRS_HourlySum", "CITY11BR_HourlySum", 
                        "DIEP05ER_HourlySum", "STRA01RS_HourlySum")
rainfall_6_hourly <- hourly_weather_data %>%
  mutate(Date_Only = as_date(Hourly_Timestamp)) %>% 
  filter(
    # 1. Filter the overall date range (19 May 2025 to 30 May 2025)
    Date_Only >= as_date("2025-05-19") & Date_Only <= as_date("2025-05-30")
  ) %>%
  filter(
    # 2. Exclude the dates 24 May 2025 and 25 May 2025
    !(Date_Only %in% as_date(c("2025-05-24", "2025-05-25")))
  ) %>%
  
  # Create the 'week' column based on the filtered dates
  mutate(
    week = case_when(
      # Week 1: 19 May to 23 May
      Date_Only >= as_date("2025-05-19") & Date_Only <= as_date("2025-05-23") ~ "Week 1", 
      # Week 2: 26 May to 30 May
      Date_Only >= as_date("2025-05-26") & Date_Only <= as_date("2025-05-30") ~ "Week 2", 
      TRUE ~ NA_character_
    )
  ) %>%
  # Floor the Hourly_Timestamp down to the start of the 6-hour interval
  mutate(Six_Hourly_Timestamp = floor_date(Hourly_Timestamp, unit = "6 hours")) %>% 
  group_by(week, Six_Hourly_Timestamp) %>%
  summarise(
    across(
      .cols = all_of(target_hourly_cols), 
      .fns = \(x) sum(x, na.rm = TRUE), 
      # Rename columns to reflect the new 6-hourly sum
      .names = "{.col}_6HourlySum"
    ),
    .groups = "drop" 
  ) %>%
  # Clean up column names by removing the intermediate "_HourlySum" part
  rename_with(~ sub("_HourlySum", "", .x), .cols = ends_with("_HourlySum_6HourlySum")) %>%
  rename_with(~ sub("_6HourlySum", "", .x), .cols = ends_with("_6HourlySum")) %>%
  
  # Final tidy-up of column names for clarity
  rename_with(~ paste0(.x, "_6HourlySum"), .cols = all_of(target_stations))


rainfall_to_plot <- rainfall_6_hourly %>%
  mutate(
    # Calculate the average rainfall between the two specified stations
    Rainfall_Avg = (CITY11BR_6HourlySum + DIEP05ER_6HourlySum) / 2
  ) %>%
  # Select columns needed for the rainfall layer and for faceting
  select(
    Six_Hourly_Timestamp,
    Rainfall_Avg,
    week
  )

# Add the 'week' column to your bacteria data for consistent faceting.
# The week definition must match the one used in the rainfall prep.
cb_high_plotting <- cb_high_prepared %>%
  mutate(
    Date_Only = as_date(date_time), # Create Date_Only column to join/match week
    # Define the week using the *same logic* as in your rainfall preparation
    week = case_when(
      Date_Only >= as_date("2025-05-19") & Date_Only <= as_date("2025-05-23") ~ "Week 1",
      Date_Only >= as_date("2025-05-26") & Date_Only <= as_date("2025-05-30") ~ "Week 2",
      TRUE ~ NA_character_
    )
  ) %>%
  # Filter out NA weeks if any bacteria data falls in the excluded dates (May 24/25)
  filter(!is.na(week)) %>%
  filter(numeric_value >= 0, !is.na(numeric_value))


# Redefine the custom transformation to use +0.1 for the axis
log_plus_01_trans <- scales::trans_new(
  name = "log10_plus_01",
  transform = function(x) log10(x + 0.1),
  inverse = function(x) 10^x - 0.1
)

TARGET_SITE_ID <- "CN11"

# --- Primary (Bacteria) Axis Parameters ---
# --- DYNAMIC BACTERIA MAX (Standard Breaks) ---
ACTUAL_MAX_BACT <- max(cb_high_plotting$numeric_value, na.rm = TRUE)
# Find the next highest standard log break (e.g., 10000)
BACT_MAX_RAW <- 10^ceiling(log10(ACTUAL_MAX_BACT + 0.1)) 
BACT_MAX_RAW <- max(BACT_MAX_RAW, 10) 

# 💥 NEW: Max plot height is the log of BACT_MAX_RAW (This is the plot's Y-max coordinate)
BACT_MAX_PLOT_HEIGHT <- log10(BACT_MAX_RAW + 0.1) 
# Min plot height (corresponds to 0 CFU)
Y_MIN_LOG <- log10(0 + 0.1) # -1.0

# --- DYNAMIC RAINFALL MAX ---
RAIN_MAX_LIN <- max(rainfall_to_plot$Rainfall_Avg, na.rm = TRUE)
RAIN_MAX_LIN <- RAIN_MAX_LIN * 1.05 # 5% buffer
RAIN_MIN_LIN <- 0 

# --- Calculate Factor Z ---
# Z = (Total Plot Height) / (Total Rainfall Range)
# Total Plot Height: BACT_MAX_PLOT_HEIGHT - Y_MIN_LOG (e.g., 4.0 - (-1.0) = 5.0)
Z_PLOT_RANGE <- BACT_MAX_PLOT_HEIGHT - Y_MIN_LOG
RAIN_RANGE <- RAIN_MAX_LIN - RAIN_MIN_LIN

z <- Z_PLOT_RANGE / RAIN_RANGE
if (is.infinite(z) || is.nan(z)) z <- 1 

# --- Dynamic Breaks and Labels for Primary Axis ---
# Breaks are the LOG COORDINATES (0, 1, 2, 3, 4, ...)
final_breaks_log_coords <- seq(0, BACT_MAX_PLOT_HEIGHT, by = 1) 
# Labels are the RAW VALUES (1, 10, 100, 1000, ...)
final_labels_raw <- 10^final_breaks_log_coords

# -----------------------------------------------------------
# 4. PLOTTING (MANUAL LOGGING - NO TRANS FUNCTION)
# -----------------------------------------------------------

single_site_plot <- ggplot() +
  
  # Layer 1: 6-hourly Rainfall (Bars)
  geom_col(
    data = rainfall_to_plot %>% filter(Rainfall_Avg > 0),
    aes(
      x = Six_Hourly_Timestamp, 
      # If Rainfall_Avg > 0, the bar's top is: Scaled_Rain + Y_MIN_LOG.
      # If Rainfall_Avg == 0, the bar's top is: Y_MIN_LOG (-1.0), meaning the bar is invisible or reduced to a line.
      y = Rainfall_Avg * z + Y_MIN_LOG
    ),
    fill = "lightgrey",
    color = NA,
    alpha = 0.7,
    width = 6 * 3600
  ) +
  
  # Layer 2: Hourly Bacteria Data 
  geom_point(
    data = cb_high_plotting,
    # 💥 FIX: MANUALLY LOG THE Y-AESTHETIC (Data is now plotted on a linear y-axis of log values)
    aes(x = date_time, y = log10(numeric_value + 0.1), color = tag), 
    size = 1.5,
    alpha = 0.7
  ) +
  
  # Faceting: Only by Week
  facet_wrap(
    ~ week,
    scales = "free_x", 
    ncol = 1
  ) +
  
  # --- Y-Axis with sec_axis and Factor Z ---
  scale_y_continuous(
    # Primary Y-Axis (Manual Log Scale)
    # NO 'trans' ARGUMENT!
    breaks = c(Y_MIN_LOG, final_breaks_log_coords), # Use log coordinates for breaks
    labels = c("ND", scales::label_comma()(final_labels_raw)), # Label -1 as ND
    name = paste0("log(Enterococci Counts (CFU/100 ml) + 0.1)"),
    limits = c(Y_MIN_LOG, BACT_MAX_PLOT_HEIGHT), # Limits are the plot coordinates (-1 to 4.x)
    
    # Secondary Y-Axis (Rainfall - LINEAR SCALE)
    sec.axis = sec_axis(
      # Inverse transform: (y - Y_MIN_LOG) / z
      # This correctly reverses the manual scaling done in geom_col
      trans = ~ (. - Y_MIN_LOG) / z, 
      breaks = unique(c(0, pretty(c(RAIN_MIN_LIN, RAIN_MAX_LIN), n = 5))),
      name = "6-Hourly Rainfall Avg (mm)"
    )
  ) +
  
  # --- X-Axis and Theme (Unchanged) ---
  scale_x_datetime(
    labels = date_format("%b %d\n%H:%M"),
    breaks = "6 hours"
  ) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Replicates") +
  theme_bw() +
  labs(
    title = paste("Enterococci vs. Rainfall for Site:", TARGET_SITE_ID),
    x = "Date and Time"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    axis.title.y.right = element_text(color = "darkred", face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "gray")
  )

print(single_site_plot)

 
 
 
 
 

