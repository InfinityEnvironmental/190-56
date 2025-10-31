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
#install.packages("ggpubr")
library(ggpubr)
library(ggh4x)

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

#####Filter needed weather stations and aggregate to daily, weekly, monthly data #####

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
#write.csv(daily_weather_data, "daily_rainfall_data.csv", row.names = FALSE)


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
#write.csv(hourly_weather_data, "hourly_rainfall_data.csv", row.names = FALSE)


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
#dates_to_exclude <- as.Date()

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



###############################Import coastal FIB data ###############################################################
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
percentile_boxplot_stats <- function(x) {
  # Calculate the required quantiles
  r <- as.vector(quantile(x, c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE))
  # Name the output elements for geom="boxplot" to recognize the new whisker endpoints
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}

# --- MANUAL TRANSFORMATION SETUP ---
# 1. Create the new column for the manually logged data
coastal_data$log_numeric_value <- log10(coastal_data$numeric_value + 0.1)

# 2. Define the correct y-intercept value (approx 2.267)
MANUAL_Y_INTERCEPT <- log10(185 + 0.1)

# 3. Define the breaks as the logged values
LOGGED_BREAKS <- log10(c(0.1, 1.1, 10.1, 100.1, 1000.1, 10000.1))

# These are the labels displayed on the axis (remain unchanged)
FINAL_LABELS_DISPLAY <- c("ND", "1", "10", "100", "1,000", "10,000")
# --- END SETUP ---


p <- ggplot(data = coastal_data, aes(x = site_id, y = log_numeric_value, fill = site_group)) +
  stat_summary(
    fun.data = percentile_boxplot_stats,
    geom = "boxplot",
    colour = "black"
  ) +
  geom_point(
    # Use position_jitterdodge to spread the points out slightly 
    # and align them with the boxplots (Camps Bay vs. Strand)
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
    size = 1,              # Adjust size as needed
    alpha = 0.7,           # Use transparency to avoid overplotting
    shape = 16,            # Solid circle shape
    color = "darkgrey"        # Use a contrasting color for visibility
  ) +
  geom_hline(
    # Use the manually calculated y-intercept value
    yintercept = MANUAL_Y_INTERCEPT,
    linetype = "dashed",  
    color = "darkred",  
    linewidth = 1,
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = 2, 
    # Position the text relative to the manually logged intercept
    y = MANUAL_Y_INTERCEPT+0.2, 
    label = "DWAF Guideline Threshold\n (185 cfu/100 ml)",
    color = "darkred",
    size = 3,
    hjust = 1,  
    vjust = 1
  ) +
  scale_y_continuous(
    # **CRITICAL FIX: REMOVE trans = "log10"**
    # Use the manually LOGGED breaks
    breaks = LOGGED_BREAKS, 
    # Use the clean labels (ND, 1, 10, etc.)
    labels = FINAL_LABELS_DISPLAY,
    # Adjust limits to the LOGGED range
    limits = log10(c(0.1, max(coastal_data$numeric_value, na.rm = TRUE) + 0.1)),
    # Set the axis name correctly
    name = "log(Enterococci Counts (CFU/100 ml) + 0.1)"
  ) +
  scale_fill_manual(
    values = c(
      "Camps Bay" = "grey",
      "Strand" = "white"
    )
  ) +
  labs(
    title = "Yearly Distribution of Enterococci Counts \n (June 2024 - August 2025)",
    x = "Site ID",
    fill = "Site Group"
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
ggsave(filename = "Yearly_boxplots_10th_90th_perc.png",
       plot = p, width = 170, height = 120, units = "mm", dpi = 600)

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
p <- ggplot(data = coastal_cb, aes(x = sample_month_year, y = log_numeric_value, fill = site_group)
) +
  stat_summary(
    fun.data = percentile_boxplot_stats,
    geom = "boxplot",
    colour = "black",
    size = 0.3
  ) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
    size = 1,              # Adjust size as needed
    alpha = 0.5,           # Use transparency to avoid overplotting
    shape = 16,            # Solid circle shape
    color = "darkgrey"        # Use a contrasting color for visibility
  ) +
  geom_hline(
    # Use the manually calculated y-intercept value
    yintercept = MANUAL_Y_INTERCEPT,
    linetype = "dashed",  
    color = "darkred",  
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  geom_text(
    # --- Define the data for the annotation inline ---
    data = data.frame(
      # IMPORTANT: Replace "CN11" with the actual site_id of your top facet
      site_id = factor("CN11"), 
      label_text = "DWAF Guideline Threshold\n(185 cfu/100 ml)",
      x_coord = 15, 
      y_coord = MANUAL_Y_INTERCEPT + 1.2 # Y position (above the line)
    ),
    aes(
      x = x_coord,
      y = y_coord,
      label = label_text
    ),
    color = "darkred",
    size = 2,
    hjust = 1,  
    vjust = 1,
    inherit.aes = FALSE # Ensures it uses the coordinates defined above, not the main data
  ) +
  # Use facet_wrap to create a separate plot for each site_id
  facet_wrap(~ site_id, scales = "free_x", ncol = 1) + 
  scale_y_continuous(
    # **CRITICAL FIX: REMOVE trans = "log10"**
    # Use the manually LOGGED breaks
    breaks = LOGGED_BREAKS, 
    # Use the clean labels (ND, 1, 10, etc.)
    labels = FINAL_LABELS_DISPLAY,
    # Adjust limits to the LOGGED range
    limits = log10(c(0.1, max(coastal_data$numeric_value, na.rm = TRUE) + 0.1)),
    # Set the axis name correctly
    name = "log(Enterococci Counts (CFU/100 ml) + 0.1)"
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
    y = "log(Enterococci Counts (CFU per 100 ml) + 0.1)"
  ) +
  theme_bw() +
  theme(
    # Apply Century Gothic font
    text = element_text(family = "Century Gothic", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", family = "Century Gothic", size = 9),
    legend.position = "none"
  )
p
ggsave(filename = "Monthly_boxplots_10th_90th_perc_CB.png",
       plot = p, width = 170, height = 160, units = "mm", dpi = 600)

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
p <- ggplot(data = coastal_strand, aes(x = sample_month_year, y = log_numeric_value, fill = site_group)
) +
  stat_summary(
    fun.data = percentile_boxplot_stats,
    geom = "boxplot",
    colour = "black",
    size = 0.3
  ) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
    size = 1,              # Adjust size as needed
    alpha = 0.5,           # Use transparency to avoid overplotting
    shape = 16,            # Solid circle shape
    color = "darkgrey"        # Use a contrasting color for visibility
  ) +
  geom_hline(
    # Use the manually calculated y-intercept value
    yintercept = MANUAL_Y_INTERCEPT,
    linetype = "dashed",  
    color = "darkred",  
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  geom_text(
    # --- Define the data for the annotation inline ---
    data = data.frame(
      site_id = factor("XCS26"), 
      label_text = "DWAF Guideline Threshold\n(185 cfu/100 ml)",
      x_coord = 2.5, 
      y_coord = MANUAL_Y_INTERCEPT + 1.2 # Y position (above the line)
    ),
    aes(
      x = x_coord,
      y = y_coord,
      label = label_text
    ),
    color = "darkred",
    size = 2,
    hjust = 1,  
    vjust = 1,
    inherit.aes = FALSE # Ensures it uses the coordinates defined above, not the main data
  ) +
  # Use facet_wrap to create a separate plot for each site_id
  facet_wrap(~ site_id, scales = "free_x", ncol = 1) + 
  scale_y_continuous(
    # **CRITICAL FIX: REMOVE trans = "log10"**
    # Use the manually LOGGED breaks
    breaks = LOGGED_BREAKS, 
    # Use the clean labels (ND, 1, 10, etc.)
    labels = FINAL_LABELS_DISPLAY,
    # Adjust limits to the LOGGED range
    limits = log10(c(0.1, max(coastal_data$numeric_value, na.rm = TRUE) + 0.1)),
    # Set the axis name correctly
    name = "log(Enterococci Counts (CFU/100 ml) + 0.1)"
  ) +
  # Manual Fill Colors
  scale_fill_manual(
    values = c(
      "Strand" = "white"     
    )
  ) +
  labs(
    title = "Monthly Distribution of Strand Enterococci Counts \n ( September 2024 - August 2025)",
    x = "Month",
    y = "log(Enterococci Counts (CFU per 100 ml) + 0.1)"
  ) +
  theme_bw() +
  theme(
    # Apply Century Gothic font
    text = element_text(family = "Century Gothic", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", family = "Century Gothic", size = 9),
    legend.position = "none"
  )
p
ggsave(filename = "Monthly_boxplots_10th_90th_perc_strand.png",
       plot = p, width = 170, height = 120, units = "mm", dpi = 600)


#All sites
ALL_SITES <- c("CN11", "CN31", "CN41", "XCS34", "XCS26")

coastal_all <- coastal_data %>%
  mutate(
    sample_date = ymd(sample_date), 
    sample_month_abbr = month(sample_date, label = TRUE, abbr = TRUE),
    sample_year = year(sample_date)
  ) %>%
  arrange(sample_date) %>%
  mutate(
    # Create the Month-Year factor for the X-axis
    sample_month_year = factor(
      paste(sample_month_abbr, sample_year),
      levels = unique(paste(sample_month_abbr, sample_year))
    )
  )

p_combined_sites_log <- ggplot(data = coastal_all, aes(x = sample_month_year, y = log_numeric_value, fill = site_id)
) +
  stat_summary(
    fun.data = percentile_boxplot_stats,
    geom = "boxplot",
    colour = "black",
    size = 0.3,
    position = position_dodge(width = 0.8)
  ) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
    size = 1,              # Adjust size as needed
    alpha = 0.7,           # Use transparency to avoid overplotting
    shape = 16,            # Solid circle shape
    color = "darkgrey"        # Use a contrasting color for visibility
  ) +
  geom_hline(
    # Use the manually calculated y-intercept value
    yintercept = MANUAL_Y_INTERCEPT,
    linetype = "dashed",  
    color = "darkred",  
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = 2.5, 
    # Position the text relative to the manually logged intercept
    y = MANUAL_Y_INTERCEPT + 0.5, 
    label = "DWAF Guideline Threshold\n (185 cfu/100 ml)",
    color = "darkred",
    size = 2.5,
    hjust = 1,  
    vjust = 1
  ) +
  scale_y_continuous(
    # **CRITICAL FIX: REMOVE trans = "log10"**
    # Use the manually LOGGED breaks
    breaks = LOGGED_BREAKS, 
    # Use the clean labels (ND, 1, 10, etc.)
    labels = FINAL_LABELS_DISPLAY,
    # Adjust limits to the LOGGED range
    limits = log10(c(0.1, max(coastal_data$numeric_value, na.rm = TRUE) + 0.1)),
    # Set the axis name correctly
    name = "log(Enterococci Counts (CFU/100 ml) + 0.1)"
  ) +
  # Manual Fill Colors
  scale_fill_manual(
    values = c(
      "CN11" = "pink", 
      "CN31" = "blue",
      "CN41" = "lightgreen", 
      "XCS34" = "white",
      "XCS26" = "darkgreen" 
    ),
    name = "Site ID"
  ) +
  
  labs(
    title = "Monthly Distribution of Enterococci Counts (All Sites)",
    x = "Month"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 7),
    axis.title.y.right = element_text(color = "darkred", face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7), 
    axis.title.y.left = element_text(face = "bold", size = 8),
    axis.text.y.left = element_text(size = 7), 
    axis.text.y.right = element_text(size = 7),
    axis.title = element_text(face = "bold", size = 8),
    text = element_text(family = "Century Gothic", size = 9),
    strip.text = element_text(face = "bold", family = "Century Gothic", size = 9),
    
  )

print(p_combined_sites_log)
ggsave(filename = "Monthly_boxplots_All_Sites_Log_10th_90th_perc.png", plot = p_combined_sites_log, width = 250, height = 120, units = "mm", dpi = 600)

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
p <- ggplot(data = coastal_cb_seasonal, aes(x = season, y = log_numeric_value, fill = site_group)
) +
  stat_summary(
    fun.data = percentile_boxplot_stats,
    geom = "boxplot",
    colour = "black",
    size = 0.3,
    position = position_dodge(width = 0.8)
  ) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
    size = 1,              # Adjust size as needed
    alpha = 0.7,           # Use transparency to avoid overplotting
    shape = 16,            # Solid circle shape
    color = "darkgrey"        # Use a contrasting color for visibility
  ) +
  geom_hline(
    # Use the manually calculated y-intercept value
    yintercept = MANUAL_Y_INTERCEPT,
    linetype = "dashed",  
    color = "darkred",  
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  geom_text(
    data = data.frame(
      site_id = factor("CN11"), 
      label_text = "DWAF Guideline Threshold\n(185 cfu/100 ml)",
      x_coord = 2.5, 
      y_coord = MANUAL_Y_INTERCEPT + 0.5 # Y position (above the line)
    ),
    aes(
      x = x_coord,
      y = y_coord,
      label = label_text
    ),
    color = "darkred",
    size = 2.5,
    hjust = 1,  
    vjust = 1,
    inherit.aes = FALSE # Ensures it uses the coordinates defined above, not the main data
  ) +
  # Use facet_wrap to create a separate plot for each site_id
  facet_wrap(~ site_id, scales = "free_x", ncol = 3)+
    scale_y_continuous(
      # **CRITICAL FIX: REMOVE trans = "log10"**
      # Use the manually LOGGED breaks
      breaks = LOGGED_BREAKS, 
      # Use the clean labels (ND, 1, 10, etc.)
      labels = FINAL_LABELS_DISPLAY,
      # Adjust limits to the LOGGED range
      limits = log10(c(0.1, max(coastal_data$numeric_value, na.rm = TRUE) + 0.1)),
      # Set the axis name correctly
      name = "log(Enterococci Counts (CFU/100 ml) + 0.1)"
    ) +
  # Manual Fill Colors
  scale_fill_manual(
    values = c(
      "Camps Bay" = "grey50"     
    )
  ) +
  labs(
    title = "Seasonal Distribution of Camps Bay Enterococci Counts \n (June 2024 - August 2025)",
    x = "Season",
    y = "log(Enterococci Counts (CFU per 100 ml) + 0.1)", 
  ) +
  theme_bw() +
  theme(
    # Apply Century Gothic font
    text = element_text(family = "Century Gothic", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", family = "Century Gothic", size = 9),
    legend.position = "none"
  )
p
ggsave(filename = "Seasonal_boxplots_CB_10th_90th_perc.png",
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
p <- ggplot(data = coastal_strand_seasonal, aes(x = season, y = log_numeric_value, fill = site_group)
) +
  stat_summary(
    fun.data = percentile_boxplot_stats,
    geom = "boxplot",
    colour = "black",
    size = 0.3,
    position = position_dodge(width = 0.8)
  ) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
    size = 1,              # Adjust size as needed
    alpha = 0.7,           # Use transparency to avoid overplotting
    shape = 16,            # Solid circle shape
    color = "darkgrey"        # Use a contrasting color for visibility
  ) +
  geom_hline(
    # Use the manually calculated y-intercept value
    yintercept = MANUAL_Y_INTERCEPT,
    linetype = "dashed",  
    color = "darkred",  
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  geom_text(
    data = data.frame(
      site_id = factor("XCS26"), 
      label_text = "DWAF Guideline Threshold\n(185 cfu/100 ml)",
      x_coord = 2.5, 
      y_coord = MANUAL_Y_INTERCEPT + 0.5 # Y position (above the line)
    ),
    aes(
      x = x_coord,
      y = y_coord,
      label = label_text
    ),
    color = "darkred",
    size = 2.5,
    hjust = 1,  
    vjust = 1,
    inherit.aes = FALSE # Ensures it uses the coordinates defined above, not the main data
  ) +
  # Use facet_wrap to create a separate plot for each site_id
  facet_wrap(~ site_id, scales = "free_y", ncol = 3) + 
  scale_y_continuous(
    # **CRITICAL FIX: REMOVE trans = "log10"**
    # Use the manually LOGGED breaks
    breaks = LOGGED_BREAKS, 
    # Use the clean labels (ND, 1, 10, etc.)
    labels = FINAL_LABELS_DISPLAY,
    # Adjust limits to the LOGGED range
    limits = log10(c(0.1, max(coastal_data$numeric_value, na.rm = TRUE) + 0.1)),
    # Set the axis name correctly
    name = "log(Enterococci Counts (CFU/100 ml) + 0.1)"
  ) +
  # Manual Fill Colors
  scale_fill_manual(
    values = c(
      "Strand" = "white"     
    )
  ) +
  labs(
    title = "Seasonal Distribution of Strand Enterococci Counts \n (September 2024 - August 2025)",
    x = "Season",
    y = "log(Enterococci Counts (CFU per 100 ml) + 0.1)", 
  ) +
  theme_bw() +
  theme(
    # Apply Century Gothic font
    text = element_text(family = "Century Gothic", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", family = "Century Gothic", size = 9),
    legend.position = "none"
  )
p
ggsave(filename = "Seasonal_boxplots_strand_10th_90th_perc.png",
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
  # 💥 FIX: Simplify column renaming to just remove the superfluous "_HourlySum" part
  rename_with(~ sub("_HourlySum", "", .x), .cols = contains("_HourlySum_6HourlySum"))

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
TARGET_SITE_ID <- "CN41"

# The data is being filtered HERE.
cb_high_plotting <- cb_high_prepared %>%
  # CRITICAL FIX: Filter for the target site ID
  filter(site_id == TARGET_SITE_ID) %>% 
  
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
#Not used
log_plus_01_trans <- scales::trans_new(
  name = "log10_plus_01",
  transform = function(x) log10(x + 0.1),
  inverse = function(x) 10^x - 0.1
)

# --- Primary (Bacteria) Axis Parameters ---
ACTUAL_MAX_BACT <- max(cb_high_plotting$numeric_value, na.rm = TRUE)
BACT_MAX_RAW <- 10^ceiling(log10(ACTUAL_MAX_BACT + 0.1))
BACT_MAX_RAW <- max(BACT_MAX_RAW, 10) 

BACT_MAX_PLOT_HEIGHT <- log10(BACT_MAX_RAW + 0.1) 
Y_MIN_LOG <- log10(0 + 0.1) # -1.0

# --- DYNAMIC RAINFALL MAX ---
RAIN_MAX_TRUE <- max(rainfall_to_plot$Rainfall_Avg, na.rm = TRUE)
RAIN_MAX_LIN <- RAIN_MAX_TRUE * 1.05 
RAIN_MIN_LIN <- 0 

# --- Calculate Factor Z ---
Z_PLOT_RANGE <- BACT_MAX_PLOT_HEIGHT - Y_MIN_LOG
RAIN_RANGE <- RAIN_MAX_LIN - RAIN_MIN_LIN

z <- Z_PLOT_RANGE / RAIN_RANGE
if (is.infinite(z) || is.nan(z)) z <- 1 

# --- Dynamic Breaks and Labels for Primary Axis ---
final_breaks_log_coords <- seq(0, BACT_MAX_PLOT_HEIGHT, by = 1) 
final_labels_raw <- 10^final_breaks_log_coords


#### 4. PLOTTING - LOGGING (USING GEOM_RECT) ####

# Calculate the half-width of the 6-hour bar in seconds
BAR_HALF_WIDTH_SEC <- 3 * 3600 

single_site_plot <- ggplot() +
  
  # Layer 1: 6-hourly Rainfall (Bars) - Now uses geom_rect
  geom_rect(
    data = rainfall_to_plot %>% filter(Rainfall_Avg > 0),
    aes(
      #  FIX: Define the time window (xmin/xmax) for the 6-hour bar
      xmin = Six_Hourly_Timestamp,
      xmax = Six_Hourly_Timestamp + seconds(2*BAR_HALF_WIDTH_SEC),
      
      # ymin is the BASE (ND/0)
      ymin = Y_MIN_LOG,
      # ymax is the TOP of the scaled bar
      y = Rainfall_Avg * z + Y_MIN_LOG
    ),
    fill = "lightgrey",
    color = NA,
    alpha = 0.7
  ) +
  
  # Layer 2: Hourly Bacteria Data (Scatterplot)
  geom_point(
    data = cb_high_plotting,
    # Manually log the Y-AESTHETIC
    aes(x = date_time, y = log10(numeric_value + 0.1), color = tag), 
    size = 0.5,
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
    breaks = c(Y_MIN_LOG, final_breaks_log_coords), 
    labels = c("ND", scales::label_comma()(final_labels_raw)), 
    name = paste0("log(Enterococci Counts (CFU/100 ml) + 0.1)"),
    limits = c(Y_MIN_LOG, BACT_MAX_PLOT_HEIGHT), 
    
    # Secondary Y-Axis (Rainfall - LINEAR SCALE)
    sec.axis = sec_axis(
      trans = ~ (. - Y_MIN_LOG) / z, 
      breaks = unique(c(0, pretty(c(RAIN_MIN_LIN, RAIN_MAX_LIN), n = 5))),
      name = "6-Hourly Rainfall Avg (mm)"
    )
  ) +
  
  # --- X-Axis and Theme ---
  scale_x_datetime(
    labels = date_format("%b %d\n%H:%M"),
    # Set the break sequence to align with 00:00, 06:00, etc.
    breaks = seq(
      from = as.POSIXct("2025-05-19 00:00:00", tz = "UTC"), 
      to = max(cb_high_plotting$date_time, na.rm = TRUE), # Go up to the max date
      by = "6 hours"
    )
  ) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Replicates") +
  theme_bw() +
  labs(
    title = paste("Enterococci vs. Rainfall for Site:", TARGET_SITE_ID),
    x = "Date and Time"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6),
    axis.title.y.right = element_text(color = "darkred", face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
    axis.title.y.left = element_text(face = "bold", size = 8),
    axis.text.y.left = element_text(size = 6), 
    axis.text.y.right = element_text(size = 6),
    axis.title.x = element_text(face = "bold", size = 8)
  )

print(single_site_plot)
filename <- paste0("CB_HF_FIB_6Hourly_Log_Rainfall_", TARGET_SITE_ID, ".png")

ggsave(
  filename = filename, 
  plot = single_site_plot, 
  width = 170, 
  height = 100, # Increased height to account for vertical facet strips
  units = "mm", 
  dpi = 600
)


# Plotting separately
TARGET_SITE_ID <- "CN11"

# The data is being filtered HERE.
cb_high_plotting <- cb_high_prepared %>%
  # CRITICAL FIX: Filter for the target site ID
  filter(site_id == TARGET_SITE_ID) %>% 
  
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

BAR_HALF_WIDTH_SEC <- 3 * 3600 # 3 hours in seconds 

replicate_colors <- c("A" = "red", "B" = "blue", "C" = "darkgreen", 
                      "Blank" = "gray", "single" = "orange")

# --- Dynamic Axes Calculation (Adjust these if your data changes) ---
ACTUAL_MAX_BACT <- max(cb_high_plotting$numeric_value, na.rm = TRUE)
BACT_MAX_RAW <- 10^ceiling(log10(ACTUAL_MAX_BACT + 0.1))
BACT_MAX_RAW <- max(BACT_MAX_RAW, 10) 
BACT_MAX_PLOT_HEIGHT <- log10(BACT_MAX_RAW + 0.1) 
Y_MIN_LOG <- log10(0 + 0.1) 

final_breaks_log_coords <- seq(0, BACT_MAX_PLOT_HEIGHT, by = 1)
final_labels_raw <- 10^final_breaks_log_coords

RAIN_MAX_TRUE <- max(rainfall_to_plot$Rainfall_Avg, na.rm = TRUE)
RAIN_MAX_PLOT <- RAIN_MAX_TRUE * 1.05

# --- X-Axis Break Filtering ---
# Defines the start/end of the two weeks with a gap in between.
# 1. Define the start/end points for the breaks we want to see
WEEK1_START <- as.POSIXct("2025-05-19 00:00:00", tz = "UTC")
WEEK1_END <- as.POSIXct("2025-05-24 00:00:00", tz = "UTC") 
WEEK2_START <- as.POSIXct("2025-05-26 00:00:00", tz = "UTC")
WEEK2_END <- as.POSIXct("2025-05-31 00:00:00", tz = "UTC")

# 2. Generate ALL 6-hourly breaks
all_x_breaks_full <- seq(
  from = min(WEEK1_START, WEEK2_START),
  to = max(WEEK1_END, WEEK2_END),
  by = "6 hours"
)

# 3. Filter out the breaks for the missing days (May 24 & May 25)
master_x_breaks_filtered <- all_x_breaks_full[
  (all_x_breaks_full < WEEK1_END) | 
    (all_x_breaks_full >= WEEK2_START)
]


# =========================================================================
# 1. PLOT 1: ENTEROCOCCI (LOG SCALE, STANDALONE)
# =========================================================================

p_entero_separate <- ggplot(data = cb_high_plotting, 
                            aes(x = date_time, y = log10(numeric_value+0.1), color = tag)) +
  
  geom_point(size = 0.8, alpha = 0.9) +
  
  # Faceting by week with free X-scales
  facet_wrap(
    ~ week,
    scales = "free_x", 
    ncol = 1
  ) +
  
  # Y-Axis (Logarithmic Scale)
  scale_y_continuous(
    breaks = c(Y_MIN_LOG, final_breaks_log_coords), 
    labels = c("ND", scales::label_comma()(final_labels_raw)), 
    name = "log(Enterococci Counts (CFU/100 ml) + 0.1)",
    limits = c(Y_MIN_LOG, BACT_MAX_PLOT_HEIGHT)
  ) +
  
  # X-Axis: Use filtered 6-hourly breaks and limits
  scale_x_datetime(
    breaks = master_x_breaks_filtered,
    labels = date_format("%b %d\n%H:%M")
  ) +
  
  # Theme and Colors
  scale_color_manual(values = replicate_colors, name = "Replicates") + 
  theme_bw() +
  labs(title = paste("Hourly Enterococci for Site:", TARGET_SITE_ID), 
       x = "Date and Time") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.right = element_text(color = "darkred", face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
    axis.title.y.left = element_text(face = "bold", size = 8),
    axis.text.y.left = element_text(size = 6), 
    axis.text.y.right = element_text(size = 6),
    axis.title.x = element_text(face = "bold", size = 8)
  )

print(p_entero_separate)
ggsave(
  filename = paste0("CB_HF_Log_FIB_", TARGET_SITE_ID, ".png"),
  plot = p_entero_separate,
  width = 170,
  height = 90,
  units = "mm",
  dpi = 600
)

# =========================================================================
# 2. PLOT 2: RAINFALL (LINEAR SCALE, STANDALONE)
# =========================================================================

p_rain_separate <- ggplot(data = rainfall_to_plot %>% filter(Rainfall_Avg >= 0), 
                          aes(x = Six_Hourly_Timestamp, y = Rainfall_Avg)) +
  
  geom_col(
    width = 6 * 3600, 
    fill = "lightgrey",
    color = "darkgrey",
    alpha = 0.7,
    position = position_nudge(x = 0) 
  ) +
  
  # Faceting: Still by week with free X-scales
  facet_wrap(
    ~ week,
    scales = "free_x", 
    ncol = 1
  ) +
  
  # Y-Axis (Linear Scale)
  scale_y_continuous(
    breaks = pretty(c(0, RAIN_MAX_TRUE), n = 5),
    name = "6-Hourly Rainfall Avg (mm)",
    limits = c(0, RAIN_MAX_PLOT)
  ) +
  
  # X-Axis: Use filtered 6-hourly breaks and limits
  scale_x_datetime(
    breaks = master_x_breaks_filtered,
    labels = date_format("%b %d\n%H:%M")
  ) +
  
  # Theme
  theme_bw() +
  labs(title = paste("6-Hourly Rainfall for Site:", TARGET_SITE_ID), 
       x = "Date and Time", y = "6-Hourly Rainfall Avg (mm)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title.y.right = element_text(color = "darkred", face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
    axis.title.y.left = element_text(face = "bold", size = 8),
    axis.text.y.left = element_text(size = 6), 
    axis.text.y.right = element_text(size = 6),
    axis.title.x = element_text(face = "bold", size = 8)
  )

print(p_rain_separate)
ggsave(
  filename = paste0("CB_HF_rain_", TARGET_SITE_ID, ".png"),
  plot = p_rain_separate,
  width = 170,
  height = 90,
  units = "mm",
  dpi = 600
)



####Linear FIB Camps Bay High Frequency ####
TARGET_SITE_ID <- "CN41"
CAP_VALUE <- 1000 # Define the linear cap value
Y_MIN_LIN <- 0    # Define the linear minimum
BAR_HALF_WIDTH_SEC <- 3 * 3600 # 3 hours in seconds

# --- 2. Data Preparation ---

cb_high_plotting <- cb_high_prepared %>%
  # CRITICAL FIX: Filter for the target site ID
  filter(site_id == TARGET_SITE_ID) %>% 
  
  mutate(
    Date_Only = as_date(date_time), 
    week = case_when(
      Date_Only >= as_date("2025-05-19") & Date_Only <= as_date("2025-05-23") ~ "Week 1",
      Date_Only >= as_date("2025-05-26") & Date_Only <= as_date("2025-05-30") ~ "Week 2",
      TRUE ~ NA_character_
    ),
    # 💥 NEW: Create the CAPPED LINEAR value
    capped_value = pmin(numeric_value, CAP_VALUE)
  ) %>%
  filter(!is.na(week)) %>%
  filter(numeric_value >= 0, !is.na(numeric_value))


# --- 3. DYNAMIC SCALING FACTOR (Z) CALCULATION (LINEAR BACTERIA RANGE) ---

# 💥 CHANGE: Linear Bacteria Max Plot Height is simply the cap value
ACTUAL_MAX_BACT <- max(cb_high_plotting$capped_value, na.rm = TRUE)
BACT_MAX_PLOT_HEIGHT <- CAP_VALUE 

# --- DYNAMIC RAINFALL MAX (Unchanged, remains linear) ---
RAIN_MAX_LIN <- max(rainfall_to_plot$Rainfall_Avg, na.rm = TRUE)
RAIN_MAX_LIN <- RAIN_MAX_LIN 
RAIN_MIN_LIN <- 0 

# --- Calculate Factor Z ---
# 💥 CHANGE: Z is calculated using the LINEAR Y-range
Z_PLOT_RANGE <- BACT_MAX_PLOT_HEIGHT - Y_MIN_LIN # e.g., 1000 - 0 = 1000
RAIN_RANGE <- RAIN_MAX_LIN - RAIN_MIN_LIN

z <- Z_PLOT_RANGE / RAIN_RANGE
if (is.infinite(z) || is.nan(z)) z <- 1 

# --- Dynamic Breaks and Labels for Primary Axis (LINEAR) ---
# 💥 CHANGE: Use pretty() for linear breaks
primary_breaks <- pretty(c(Y_MIN_LIN, BACT_MAX_PLOT_HEIGHT), n = 5)

# Y-Axis Label function for capping
cap_labels <- function(x) {
  ifelse(x == CAP_VALUE, paste0(">", CAP_VALUE-1, "+"), scales::label_comma()(x))
}


### 4. PLOTTING - LINEAR (USING GEOM_RECT) ###

single_site_plot <- ggplot() +
  
  # Layer 1: 6-hourly Rainfall (Bars)
  geom_rect(
    data = rainfall_to_plot %>% filter(Rainfall_Avg > 0),
    aes(
      xmin = Six_Hourly_Timestamp,
      xmax = Six_Hourly_Timestamp + seconds(2*BAR_HALF_WIDTH_SEC),
      
      # ymin is the base (ND/0)
      ymin = Y_MIN_LIN, 
      # YMAX IS THE TOP OF THE SCALED BAR
      y = Rainfall_Avg * z + Y_MIN_LIN
    ),
    fill = "lightgrey",
    color = NA,
    alpha = 0.7
  ) +
  
  # Layer 2: Hourly Bacteria Data (Scatterplot)
  geom_point(
    data = cb_high_plotting,
    # 💥 CHANGE: Use the LINEAR 'capped_value'
    aes(x = date_time, y = capped_value, color = tag), 
    size = 0.5,
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
    # 💥 CHANGE: Use linear breaks and the custom cap_labels function
    breaks = primary_breaks, 
    labels = cap_labels,
    name = paste0("Enterococci Counts (CFU/100 ml)"),
    limits = c(Y_MIN_LIN, BACT_MAX_PLOT_HEIGHT), 
    
    # Secondary Y-Axis (Rainfall - LINEAR SCALE)
    sec.axis = sec_axis(
      # 💥 CHANGE: The trans function now uses Y_MIN_LIN (0)
      trans = ~ (. - Y_MIN_LIN) / z, 
      breaks = unique(c(0, pretty(c(RAIN_MIN_LIN, RAIN_MAX_LIN), n = 5))),
      name = "6-Hourly Rainfall Avg (mm)"
    )
  ) +
  
  # --- X-Axis and Theme ---
  scale_x_datetime(
    labels = date_format("%b %d\n%H:%M"),
    # Set the break sequence to align with 00:00, 06:00, etc.
    breaks = seq(
      from = as.POSIXct("2025-05-19 00:00:00", tz = "UTC"), 
      to = max(cb_high_plotting$date_time, na.rm = TRUE), # Go up to the max date
      by = "6 hours"
    )
  ) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Replicates") +
  theme_bw() +
  labs(
    title = paste("Hourly Enterococci vs. Rainfall for Site:", TARGET_SITE_ID),
    x = "Date and Time"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6),
    axis.title.y.right = element_text(color = "darkred", face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
    axis.title.y.left = element_text(face = "bold", size = 8),
    axis.text.y.left = element_text(size = 6), 
    axis.text.y.right = element_text(size = 6),
    axis.title.x = element_text(face = "bold", size = 8)
  )

print(single_site_plot)
filename <- paste0("CB_HF_FIB_6Hourly_Linear_Rainfall_", TARGET_SITE_ID, ".png")

ggsave(
  filename = filename, 
  plot = single_site_plot, 
  width = 170, 
  height = 100, 
  units = "mm", 
  dpi = 600
)
 
 
 
 
 



###################### Strand FIB & rainfall plots############################################################
coastal_strand <- coastal_data %>%
  filter(
    site_id %in% c("XCS34", "XCS26") 
  )

####Log FIB Strand####
# Define the rainfall column name
RAIN_COL_NAME <- "LOUR06BRS_WeeklySum"

# --- 1. Data Preparation ---

# Bacteria Data: Add log_value and factor for faceting
bacteria_to_plot <- coastal_strand %>%
  mutate(
    # Create a factor for faceting using your site_id column
    site_id_fct = factor(site_id),
    # Manually log the Y-value with the +0.1 offset
    log_value = log10(numeric_value + 0.1)
  )

# Rainfall Data: Prepare weekly data for plotting, keeping the original column name
rainfall_to_plot <- weekly_rainfall %>%
  select(Week_Start, all_of(RAIN_COL_NAME)) %>%
  rename(Rainfall_Value = all_of(RAIN_COL_NAME)) 

# --- 2. DYNAMIC SCALING FACTOR (Z) CALCULATION ---

# Calculate plot height range (based on ALL bacteria data)
ACTUAL_MAX_BACT <- max(bacteria_to_plot$numeric_value, na.rm = TRUE)
BACT_MAX_RAW <- 10^ceiling(log10(ACTUAL_MAX_BACT + 0.1))
BACT_MAX_RAW <- max(BACT_MAX_RAW, 10) 
BACT_MAX_PLOT_HEIGHT <- log10(BACT_MAX_RAW + 0.1)
Y_MIN_LOG <- log10(0 + 0.1) # -1.0 (The plot coordinate for ND/0 mm)

# Calculate rainfall range
RAIN_MAX_LIN <- max(rainfall_to_plot[["Rainfall_Value"]], na.rm = TRUE)
RAIN_MAX_LIN <- RAIN_MAX_LIN * 1.05 
RAIN_MIN_LIN <- 0

# Calculate Factor Z (Harmonizes the visual height of the two axes)
Z_PLOT_RANGE <- BACT_MAX_PLOT_HEIGHT - Y_MIN_LOG
RAIN_RANGE <- RAIN_MAX_LIN - RAIN_MIN_LIN
z <- Z_PLOT_RANGE / RAIN_RANGE
if (is.infinite(z) || is.nan(z)) z <- 1

# Breaks for Primary Axis
final_breaks_log_coords <- seq(0, BACT_MAX_PLOT_HEIGHT, by = 1)
final_labels_raw <- 10^final_breaks_log_coords

# --- 3. PLOTTING (Rainfall is ANCHORED geom_rect) ---

# Column width for weekly data. Width is expressed in DAYS.
bar_width <- 7 

# --- Manual Site Colors ---
site_colors <- c(
  "XCS34" = "purple", 
  "XCS26" = "darkgreen" 
)

plot_single_site <- function(site, bacteria_data, rainfall_data, z_factor, y_min, y_max, rain_max, log_breaks, log_labels, bar_w) {
  
  # Filter data for the current site
  site_data <- bacteria_data %>% filter(site_id == site)
  
  single_plot <- ggplot() +
    
    # Layer 1: Weekly Rainfall (Bars)
    geom_rect(
      data = rainfall_data %>% filter(Rainfall_Value > 0), 
      aes(
        xmin = Week_Start - (bar_w / 2),
        xmax = Week_Start + (bar_w / 2),
        ymin = y_min,
        ymax = Rainfall_Value * z_factor + y_min
      ),
      fill = "lightgrey",
      color = NA,
      alpha = 0.7
    ) +
    
    # Layer 2: Daily Bacteria Data (Scatterplot)
    geom_point(
      data = site_data,
      aes(x = sample_date, y = log_value),
      color = site_colors[site], 
      size = 0.8,
      alpha = 0.9
    ) +
    
    # --- Y-Axis with sec_axis and Factor Z ---
    scale_y_continuous(
      breaks = c(y_min, log_breaks), 
      labels = c("ND", scales::label_comma()(log_labels)), 
      name = "log(Enterococci Counts (CFU/100 ml) + 0.1)",
      limits = c(y_min, y_max), 
      
      sec.axis = sec_axis(
        trans = ~ (. - y_min) / z_factor, 
        breaks = unique(c(0, pretty(c(RAIN_MIN_LIN, rain_max), n = 5))),
        name = paste("Weekly Rainfall Total (mm) - LOUR06BRS")
      )
    ) +
    
    # --- X-Axis and Theme ---
    scale_x_date(
      labels = date_format("%d %b %y"),
      breaks = "2 weeks"
    ) +
    theme_bw() +
    labs(
      title = paste("Site", site, ": Daily Strand Enterococci vs. Weekly Rainfall"),
      x = "Date"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      axis.title.y.right = element_text(color = "darkred", face = "bold", size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
      axis.title.y.left = element_text(face = "bold", size = 8),
      axis.text.y.left = element_text(size = 6), 
      axis.text.y.right = element_text(size = 6),
      axis.title.x = element_text(face = "bold", size = 8)
    )
  
  return(single_plot)
}

site_ids <- unique(coastal_strand$site_id) 

plot_list <- lapply(site_ids, function(id) {
  plot_single_site(
    site = id, bacteria_data = bacteria_to_plot, rainfall_data = rainfall_to_plot,
    z_factor = z, y_min = Y_MIN_LOG, y_max = BACT_MAX_PLOT_HEIGHT,
    rain_max = RAIN_MAX_LIN, log_breaks = final_breaks_log_coords, 
    log_labels = final_labels_raw, bar_w = bar_width
  )
})
names(plot_list) <- site_ids

# To view all plots sequentially:
invisible(lapply(plot_list, print))

# Saving loop
for (site_id in names(plot_list)) {
  plot <- plot_list[[site_id]]
  filename <- paste0("Strand_FIB_Log_Rainfall_", site_id, ".png")
  ggsave(filename, plot, width = 170, height = 90, units = "mm", dpi = 600)
  message(paste("Saved plot:", filename))
}

####Linear FIB ####

# --- 1. Define Constants & Guidelines (LINEAR) ---
CAP_VALUE <- 1000      # Max display value
Y_MIN_LIN <- 0         # Minimum display value
bar_width <- 7         
GUIDE_BLUE_FLAG <- 100
GUIDE_DWAF <- 185
RAIN_COL_NAME <- "LOUR06BRS_WeeklySum" # Strand's rainfall column

# --- 2. Data Preparation ---

# Bacteria Data: Use the linear CAPPED value
bacteria_to_plot <- coastal_strand %>%
  mutate(
    site_id_fct = factor(site_id),
    # CENSORING STEP: Use pmin() for capping
    capped_value = pmin(numeric_value, CAP_VALUE)
  )

# Rainfall Data: Prepare weekly data
rainfall_to_plot <- weekly_rainfall %>%
  select(Week_Start, all_of(RAIN_COL_NAME)) %>%
  rename(Rainfall_Value = all_of(RAIN_COL_NAME))

# --- 3. DYNAMIC SCALING FACTOR (Z) CALCULATION (LINEAR) ---

ACTUAL_MAX_BACT <- max(bacteria_to_plot$capped_value, na.rm = TRUE)
BACT_MAX_PLOT_HEIGHT <- CAP_VALUE 

RAIN_MAX_LIN <- max(rainfall_to_plot$Rainfall_Value, na.rm = TRUE)
RAIN_MAX_LIN <- RAIN_MAX_LIN * 1.05 
RAIN_MIN_LIN <- 0

# Calculate Factor Z based on linear ranges
Z_PLOT_RANGE <- BACT_MAX_PLOT_HEIGHT - Y_MIN_LIN 
RAIN_RANGE <- RAIN_MAX_LIN - RAIN_MIN_LIN
z <- Z_PLOT_RANGE / RAIN_RANGE
if (is.infinite(z) || is.nan(z)) z <- 1

primary_breaks <- pretty(c(Y_MIN_LIN, BACT_MAX_PLOT_HEIGHT), n = 5)

# --- 4. PLOTTING FUNCTION (LINEAR/CAPPED) ---

plot_single_site <- function(site, bacteria_data, rainfall_data, z_factor, y_min, y_max, rain_max, primary_breaks, bar_w) {
  
  site_data <- bacteria_data %>% filter(site_id == site)
  
  # Y-Axis Label function for capping
  cap_labels <- function(x) {
    ifelse(x == CAP_VALUE, paste0(">", CAP_VALUE-1, "+"), scales::label_comma()(x))
  }
  
  # Create dummy data for horizontal guidelines (LINEAR values)
  x_min_plot <- min(bacteria_data$sample_date, na.rm=TRUE)
  x_max_plot <- max(bacteria_data$sample_date, na.rm=TRUE)
  
  guideline_data_long <- tibble(
    y_value = c(GUIDE_BLUE_FLAG, GUIDE_BLUE_FLAG, GUIDE_DWAF, GUIDE_DWAF),
    x_value = c(x_min_plot, x_max_plot, x_min_plot, x_max_plot),
    line_label = factor(c("Blue Flag (100)", "Blue Flag (100)", 
                          "DWAF (185)", "DWAF (185)")) 
  )
  
  # Define custom colors for the sites (Manual setting)
  site_colors <- c("XCS34" = "purple", "XCS26" = "darkgreen")
  
  single_plot <- ggplot() +
    
    # Layer 1: Weekly Rainfall (Bars)
    geom_rect(
      data = rainfall_data %>% filter(Rainfall_Value > 0), 
      aes(
        xmin = Week_Start - (bar_w / 2),
        xmax = Week_Start + (bar_w / 2),
        ymin = y_min, 
        # Rainfall value is now linear (not log)
        ymax = (Rainfall_Value * z_factor) + y_min
      ),
      fill = "lightgrey", color = NA, alpha = 0.7
    ) +
    
    # Guideline Lines 
    geom_line(
      data = guideline_data_long,
      aes(
        x = x_value, y = y_value,
        color = line_label,   
        group = line_label    
      ),
      linetype = "dashed", 
      linewidth = 0.5,
      show.legend = TRUE 
    ) +
    
    # Layer 2: Daily Bacteria Data (Scatterplot)
    geom_point(
      data = site_data,
      # Y-aesthetic uses the linear capped value
      aes(x = sample_date, y = capped_value), 
      # Manual color assignment suppresses the legend for points
      color = site_colors[site], 
      size = 0.8, alpha = 0.9, position = position_jitter(width = 0.5, height = 0)
    ) +
    
    # --- Y-Axis with sec_axis (LINEAR) ---
    scale_y_continuous(
      breaks = primary_breaks, 
      labels = cap_labels, # Uses the custom capped label function
      name = "Enterococci Counts (CFU/100 ml)",
      limits = c(y_min, y_max), 
      
      sec.axis = sec_axis(
        trans = ~ (. - y_min) / z_factor, 
        breaks = unique(c(0, pretty(c(RAIN_MIN_LIN, rain_max), n = 5))),
        name = paste("Weekly Rainfall Total (mm)- LOUR06BRS")
      )
    ) +
    
    # Custom Scales for Legend Appearance (GUIDELINES ONLY)
    scale_color_manual(
      values = c("Blue Flag (100)" = "blue", "DWAF (185)" = "red"),
      name = "Guideline" 
    ) +
    
    # --- X-Axis and Theme ---
    scale_x_date(labels = date_format("%d %b %y"), breaks = "2 weeks") +
    theme_bw() +
    labs(
      title = paste("Site", site, ": Daily Strand Enterococci vs. Weekly Rainfall"),
      x = "Date"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 7, face = "bold"),
      legend.text = element_text(size = 6),
      axis.title.y.right = element_text(color = "darkred", face = "bold", size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
      axis.title.y.left = element_text(face = "bold", size = 8),
      axis.text.y.left = element_text(size = 6), 
      axis.text.y.right = element_text(size = 6),
      axis.title.x = element_text(face = "bold", size = 8)
    ) +
    # Guides: Override to ensure lines are shown for the 2 legend items
    guides(color = guide_legend(override.aes = list(
      shape = NA, 
      linetype = 2, 
      linewidth = 1 
    ))) 
  
  return(single_plot)
}

# --- 5. Generate and Save Plots ---

site_ids <- unique(bacteria_to_plot$site_id) # XCS34, XCS26
plot_list <- lapply(site_ids, function(id) {
  plot_single_site(
    site = id, bacteria_data = bacteria_to_plot, rainfall_data = rainfall_to_plot,
    z_factor = z, y_min = Y_MIN_LIN, y_max = BACT_MAX_PLOT_HEIGHT,
    rain_max = RAIN_MAX_LIN, primary_breaks = primary_breaks, bar_w = bar_width
  )
})
names(plot_list) <- site_ids

# To view plots:
invisible(lapply(plot_list, print))

# Saving loop
for (site_id in names(plot_list)) {
  plot <- plot_list[[site_id]]
  filename <- paste0("Strand_FIB_Linear_Rainfall_", site_id, ".png")
  ggsave(filename, plot, width = 170, height = 90, units = "mm", dpi = 600)
  message(paste("Saved plot:", filename))
}








#########################Camps Bay daily & rainfall plots###########################################

coastal_cb <- coastal_data %>%
  filter(
    site_id %in% c("CN11", "CN31", "CN41") 
  ) 

####Log FIB Camps Bay####
bacteria_to_plot <- coastal_cb %>%
  mutate(
    # Create a factor for faceting
    site_id_fct = factor(site_id),
    # Manually log the Y-value with the +0.1 offset
    log_value = log10(numeric_value + 0.1)
  )

# 1.B. Rainfall Data: Calculate the average of CITY and DIEP stations (Weekly)
RAIN_COL_AVG <- "AVG_CITY_DIEP_WeeklySum"

rainfall_to_plot <- weekly_rainfall %>%
  # 💥 REVISED CALCULATION using specific column names (WeeklySum assumed)
  mutate(
    "{RAIN_COL_AVG}" := (CITY11BR_WeeklySum + DIEP05ER_WeeklySum) / 2
  ) %>%
  select(Week_Start, all_of(RAIN_COL_AVG)) %>%
  rename(Rainfall_Avg = all_of(RAIN_COL_AVG)) 

# --- 2. DYNAMIC SCALING FACTOR (Z) CALCULATION ---

# Calculate plot height range
ACTUAL_MAX_BACT <- max(bacteria_to_plot$numeric_value, na.rm = TRUE)
BACT_MAX_RAW <- 10^ceiling(log10(ACTUAL_MAX_BACT + 0.1))
BACT_MAX_RAW <- max(BACT_MAX_RAW, 10) 
BACT_MAX_PLOT_HEIGHT <- log10(BACT_MAX_RAW + 0.1)
Y_MIN_LOG <- log10(0 + 0.1) # -1.0

# Calculate rainfall range
RAIN_MAX_LIN <- max(rainfall_to_plot$Rainfall_Avg, na.rm = TRUE)
RAIN_MAX_LIN <- RAIN_MAX_LIN * 1.05 
RAIN_MIN_LIN <- 0

# Calculate Factor Z 
Z_PLOT_RANGE <- BACT_MAX_PLOT_HEIGHT - Y_MIN_LOG
RAIN_RANGE <- RAIN_MAX_LIN - RAIN_MIN_LIN
z <- Z_PLOT_RANGE / RAIN_RANGE
if (is.infinite(z) || is.nan(z)) z <- 1

# Breaks for Primary Axis
final_breaks_log_coords <- seq(0, BACT_MAX_PLOT_HEIGHT, by = 1)
final_labels_raw <- 10^final_breaks_log_coords

# --- 3. PLOTTING (Rainfall is UNANCHORED geom_col) ---

# Column width for weekly data (DAYS)
bar_width <- 7 
site_colors <- c(
  "CN11" = "red", 
  "CN31" = "blue", 
  "CN41" = "green4"
)

plot_single_site <- function(site, bacteria_data, rainfall_data, z_factor, y_min, y_max, rain_max, log_breaks, log_labels, bar_w) {
  
  # Filter data for the current site
  site_data <- bacteria_data %>% filter(site_id == site)
  
  single_plot <- ggplot() +
    
    # Layer 1: Weekly Rainfall (Bars)
    geom_rect(
      data = rainfall_data %>% filter(Rainfall_Avg > 0), 
      aes(
        xmin = Week_Start - (bar_w / 2),
        xmax = Week_Start + (bar_w / 2),
        ymax = Rainfall_Avg * z_factor + y_min,
        ymin = y_min 
      ),
      fill = "grey",
      color = NA,
      alpha = 0.7
    ) +
    
    # Layer 2: Daily Bacteria Data (Scatterplot)
    geom_point(
      data = site_data,
      aes(x = sample_date, y = log_value),
      color = site_colors[site], 
      size = 0.8, 
      alpha = 0.9
    ) +
    
    # --- Y-Axis with sec_axis and Factor Z ---
    scale_y_continuous(
      breaks = c(y_min, log_breaks), 
      labels = c("ND", scales::label_comma()(log_labels)), 
      name = "log(Enterococci Counts (CFU/100 ml) + 0.1)",
      limits = c(y_min, y_max), 
      
      sec.axis = sec_axis(
        trans = ~ (. - y_min) / z_factor, 
        breaks = unique(c(0, pretty(c(RAIN_MIN_LIN, rain_max), n = 5))),
        name = "Avg. Weekly Rainfall (mm) - CITY & DIEP"
      )
    ) +
    
    # --- X-Axis and Theme ---
    scale_x_date(
      labels = date_format("%b %d"),
      breaks = "2 weeks"
    ) +
    theme_bw() +
    labs(
      title = paste("Site", site, ": Daily Camps Bay Enterococci vs. Avg. Weekly Rainfall"),
      x = "Date"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      axis.title.y.right = element_text(color = "darkred", face = "bold", size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
      axis.title.y.left = element_text(face = "bold", size = 8),
      axis.text.y.left = element_text(size = 6), 
      axis.text.y.right = element_text(size = 6),
      axis.title.x = element_text(face = "bold", size = 8)
    )
  
  return(single_plot)
}

# --- 3. Generate and Save Plots ---

# 💥 CHANGE 3: Only three site IDs will be found
site_ids <- unique(coastal_cb$site_id) 

plot_list <- lapply(site_ids, function(id) {
  plot_single_site(
    site = id, bacteria_data = bacteria_to_plot, rainfall_data = rainfall_to_plot,
    z_factor = z, y_min = Y_MIN_LOG, y_max = BACT_MAX_PLOT_HEIGHT,
    rain_max = RAIN_MAX_LIN, log_breaks = final_breaks_log_coords, 
    log_labels = final_labels_raw, bar_w = bar_width
  )
})
names(plot_list) <- site_ids

# To view all plots sequentially:
invisible(lapply(plot_list, print))

# Saving loop
for (site_id in names(plot_list)) {
  plot <- plot_list[[site_id]]
  filename <- paste0("CB_FIB_Log_Rainfall_", site_id, ".png")
  ggsave(filename, plot, width = 170, height = 90, units = "mm", dpi = 600)
  message(paste("Saved plot:", filename))
}

####Linear FIB Camps Bay####

# --- 1. Define Constants & Guidelines ---
CAP_VALUE <- 1000
Y_MIN_LIN <- 0 
bar_width <- 7 
GUIDE_BLUE_FLAG <- 100
GUIDE_DWAF <- 185
RAIN_COL_AVG <- "AVG_CITY_DIEP_WeeklySum"

# --- 2. Data Preparation ---

bacteria_to_plot <- coastal_cb %>%
  mutate(
    site_id_fct = factor(site_id),
    # CENSORING STEP
    capped_value = pmin(numeric_value, CAP_VALUE)
  )

rainfall_to_plot <- weekly_rainfall %>%
  mutate(
    "{RAIN_COL_AVG}" := (CITY11BR_WeeklySum + DIEP05ER_WeeklySum) / 2
  ) %>%
  select(Week_Start, all_of(RAIN_COL_AVG)) %>%
  rename(Rainfall_Avg = all_of(RAIN_COL_AVG)) 

# --- 3. DYNAMIC SCALING FACTOR (Z) CALCULATION ---

ACTUAL_MAX_BACT <- max(bacteria_to_plot$capped_value, na.rm = TRUE)
BACT_MAX_PLOT_HEIGHT <- CAP_VALUE 

RAIN_MAX_LIN <- max(rainfall_to_plot$Rainfall_Avg, na.rm = TRUE)
RAIN_MAX_LIN <- RAIN_MAX_LIN * 1.05 
RAIN_MIN_LIN <- 0

Z_PLOT_RANGE <- BACT_MAX_PLOT_HEIGHT - Y_MIN_LIN 
RAIN_RANGE <- RAIN_MAX_LIN - RAIN_MIN_LIN
z <- Z_PLOT_RANGE / RAIN_RANGE
if (is.infinite(z) || is.nan(z)) z <- 1

primary_breaks <- pretty(c(Y_MIN_LIN, BACT_MAX_PLOT_HEIGHT), n = 5)


# --- 5. PLOTTING FUNCTION  ---

plot_single_site <- function(site, bacteria_data, rainfall_data, z_factor, y_min, y_max, rain_max, primary_breaks, bar_w) {
  
  site_data <- bacteria_data %>% filter(site_id == site)
  
  # CORRECTED: Create dummy data spanning the X-axis for geom_line
  x_min_plot <- min(bacteria_data$sample_date, na.rm=TRUE)
  x_max_plot <- max(bacteria_data$sample_date, na.rm=TRUE)
  
  guideline_data_long <- tibble(
    y_value = c(GUIDE_BLUE_FLAG, GUIDE_BLUE_FLAG, GUIDE_DWAF, GUIDE_DWAF),
    x_value = c(x_min_plot, x_max_plot, x_min_plot, x_max_plot),
    # Use a factor for the line label and grouping
    line_label = factor(c("Blue Flag Guideline (100)", "Blue Flag Guideline (100)", 
                          "DWAF Guideline (185)", "DWAF Guideline (185)")) 
  )
  
  cap_labels <- function(x) {
    ifelse(x == CAP_VALUE, paste0(">", CAP_VALUE-1, "+"), scales::label_comma()(x))
  }
  
  # Define custom colors for the sites
  site_colors <- c("CN11" = "red", "CN31" = "blue", "CN41" = "green4")
  
  single_plot <- ggplot() +
    
    # Layer 1: Weekly Rainfall (Bars)
    geom_rect(
      data = rainfall_data %>% filter(Rainfall_Avg > 0), 
      aes(
        xmin = Week_Start - (bar_w / 2),
        xmax = Week_Start + (bar_w / 2),
        ymin = y_min, 
        ymax = (Rainfall_Avg * z_factor) + y_min
      ),
      fill = "grey", color = NA, alpha = 0.7
    ) +
    
    # Guideline Lines: Color is mapped to 'line_label' to create the legend
    geom_line(
      data = guideline_data_long,
      aes(
        x = x_value, y = y_value,
        color = line_label,   
        group = line_label    
      ),
      linetype = "dashed", 
      linewidth = 0.5,
      show.legend = TRUE 
    ) +
    
    # Layer 2: Daily Bacteria Data (Scatterplot)
    geom_point(
      data = site_data,
      aes(x = sample_date, y = capped_value), 
      color = site_colors[site], 
      size = 0.8, alpha = 0.9, position = position_jitter(width = 0.5, height = 0)
    ) +
    
    # --- Y-Axis with sec_axis ---
    scale_y_continuous(
      breaks = primary_breaks, 
      labels = cap_labels,
      name = "Enterococci Count (CFU/100 ml)",
      limits = c(y_min, y_max), 
      
      sec.axis = sec_axis(
        trans = ~ (. - y_min) / z_factor, 
        breaks = unique(c(0, pretty(c(RAIN_MIN_LIN, rain_max), n = 5))),
        name = "Avg. Weekly Rainfall (mm) - CITY & DIEP"
      )
    ) +
    
    # 💥 CHANGE 2: Simplify scale_color_manual for guidelines ONLY
    scale_color_manual(
      values = c("Blue Flag Guideline (100)" = "blue", "DWAF Guideline (185)" = "red"),
      breaks = c("Blue Flag Guideline (100)", "DWAF Guideline (185)"),
      name = "Guideline" 
    ) +
    
    # --- X-Axis and Theme ---
    scale_x_date(labels = date_format("%d %b %y"), breaks = "2 weeks") +
    theme_bw() +
    labs(
      title = paste("Site", site, ": Daily Enterococci vs. Avg. Weekly Rainfall"),
      x = "Date"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_text(size = 7, face = "bold"),
      legend.text = element_text(size = 6),
      axis.title.y.right = element_text(color = "darkred", face = "bold", size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6), 
      axis.title.y.left = element_text(face = "bold", size = 8),
      axis.text.y.left = element_text(size = 6), 
      axis.text.y.right = element_text(size = 6),
      axis.title.x = element_text(face = "bold", size = 8)
    ) +
    guides(color = guide_legend(override.aes = list(
      shape = NA, # No shape/point
      linetype = 2, # Dashed line
      linewidth = 1 # Ensure line is visible
    ))) 
  
  return(single_plot)
}

# --- 6. Generate and View/Save Plots ---

site_ids <- unique(bacteria_to_plot$site_id)
plot_list <- lapply(site_ids, function(id) {
  plot_single_site(
    site = id, bacteria_data = bacteria_to_plot, rainfall_data = rainfall_to_plot,
    z_factor = z, y_min = Y_MIN_LIN, y_max = BACT_MAX_PLOT_HEIGHT,
    rain_max = RAIN_MAX_LIN, primary_breaks = primary_breaks, bar_w = bar_width
  )
})
names(plot_list) <- site_ids

# To view all three plots sequentially:
invisible(lapply(plot_list, print))

for (site_id in names(plot_list)) {
     plot <- plot_list[[site_id]]
     filename <- paste0("CB_FIB_Linear_rainfall", site_id, ".png")
     ggsave(filename, plot, width = 170, height = 90, units = "mm", dpi = 600)
     message(paste("Saved plot:", filename))
   }




















