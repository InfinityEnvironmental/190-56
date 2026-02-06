# Load libraries
library(dplyr)
library(ggplot2)

# Load data
day_rain <- read.csv("C:/Users/DikelediMothiba/Desktop/daily_rainfall_data.csv")
hour_rain <- read.csv("C:/Users/DikelediMothiba/Desktop/hourly_rainfall_data.csv")
coast_data <- read.csv("C:/Users/DikelediMothiba/Desktop/filtered_coastal_data.csv")
high_frequency <- read.csv("C:/Users/DikelediMothiba/Desktop/cb_high_freq_data.csv")

# Filter for strand data
strand <- coast_data |> 
  filter(site_id %in% 
           c("XCS26", "XCS34"))

# Convert character Date to object 
strand$sample_date <- as.Date(strand$sample_date)
day_rain$Date_Only <- as.Date(day_rain$Date_Only)
hour_rain$Hourly_Timestamp <- as.Date(hour_rain$Hourly_Timestamp)

# Log transform since largest count is close to 20 000
#strand_log <- strand |> 
 # mutate(log_val = log10(numeric_value + 1)) # Add 1 because log(0) is undefined

# Plot entero
ggplot(strand, aes(x = sample_date, y = numeric_value + 1)) + # +1 added to keep 0 and not calculate infinity
  geom_jitter(aes(color = site_id), alpha = 0.6, width = 0.2) +
  geom_hline(yintercept = 240, linetype = "dashed", color = "red", linewidth = 0.3) +
  scale_y_log10(breaks = c(1, 10, 100, 240, 1000, 10000), 
                labels = c("1", "10", "100", "240", "1k", "10k")) +  
  facet_wrap(~site_id) +
  theme_minimal() +
  labs(title ="Strand beaches over 12mths (log scale)",
       y = "Enterococci CFU/100 ml",
       x = "Date")

# Plot rainfall 
ggplot(day_rain, aes(x = Date_Only, y = LOUR06BRS_DailySum)) +
  # Use fill for the inside color and color for the outline
  geom_col(fill = "blue", alpha = 0.6) +
  #scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme_minimal() +
  labs(title = "Daily Rainfall Over Year",
       x = "Date",
       y = "Rainfall (mm)")

# Merge by date
strand_rain <- left_join(strand, day_rain, by = c("sample_date" = "Date_Only"))

# Visualise plots together --- correlation
ggplot(strand_rain, aes(x = LOUR06BRS_DailySum, y = numeric_value + 1)) +
  geom_jitter(aes(color = site_id), alpha = 0.6) +
  # Add a trend line to see the correlation
  geom_smooth(method = "lm", color = "red") + 
  scale_y_log10(breaks = c(1, 10, 100, 240, 1000, 10000)) +
  facet_wrap(~site_id) +
  theme_minimal() +
  labs(title = "Effect of Rainfall on Enterococci Counts",
       x = "Daily Rainfall (mm)",
       y = "Enterococci (Log Scale)")

# Only look at rainy days to see the trend more clearly
strand_rain |> 
  filter(LOUR06BRS_DailySum > 1) |> 
  ggplot(aes(x = LOUR06BRS_DailySum, y = numeric_value + 1)) +
  geom_point(aes(color = site_id)) +
  geom_smooth(method = "lm") +
  scale_y_log10() +
  facet_wrap(~site_id) # shows a positive relationship

# This gives you the correlation coefficient (r)
# Use 'spearman' because bacteria data is usually not a normal distribution
cor.test(strand_rain$LOUR06BRS_DailySum, strand_rain$numeric_value, method = "spearman")

# Timeline layers
ggplot(strand_rain, aes(x = sample_date)) +
      geom_col(aes(y = LOUR06BRS_DailySum * 100), fill = "darkgrey", alpha = 0.5) + # Multiply rain by 100 so it's visible on the log scale
      geom_jitter(aes(y = numeric_value + 1, color = site_id), alpha = 0.6) +  # 2. Add entero as dots
      geom_hline(yintercept = 240, linetype = "dashed", color = "red") +
      scale_y_log10(breaks = c(1, 10, 100, 240, 1000, 10000),
                labels = c("1", "10", "100", "240", "1k", "10k")) +
      #scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
    facet_wrap(~site_id) +
    theme_minimal() +
    labs(title = "Bacteria Spikes vs. Rainfall Events",
       subtitle = "Grey bars = Rainfall | Colored dots = Enterococci",
       x = "Date",
       y = "Enterococci CFU/100ml (Log Scale)")

# Test for lag effect
# 1. Create the lag variable (shifted by 1 day)
strand_rain <- strand_rain |>
  group_by(site_id) |> # Important: Lag separately for each beach
  arrange(sample_date) |> 
  mutate(rain_yesterday = lag(LOUR06BRS_DailySum, 1)) |>
  ungroup()

# 2. Run the Spearman test on YESTERDAY'S rain
cor.test(strand_rain$rain_yesterday, strand_rain$numeric_value, method = "spearman")
