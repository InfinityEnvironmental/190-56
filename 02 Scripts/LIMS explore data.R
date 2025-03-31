#LIMS Historical Data
#Exploration of data
#Author: Robyn Daniels

#Load libraries
library(tidyverse)
library(dbplyr)
library(DBI)
library(pdftools)
library(readxl)
library(stringr)
library(tidyr)

#Set working directory
setwd("C:/Users/RobynDaniels/Repositories/190-56/01 Input Data/Coastal Water Quality Data/LIMS Data")
getwd()

#Load data into R
lims <- read_excel("LIMS Data.xlsm", sheet = "2004-2025 Entero")
view(lims)

#Load site ID list into R
updated_sites <- read_excel("C:/Users/RobynDaniels/Infinity/Coastal Water Quality - Documents/Shared Results/coastal_wq_weekly_sites_updated.xlsx")
view(updated_sites)

#Check if there are any sites in the LIMS data that isn't on our updated site list
joined_data <- lims %>%
  left_join(updated_sites, by = c("SAMPLING_POINT" = "site_id"))
view(joined_data)

missing_sites <- joined_data %>%
  filter(is.na(site_name)) %>% # changed to site_name, as that is the column from updated_sites
  select(SAMPLING_POINT)
view(missing_sites)
# missing_sites now contains the SAMPLING_POINT values from lims that are not in updated_sites

#Create a distinct list with missing sites & no duplicates
missing_site_list <- missing_sites %>%
  distinct(SAMPLING_POINT) %>% # Remove duplicate rows
  pull(SAMPLING_POINT) # Extract the site IDs as a vector

# Print the list of unique missing site IDs
print(missing_site_list)

#################################################################################################

#Create bar graph indicating number of samples for each site (to display sampling effort)

# Group by 'SAMPLING_POINT' and count the number of samples
sample_counts_site <- lims %>%
  group_by(SAMPLING_POINT) %>%
  summarise(SAMPLE_COUNT = n(), .groups = "drop")

# Split the sampling points into two groups (adjust as needed)
unique_sites <- unique(sample_counts_site$SAMPLING_POINT)
mid_point <- ceiling(length(unique_sites) / 2)
sites_group1 <- unique_sites[1:mid_point]
sites_group2 <- unique_sites[(mid_point + 1):length(unique_sites)]

# Filter the data for each group
group1_data <- sample_counts_site %>% filter(SAMPLING_POINT %in% sites_group1)
group2_data <- sample_counts_site %>% filter(SAMPLING_POINT %in% sites_group2)

# Create the two bar plots
plot1 <- ggplot(group1_data, aes(x = SAMPLING_POINT, y = SAMPLE_COUNT, fill = SAMPLING_POINT)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Samples per Sampling Point (Group 1)",
       x = "Sampling Point",
       y = "Number of Samples") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

plot2 <- ggplot(group2_data, aes(x = SAMPLING_POINT, y = SAMPLE_COUNT, fill = SAMPLING_POINT)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Samples per Sampling Point (Group 2)",
       x = "Sampling Point",
       y = "Number of Samples") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Arrange the plots vertically
library(gridExtra)
grid.arrange(plot1, plot2, nrow = 2)

##########################################################################################
#Create summary tables for each site

# Clean the 'RESULT/100ML' column
lims$`RESULT/100ML` <- gsub("[<>]", "", lims$`RESULT/100ML`) # Remove '<' and '>' characters
lims$`RESULT/100ML` <- as.numeric(gsub("[^0-9.]", "", lims$`RESULT/100ML`)) # Remove non-numeric characters and convert to numeric

# Filter out NA values from the 'RESULT/100ML' column
lims_filtered <- lims %>%
  filter(!is.na(`RESULT/100ML`))
view(lims_filtered)

# Group by 'SAMPLING_POINT' and calculate summary statistics
summary_tables <- lims_filtered %>%
  group_by(SAMPLING_POINT) %>%
  summarise(
    Max = max(`RESULT/100ML`, na.rm = TRUE),
    Min = min(`RESULT/100ML`, na.rm = TRUE),
    Average = mean(`RESULT/100ML`, na.rm = TRUE),
    Sample_Count = n()  # Add the sample count
  )

# Print the summary tables
print(summary_tables)
view(summary_tables)

############################################################################################

#Creating time series for Enterococci results for each site (Yearly)

# Convert SAMPLED_DATE to Date object
lims_filtered$SAMPLED_DATE <- as.Date(lims_filtered$SAMPLED_DATE)

# Create individual plots for each site
# Calculate yearly averages
yearly_averages <- lims_filtered %>%
  mutate(Year = format(SAMPLED_DATE, "%Y")) %>% # Extract year
  group_by(SAMPLING_POINT, Year) %>%
  summarise(Yearly_Average = mean(`RESULT/100ML`), .groups = "drop")

# Get unique sites
unique_sites <- unique(yearly_averages$SAMPLING_POINT)

# Group sites into sets of 10-12
site_groups <- split(unique_sites, ceiling(seq_along(unique_sites) / 10))

# Create plots for each group
plot_list <- lapply(site_groups, function(group) {
  group_data <- yearly_averages %>% filter(SAMPLING_POINT %in% group)
  
  ggplot(group_data, aes(x = Year, y = Yearly_Average, fill = SAMPLING_POINT)) +
    geom_bar(stat = "identity") +
    facet_wrap(~SAMPLING_POINT, scales = "free_y", ncol = 2) +
    labs(title = paste("Sites:", paste(group, collapse = ", ")),
         x = "Year",
         y = "Yearly Average Enterococci Concentration (per 100 ml)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))
})

# Display plots
for (p in plot_list) {
  print(p)
}

######################################################################################

#Create a monthly climatology of Enterococci concentration boxplots for each site

# Extract month
seasonal_data <- lims_filtered %>%
  mutate(Month = month(SAMPLED_DATE, label = TRUE))

# Get unique sites
unique_sites <- unique(seasonal_data$SAMPLING_POINT)

# Group sites into sets of 10-12
site_groups <- split(unique_sites, ceiling(seq_along(unique_sites) / 10))

# Create plots for each group
plot_list <- lapply(site_groups, function(group) {
  group_data <- seasonal_data %>% filter(SAMPLING_POINT %in% group)
  
  ggplot(group_data, aes(x = Month, y = `RESULT/100ML`, group = Month, fill = SAMPLING_POINT)) + # Fill by site
    geom_boxplot() +
    facet_wrap(~SAMPLING_POINT, scales = "free_y", ncol = 2) +
    labs(title = paste("Sites:", paste(group, collapse = ", ")),
         x = "Month",
         y = "Enterococci Concentration (Per 100 ml)") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"), # Remove grey background
          panel.grid.major = element_blank(), # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines
          axis.line = element_line(colour = "black")) # Add axis borders
})

# Display plots
for (p in plot_list) {
  print(p)
}

##################################################################################

#Create monthly climatology for Enterococci concentration line graphs for each site (with standard deviation)

# Calculate monthly statistics
monthly_stats <- lims_filtered %>%
  mutate(Month = month(SAMPLED_DATE, label = TRUE)) %>%
  group_by(SAMPLING_POINT, Month) %>%
  summarise(
    Mean = mean(`RESULT/100ML`, na.rm = TRUE),
    StdDev = sd(`RESULT/100ML`, na.rm = TRUE),
    .groups = "drop"
  )

# Get unique sites
unique_sites <- unique(monthly_stats$SAMPLING_POINT)

# Group sites into sets of 10-12
site_groups <- split(unique_sites, ceiling(seq_along(unique_sites) / 10))

# Create plots for each group
plot_list <- lapply(site_groups, function(group) {
  group_data <- monthly_stats %>% filter(SAMPLING_POINT %in% group)
  
  ggplot(group_data, aes(x = Month, y = Mean, color = SAMPLING_POINT, group = SAMPLING_POINT)) +
    geom_line() +
    geom_smooth(aes(ymin = Mean - StdDev, ymax = Mean + StdDev, fill = SAMPLING_POINT), 
                stat = "identity", alpha = 0.2, color = NA) + # Smoothed standard deviation
    facet_wrap(~SAMPLING_POINT, scales = "free_y", ncol = 2) +
    labs(title = paste("Sites:", paste(group, collapse = ", ")),
         x = "Month",
         y = "Enterococci Concentration (Per 100 ml)") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))
})

# Display plots
for (p in plot_list) {
  print(p)
}








