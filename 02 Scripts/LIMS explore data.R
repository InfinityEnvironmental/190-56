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

