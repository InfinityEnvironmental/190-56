# Data Import and Checking of Historical FIB Coastal Data
# 28 March 2025
# Francois van Schalkwyk

# Workspace Set-Up ----

# Load packages
library(tidyverse)
library(readxl)

# Data Import ----

# Create list of files
path <- "01 Input Data/Coastal Water Quality Data/"
filename <- list.files(path, pattern = "*.xls*")
full_path <- paste(path, filename, sep = "")

# Have a look at the data
full_path |>
  read_excel(
    sheet = 2, 
    col_names = c("date", "ecoli_cfu_per_100ml"), 
    col_types = c("date", "numeric")
           ) |>
  filter(!if_all(date:ecoli_cfu_per_100ml, is.na)) |>
  summarise(min_date = min(date), max_date = max(date), min_ecoli = min(ecoli_cfu_per_100ml), max_ecoli = max(ecoli_cfu_per_100ml), mean_ecoli = mean(ecoli_cfu_per_100ml), n_ecoli = sum(!is.na(ecoli_cfu_per_100ml)))
