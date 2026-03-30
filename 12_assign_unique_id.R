
#### R SCRIPT PURPOSE: 
#### Saves citations for each coded AOS
#### Runs (?) How often will this script run?

##Filtering needs
#1. Filter out articles that are coded 0, not an attack on science.

### Logistics for R Script ###

#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data
       lubridate,
       readxl) #to work with, parse, wrangle, etc. dates

##Pull in human coding spreadsheet
human_coding_spreadsheet <- read_excel("C:/AOS_db/data/11_coding_spreadsheet.xlsx")

##Filter to articles with a human coded attack on science
aoses_coded <- human_coding_spreadsheet %>%
  filter(`AOS PRESENCE` == 1,
         `FULL DATE` > "2025-12-19") %>%
  arrange(`FULL DATE`) %>%
  mutate(agg_objectid = 537 + 1:nrow(.),
         agg_objectid = paste0("AOS", str_pad(as.character(agg_objectid), width = 5, side = "left", pad = "0")))

write_csv(aoses_coded, "C:/AOS_db/data/12_coding_spreadsheet_unique_id.csv")
