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
       lubridate) #to work with, parse, wrangle, etc. dates

##Pull in human coding spreadsheet
human_coding_spreadsheet <- read_csv("C:/AOS_db/data/12_coding_spreadsheet_unique_id.csv")

##Filter to articles with a human coded attack on science
aoses_coded <- human_coding_spreadsheet %>%
  filter(`AOS PRESENCE` == 1)

#new dataset: pull in each row of data from last three days
aoses_citations <- aoses_coded %>%
  select(agg_objectid, `FULL DATE`, HEADLINE, LINK, `ARTICLE SOURCE`, `AOS PRESENCE`)

#create new data set
write_csv(aoses_citations, "C:/AOS_db/data/13_aos_citations.csv")
