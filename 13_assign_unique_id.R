#### R SCRIPT PURPOSE: 
#### Assigns unique identifiers to each human-coded attack on science.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 

p_load(tidyverse, 
       lubridate,
       readxl) 

human_coding_spreadsheet <- read_csv("C:/AOS_db/data/12_aoses_clean_no_multiples.csv")

#Pull in only articles that are coded as an attack on science and after 12/19/2025
#(Attacks on science were fully human coded and collected prior to that date)

aoses_coded <- human_coding_spreadsheet %>%
  filter(aos_presence == 1,
         full_date > "2025-12-19") %>%
  arrange(full_date) %>%
  mutate(agg_objectid = 537 + 1:nrow(.),
         agg_objectid = paste0("AOS", str_pad(as.character(agg_objectid), width = 5, side = "left", pad = "0")))

write_csv(aoses_coded, "C:/AOS_db/data/13_coding_spreadsheet_unique_id.csv")