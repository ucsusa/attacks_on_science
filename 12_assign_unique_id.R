#### R SCRIPT PURPOSE: 
#### Assigns unique identifiers to each coded AOS
#### Runs weekly

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 

p_load(tidyverse, 
       lubridate,
       readxl) 

human_coding_spreadsheet <- read_excel("C:/AOS_db/data/11_coding_spreadsheet.xlsx")

##Pull in only articles that are coded as an attack on science and those after 12/19/2025 since attacks on science were fully human coded prior to that date.

aoses_coded <- human_coding_spreadsheet %>%
  filter(`AOS PRESENCE` == 1,
         `FULL DATE` > "2025-12-19") %>%
  arrange(`FULL DATE`) %>%
  mutate(agg_objectid = 537 + 1:nrow(.),
         agg_objectid = paste0("AOS", str_pad(as.character(agg_objectid), width = 5, side = "left", pad = "0")))

write_csv(aoses_coded, "C:/AOS_db/data/12_coding_spreadsheet_unique_id.csv")
