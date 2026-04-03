#### R SCRIPT PURPOSE: 
#### Saves citations for each coded AOS
####Runs weekly

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 

p_load(tidyverse, 
       lubridate) 

human_coding_spreadsheet <- read_csv("C:/AOS_db/data/12_coding_spreadsheet_unique_id.csv")

aoses_coded <- human_coding_spreadsheet %>%
  filter(`AOS PRESENCE` == 1)

aoses_citations <- aoses_coded %>%
  select(agg_objectid, `FULL DATE`, HEADLINE, LINK, `ARTICLE SOURCE`, `AOS PRESENCE`)

write_csv(aoses_citations, "C:/AOS_db/data/13_aos_citations.csv")
