#### R SCRIPT PURPOSE: 
#### Saves meta data of each article that contain an attack on science, confirmed by human coders.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 

p_load(tidyverse, 
       lubridate) 

human_coding_spreadsheet <- read_csv("../data/13_coding_spreadsheet_unique_id.csv")

aoses_coded <- human_coding_spreadsheet %>%
  filter(aos_presence == 1)

aoses_citations <- aoses_coded %>%
  select(agg_objectid, full_date, headline, link, article_source)

##Add in citations from fully human coded data
human_coding <- read_csv("../updating_formatting_human_coding_results/results/12_attack_title_summary.csv")  %>%
  select(agg_objectid, full_date, headline, link, article_source)


aoses_citations <- bind_rows(aoses_citations, human_coding)
          
write_csv(aoses_citations, "../data/14_aos_citations.csv")