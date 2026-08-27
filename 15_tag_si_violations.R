#### R SCRIPT PURPOSE: 
#### Identifies potential scientific integrity violations based on attacks on science types identified by human coders.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse)

human_coding_spreadsheet <- read_csv("../data/13_coding_spreadsheet_unique_id.csv")


human_coding_spreadsheet_si <- human_coding_spreadsheet %>% 
  mutate(potential_SI_violation = case_when(
    altering_study_results == 1 ~ "yes", 
    data_accessibility == 1 ~ "yes",
    data_collection == 1 ~ "yes",
    censorship == 1 ~ "yes",
    restrictions_from_professional_engagement == 1 ~ "yes",
    TRUE ~ "no"
  ))

write_csv(human_coding_spreadsheet_si, "../data/15_coding_spreadsheet_unique_id_si.csv")