#### R SCRIPT PURPOSE: 
####Identifies if there is a possible scientific integrity violation described in the attack, using the types of attacks on science identified in the full article review.
####Runs weekly

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse)

human_coding_spreadsheet <- read_csv("C:/AOS_db/data/13_coding_spreadsheet_unique_id.csv")


human_coding_spreadsheet_si <- human_coding_spreadsheet %>% 
  mutate(potential_SI_violation = case_when(
    altering_study_results == 1 ~ "yes", 
    data_accessibility == 1 ~ "yes",
    data_collection == 1 ~ "yes",
    censorship == 1 ~ "yes",
    restrictions_from_professional_engagement == 1 ~ "yes",
    TRUE ~ "no"
  ))

write_csv(human_coding_spreadsheet_si, "C:/AOS_db/data/15_coding_spreadsheet_unique_id_si.csv")
