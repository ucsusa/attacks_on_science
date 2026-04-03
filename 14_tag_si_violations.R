#### R SCRIPT PURPOSE: 
####Identifies if there is a possible scientific integrity violation described in the attack, using the types of attacks on science identified in the full article review.
####Runs weekly

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse)

human_coding_spreadsheet <- read_csv("C:/AOS_db/data/12_coding_spreadsheet_unique_id.csv")


human_coding_spreadsheet_si <- human_coding_spreadsheet %>% 
  mutate(potential_SI_violation = case_when(
    `Altering Study Results` == 1 ~ "yes", 
    `Data Accessibility` == 1 ~ "yes",
    `Data Collection` == 1 ~ "yes",
    `Censorship` == 1 ~ "yes",
    `Restrictions from Professional Engagement` == 1 ~ "yes",
    TRUE ~ "no"
  ))

write_csv(human_coding_spreadsheet_si, "C:/AOS_db/data/14_coding_spreadsheet_unique_id_si.csv")
