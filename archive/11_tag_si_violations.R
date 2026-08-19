##This script identifies if there is a possible scientific integrity violation described in the attack, using the types of attacks on science identified in the full article review.

library(tidyverse)

df0 <- read_csv("C:/AOS_db/data/08_coding_spreadsheet_unique_id.csv")


df1 <- df0 %>% 
  mutate(potential_SI_violation = case_when(
    `Altering Study Results` == 1 ~ "yes", 
    `Data Accessibility` == 1 ~ "yes",
    `Data Collection` == 1 ~ "yes",
    `Censorship` == 1 ~ "yes",
    `Restrictions from Professional Engagement` == 1 ~ "yes",
    TRUE ~ "no"
  ))

write_csv(df1, "C:/AOS_db/data/11_coding_spreadsheet_unique_id_si.csv")
