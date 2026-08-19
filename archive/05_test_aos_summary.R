#install.packages("LSAfun")
library(LSAfun)
library(openxlsx)
library(tidyverse)

aos_db <- read.xlsx("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/UCS Attacks on Science - Update 12-20-23.xlsx")

aos_db_concatenate <- aos_db %>%
  filter(Admin == "Trump") %>%
  select(`Short.Description`) %>%
  unique() %>%
  group_by() %>%
  summarize(all_summaries = paste0(Short.Description, collapse = ". ")) %>%
  ungroup()

all_summaries_words <- aos_db_concatenate$all_summaries

summary <- genericSummary(all_summaries_words,k=5)

