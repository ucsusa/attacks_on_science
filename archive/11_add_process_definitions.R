##This script appends the appropriate definitions from the process document to the end of the url text for inclusion in generic summaries.

library(tidyverse)

##Read in attacks on science
human_coding_spreadsheet <- read_csv("C:/AOS_db/data/11_coding_spreadsheet_unique_id_si.csv")

##Read in definitions from process document
aos_definitions <- read_csv("C:/AOS_db/info_tables/999_aos_type_topic_definitions.csv")

type_aos_definitions <- aos_definitions %>%
  filter(type_topic == "Type")

topic_aos_definitions <- aos_definitions %>%
  filter(type_topic == "Topic")

stage_aos_definitions <- aos_definitions %>%
  filter(type_topic == "Stage")

##Pivot to long format for table join
grouped_aoses <- human_coding_spreadsheet %>%
  pivot_longer(., cols = `Agency Appointments`:`Targeting Scientists Based on Identity`, names_to = "type", values_to = "type_response") %>%
  pivot_longer(., cols = `Climate Science`:`Any`, names_to = "topic", values_to = "topic_response") %>%
  pivot_longer(., cols = Completed:Threatened, names_to = "enacted", values_to = "enacted_value") %>%
  filter(type_response == 1|topic_response != "0"|enacted_value == 1)

##Join the tables for type descriptions
grouped_aoses_defs_type <- left_join(grouped_aoses, type_aos_definitions, by = c("type" = "specific_category_name"))

grouped_aoses_defs_type <- grouped_aoses_defs_type %>%
  rowwise() %>%
  mutate(attack_summary_type = specific_category_summary_description) %>%
  select(-c(type_topic, category_name, category_description, specific_category_description, specific_category_summary_description)) %>%
  unique()

##Join the tables for topic descriptions
grouped_aoses_defs_type_topic <- left_join(grouped_aoses_defs_type, topic_aos_definitions, by = c("topic" = "specific_category_name"))

grouped_aoses_defs_type_topic <- grouped_aoses_defs_type_topic %>%
  mutate(attack_summary_topic = specific_category_summary_description) %>%
  select(-c(type_topic, category_name, category_description, specific_category_description, specific_category_summary_description)) %>%
  unique()

##Join the tables for stage descriptions
grouped_aoses_defs_type_topic_stage <- left_join(grouped_aoses_defs_type_topic, stage_aos_definitions, by = c("enacted" = "specific_category_name"))

grouped_aoses_defs_type_topic_stage <- grouped_aoses_defs_type_topic_stage %>%
  rowwise() %>%
  mutate(attack_summary_completion = specific_category_summary_description) %>%
  select(-c(type_topic, category_name, category_description, specific_category_description, specific_category_summary_description)) %>%
  unique()

write_csv(grouped_aoses_defs_type_topic_stage, "C:/AOS_db/data/11_aoses_summarized_process_defs.csv")
