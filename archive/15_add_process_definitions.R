##This script appends the appropriate definitions from the process document to the end of the url text for inclusion in generic summaries.

library(tidyverse)

##Read in attacks on science
grouped_aoses <- read_csv("C:/AOS_db/updating_formatting_human_coding_results/results/09_aoses_grouped.csv")

##Read in definitions from process document
aos_definitions <- read_csv("C:/AOS_db/updating_formatting_human_coding_results/data/999_aos_type_topic_definitions.csv")

type_aos_definitions <- aos_definitions %>%
  filter(type_topic == "Type")

topic_aos_definitions <- aos_definitions %>%
  filter(type_topic == "Topic")

##Pivot to long format for table join
grouped_aoses <- grouped_aoses %>%
  separate_longer_delim(., cols = attack_type_variable, delim = "_") %>%
  separate_longer_delim(., cols = attack_topic_variable, delim = "_") %>%
  mutate(attack_topic_variable = gsub("Public ", "", attack_topic_variable))

##Join the tables for type descriptions
grouped_aoses_defs_type <- left_join(grouped_aoses, type_aos_definitions, by = c("attack_type_variable" = "specific_category_name")) %>%
  mutate(url_text = paste(specific_category_description, url_text, "")) %>%
  select(-c(type_topic, category_name, category_description, specific_category_description))

##Join the tables for topic descriptions
grouped_aoses_defs_type_topic <- left_join(grouped_aoses_defs_type, topic_aos_definitions, by = c("attack_topic_variable" = "specific_category_name")) %>%
  mutate(url_text = paste(specific_category_description, url_text, "")) %>%
  select(-c(type_topic, category_name, category_description, specific_category_description))

write_csv(grouped_aoses_defs_type_topic, "C:/AOS_db/updating_formatting_human_coding_results/results/10_aoses_process_defs.csv")
