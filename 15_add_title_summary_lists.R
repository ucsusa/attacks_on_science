#### R SCRIPT PURPOSE: 
####This script appends the appropriate definitions from the process document to the end of the url text for inclusion in generic summaries.
####Runs weekly

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse,
       janitor) 

human_coding_spreadsheet <- read_csv("C:/AOS_db/data/14_coding_spreadsheet_unique_id_si.csv")

###Read in type, topic, and enacted descriptions
process_descriptions <- read_csv("C:/AOS_db/info_tables/999_aos_type_topic_definitions.csv") %>%
  mutate(specific_category_name = gsub(" ", "_", tolower(specific_category_name)),
         specific_category_name = gsub("_&", "", tolower(specific_category_name)),
         specific_category_name = gsub("\\/", "_", tolower(specific_category_name))) %>%
  select(specific_category_name, specific_category_summary_description)


aoses_long <- human_coding_spreadsheet %>%
  clean_names %>%
  pivot_longer(., cols = `agency_appointments`:`targeting_scientists_based_on_identity`, names_to = "attack_type", values_to = "type_response") %>%
  pivot_longer(., cols = `climate_science`:`any`, names_to = "attack_topic", values_to = "topic_response") %>%
  pivot_longer(., cols = completed:threatened, names_to = "enacted", values_to = "enacted_value") %>%
  filter(type_response == 1|topic_response != "0"|enacted_value == 1)


make_a_list <- function(x){
  if (length(unique(x)) == 0) {
    formatted_string <- ""
  } else if (length(unique(x)) == 1) {
    formatted_string <- x[1]
  } else if (length(unique(x)) == 2) {
    formatted_string <- paste(x, collapse = " and ")
  } else {
    first_part <- paste(x[-length(unique(x))], collapse = ", ")
    formatted_string <- paste0(first_part, ", and ", x[length(unique(x))])
  }
}

make_first_letter_lowercase <- function(x) {
  first_letter <- tolower(substr(x, 1, 1))
  rest_of_string <- substr(x, 2, nchar(x))
  return(paste0(first_letter, rest_of_string, collapse = " "))
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}


df_type <- aoses_long %>%
  filter(type_response == 1) %>%
  select(agg_objectid, headline, full_date, link, article_source, attack_type) %>%
  unique() %>%
  left_join(., process_descriptions, by = c("attack_type" = "specific_category_name"))

df_type <- df_type %>%
  group_by(headline, full_date, link, article_source, agg_objectid) %>%
  summarise(attack_type_list = map(attack_type, ~make_a_list(unique(attack_type))),
            summary_type_list = map(specific_category_summary_description, ~make_a_list(unique(specific_category_summary_description)))) %>%
  ungroup() %>%
  unique()


df_topic <- aoses_long %>%
  filter(topic_response == 1) %>%
  select(agg_objectid, headline, full_date, link, article_source, attack_topic) %>%
  unique() %>%
  left_join(., process_descriptions, by = c("attack_topic" = "specific_category_name"))

df_topic <- df_topic %>%
  group_by(headline, full_date, link, article_source, agg_objectid) %>%
  summarise(attack_topic_list = map(attack_topic, ~make_a_list(unique(attack_topic))),
            summary_topic_list = map(specific_category_summary_description, ~make_a_list(unique(specific_category_summary_description)))) %>%
  ungroup() %>%
  unique()
         

df_enacted <- aoses_long %>%
  filter(enacted_value == 1) %>%
  select(agg_objectid, headline, full_date, link, article_source, enacted) %>%
  unique() %>%
  left_join(., process_descriptions, by = c("enacted" = "specific_category_name"))

df_enacted <- df_enacted %>%
  group_by(headline, full_date, link, article_source, agg_objectid) %>%
  summarise(enacted_list = map(enacted, ~make_a_list(unique(enacted))),
            summary_enacted_list = map(specific_category_summary_description, ~make_a_list(unique(specific_category_summary_description)))) %>%
  ungroup() %>%
  unique()


aos_type_topic <- left_join(df_type, df_topic)

aos_type_topic <- left_join(aos_type_topic, df_enacted)

aoses_agencies <- aoses_long %>%
  select(agg_objectid, agencies_involved) %>%
  unique() %>%
  separate_longer_delim(., agencies_involved, delim = ", ")


aoses_agencies_lists <- aoses_agencies %>%
  group_by(agg_objectid) %>%
  summarise(agencies_involved_list = map(agencies_involved, ~make_a_list(unique(agencies_involved)))) %>%
  ungroup() %>%
  unique() %>%
  mutate(attack_attacks = ifelse(str_count(agencies_involved_list, ",") > 1, 'attack', 'attacks'))

aos_type_topic <- left_join(aos_type_topic, aoses_agencies_lists)

aos_data_df <- aoses_long %>%
  select(headline, full_date, link, article_source, article_description, si_mention, gss_mention, agencies_involved, coders, aos_presence, agg_objectid, potential_si_violation) %>%
  mutate(full_date = as.character(full_date)) %>%
  unique()

aos_type_topic <- aos_type_topic %>%
  mutate(across(everything(), as.character)) %>%
  left_join(., aos_data_df) %>%
  unique()

##We are not using the titles in the final dataset as they were too choppy.
aos_summaries <- aos_type_topic %>%
  mutate(attack_title = paste(agencies_involved_list, attack_attacks, "science with", attack_type_list, "negatively impacting", attack_topic_list),
         article_description = paste0(firstup(article_description), "."),
         attack_title = gsub("_", " ", attack_title),
         attack_title = gsub("health safety", "health & safety", attack_title),
         attack_summary = paste0("This attack includes ", summary_type_list, ". This reduces or undermines federal scientific capacity or knowledge ", summary_topic_list, ". This attack was ", summary_enacted_list, ".")) %>%
  ungroup()
  

write.csv(aos_summaries, "C:/AOS_db/data/15_aos_titles_summaries.csv", row.names = FALSE)
