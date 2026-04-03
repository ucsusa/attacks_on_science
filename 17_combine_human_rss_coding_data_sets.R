#### R SCRIPT PURPOSE: 
####This script combines and formats the human coded and the rss feed data sets.
####Runs weekly

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse, 
       scales) 

rss <- read_csv("C:/AOS_db/data/16_fully_coded_url_read_clean_tagged_dates.csv") %>%
  select(agg_objectid, full_date, week_month_year, link, article_source, agencies_involved_list, si_mention, gss_mention, potential_si_violation, attack_topic_list, attack_type_list, enacted_list, attack_summary)

human_coding <- read_csv("C:/AOS_db/updating_formatting_human_coding_results/results/12_attack_title_summary.csv") %>%
  mutate(date = as.Date(full_date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")),
         week_of_year_date = isoweek(date),
         date = as.Date(date, format = "%Y-%m-%d"),
         full_date = format(date, "%m/%d/%Y"),
         first_day_of_month = floor_date(date, "month"),
         week_of_year_first_day = isoweek(first_day_of_month),
         week_of_month = week_of_year_date - week_of_year_first_day + 1,
         week_month_year = paste0(week_of_month %>% ordinal(), ", ", month(date, label = TRUE), ", ", year(date))) %>%
  rowwise() %>%
  select(agg_objectid, full_date, link, article_source, agencies_involved_list, si_mention, gss_mention, potential_si_violation, attack_type_list, attack_topic_list, enacted_list, attack_summary, week_month_year)

all_the_data <- bind_rows(rss, human_coding) %>%
  mutate(attack_topic_list = gsub("health_safety", "health & safety", attack_topic_list),
         attack_topic_list = gsub("climate_science", "climate science", attack_topic_list),
         attack_topic_list = gsub("elections_and_voting", "elections & voting", attack_topic_list),
         attack_type_list = gsub("_", " ", attack_type_list),
         attack_type_list = gsub("& and", "and", attack_type_list),
         enacted_list = gsub("completed, and threatened", "completed and threatened", enacted_list)) %>%
  rename(attack_topic_variable = attack_topic_list,
         attack_type_variable = attack_type_list,
         agencies_involved = agencies_involved_list,
         attack_completion = enacted_list)

write_csv(all_the_data, "C:/AOS_db/data/17_all_the_aos_data.csv")
