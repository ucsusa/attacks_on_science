
#### R SCRIPT PURPOSE: 
#### Combines and formats the data from human-driven and automated data collection.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse, 
       scales, 
       chromote,
       httr) 

rss <- read_csv("C:/AOS_db/data/17_fully_coded_url_read_clean_tagged_dates.csv") %>%
  select(agg_objectid, headline, full_date, week_month_year, link, article_source, agencies_involved_list, si_mention, gss_mention, potential_si_violation, attack_topic_list, attack_type_list, enacted_list, attack_summary)

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
  select(agg_objectid, headline, full_date, link, article_source, agencies_involved_list, si_mention, gss_mention, potential_si_violation, attack_type_list, attack_topic_list, enacted_list, attack_summary, week_month_year)

#Correct to 2/2026 potential SI violation definition, omitting resets or dismantling of science advisory committees
human_coding <- human_coding %>%
  mutate(potential_si_violation = ifelse(grepl("altering_study_results|data_accessibility|data_collection|censorship|restrictions_from_professional_engagement", attack_type_list), "yes", "no"))

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

#Add proper AP URL to the final spreadsheet
all_the_data_no_ap <- all_the_data %>%
  filter(article_source != "Associated Press")

all_the_data_ap <- all_the_data %>%
  filter(article_source == "Associated Press") 

ap_links <- unique(all_the_data_ap$link)
all_the_data_ap_url <- data.frame(stringsAsFactors = FALSE)

for(i in ap_links){
  all_the_data_ap_split <- filter(all_the_data_ap, link == i)
  b <- ChromoteSession$new()
  Sys.sleep(4)
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(4)
  b$Page$navigate(i)
  Sys.sleep(4)
  url_link <- b$Runtime$evaluate("window.location.href")$result$value
  all_the_data_ap_split <- mutate(all_the_data_ap_split, 
                               article_source = "AP News",
                             link = url_link)
  all_the_data_ap_url <- bind_rows(all_the_data_ap_url, all_the_data_ap_split) 
  b$close()
}

all_the_data <- bind_rows(all_the_data_no_ap, all_the_data_ap_url)

write_csv(all_the_data, "C:/AOS_db/data/18_all_the_aos_data.csv")

#### R SCRIPT PURPOSE: 
####This script combines and formats the human coded and the rss feed data sets.
####Runs weekly

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse, 
       scales, 
       chromote,
       httr) 

rss <- read_csv("C:/AOS_db/data/17_fully_coded_url_read_clean_tagged_dates.csv") %>%
  select(agg_objectid, headline, full_date, week_month_year, link, article_source, agencies_involved_list, si_mention, gss_mention, potential_si_violation, attack_topic_list, attack_type_list, enacted_list, attack_summary)

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
  select(agg_objectid, headline, full_date, link, article_source, agencies_involved_list, si_mention, gss_mention, potential_si_violation, attack_type_list, attack_topic_list, enacted_list, attack_summary, week_month_year)

##Correct to 2/2026 potential SI violation definition, omitting resets or dismantling of science advisory committees
human_coding <- human_coding %>%
  mutate(potential_si_violation = ifelse(grepl("altering_study_results|data_accessibility|data_collection|censorship|restrictions_from_professional_engagement", attack_type_list), "yes", "no"))

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

##Add proper AP URL to the final spreadsheet
all_the_data_no_ap <- all_the_data %>%
  filter(article_source != "Associated Press")

all_the_data_ap <- all_the_data %>%
  filter(article_source == "Associated Press") 

ap_links <- unique(all_the_data_ap$link)
all_the_data_ap_url <- data.frame(stringsAsFactors = FALSE)

for(i in ap_links){
  all_the_data_ap_split <- filter(all_the_data_ap, link == i)
  b <- ChromoteSession$new()
  Sys.sleep(4)
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(4)
  b$Page$navigate(i)
  Sys.sleep(4)
  url_link <- b$Runtime$evaluate("window.location.href")$result$value
  all_the_data_ap_split <- mutate(all_the_data_ap_split, 
                               article_source = "AP News",
                             link = url_link)
  all_the_data_ap_url <- bind_rows(all_the_data_ap_url, all_the_data_ap_split) 
  b$close()
}

all_the_data <- bind_rows(all_the_data_no_ap, all_the_data_ap_url)

write_csv(all_the_data, "C:/AOS_db/data/18_all_the_aos_data.csv")

