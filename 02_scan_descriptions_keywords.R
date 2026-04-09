#### R SCRIPT PURPOSE: 
#### Pulls table of daily RSS feeds (from Script 01) and screens RSS descriptions/titles using first AOS search term criteria.
#### Runs 1x/day

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse, 
       readtext, 
       purrr,
       readxl) 

gc()

setwd("C:/AOS_db/data/")


### Compiling RSS Feed Data from Previous Three Days, Generally Reflecting a News Cycle ###


all_data_files <- list.files(include.dirs = FALSE)

feed_files <- all_data_files[grepl("\\d{4}\\-\\d{2}\\-\\d{2}", all_data_files)]

feed_files <- feed_files[!grepl("screened", feed_files)]

feed_files <- feed_files[grepl("01_rss_feed_dfs", feed_files)]

last_3_days <- c(Sys.Date(), Sys.Date() - 1, Sys.Date() - 2, Sys.Date() - 3)

last_3_days <- paste(last_3_days, collapse = "|")

feed_files <- feed_files[grepl(last_3_days, feed_files)]

#Removing duplicates and combining RSS feeds from previous 3 days into one df
todays_feed <- map_dfr(feed_files, read_csv) %>% 
  distinct() %>% 
  mutate(description = ifelse(is.na(description), title, description),       description_original = description,
         title_original = title,
         description = tolower(description),
         title = tolower(title),
         title = gsub("- apnews.com|- ap news|stat+", "", title),
         description = gsub("- apnews.com|- ap news|CDATA|<p>|&nbsp;", "", description),
         description_search = paste0(title, ". ", description)) %>%
  unique()


### Reading In and Cleaning Up Search Terms ###


search_terms <- read_excel("C:/AOS_db/info_tables/Search Terms AOS.xlsx")

search_terms <- search_terms %>%
  mutate(search_term = tolower(search_term))

#Break the search terms into 4 categories - negative verbs and government, science, and topic terms
gov_terms <- search_terms %>%
  filter(category == "government")
gov_terms <- gov_terms$search_term %>% str_trim(., side = "both")

science_terms <- search_terms %>%
  filter(category == "science")
science_terms <- science_terms$search_term %>% str_trim(., side = "both")

topic_terms <- search_terms %>%
  filter(category == "topics")
topic_terms <- topic_terms$search_term %>% str_trim(., side = "both")

attack_terms <- search_terms %>%
  filter(category == "negative verbs")
attack_terms <- attack_terms$search_term %>% str_trim(., side = "both")

#Add prefixes (anti, anti-) and suffixes (skeptic, denier) to science terms
suffix_terms <- search_terms %>%
  filter(category == "suffix")
suffix_terms <- suffix_terms$search_term %>% str_trim(., side = "both")

prefix_terms <- search_terms %>%
  filter(category == "prefix")
prefix_terms <- prefix_terms$search_term %>% str_trim(., side = "both")

prefix_topic <- outer(prefix_terms, topic_terms, paste0) %>% c() %>% unique()
prefix_topic_space <- outer(prefix_terms, topic_terms, paste) %>% c() %>% unique()
prefix_topic <- c(prefix_topic, prefix_topic_space) %>% unique()
prefix_topic <- prefix_topic[!grepl("- ", prefix_topic)]
science_terms <- c(science_terms, prefix_topic)

topic_suffix <- outer(topic_terms, suffix_terms, paste0) %>% c() %>% unique()
topic_suffix_space <- outer(topic_terms, suffix_terms, paste) %>% c() %>% unique()
topic_suffix <- c(topic_suffix, topic_suffix_space) %>% unique()
science_terms <- c(science_terms, topic_suffix)

science_terms <- paste0(science_terms,  collapse = "|")
topic_terms <- paste0(topic_terms,  collapse = "|")
gov_terms <- paste0(gov_terms,  collapse = "|")
attack_terms <- paste0(attack_terms,  collapse = "|")

#Screen RSS descriptions/titles using RSS feed search term criteria
#RSS feed search term criteria = ([government] OR [science]) AND [topic] AND [negative verb]
science_gov_terms <- paste0(science_terms, gov_terms, collapse = "|")

sci_gov_terms_db_text <- keep(todays_feed$description_search, 
function(x) grepl(science_gov_terms, x))
attack_terms_db_text <- keep(todays_feed$description_search, 
function(x) grepl(attack_terms, x))
topic_terms_db_text <- keep(todays_feed$description_search, 
function(x) grepl(topic_terms, x))

todays_feed_final <- filter(todays_feed, 
                            description_search %in% sci_gov_terms_db_text,
                            description_search %in% attack_terms_db_text,
                            description_search %in% topic_terms_db_text)


### Getting Data Ready for Next Script in Sequence ###


#Reading in past articles from RSS feeds that have already been screened for key words
existing_feed <- read_csv("C:/AOS_db/data/02_rss_feed_screened_dfs.csv") %>%
  filter(source != "Gov Info")

all_feed_final <- bind_rows(todays_feed_final, existing_feed) %>% 
  unique() 

#Eliminating duplicate articles, videos, and opinion articles
all_feed_final_no_dupes <- all_feed_final %>%
  select(-description_search) %>%
  mutate(description = gsub("[[:punct:]]", "", description),
         description = str_remove_all(description, "nbsp|ap news|ap news"),
         description = str_trim(description, side = "both"),
         title = gsub("[[:punct:]]", "", title),
         title = str_remove_all(title, "apnews.com"),
         title = str_remove_all(title, "AP News|ap news|ap news"),
         title = str_trim(title, side = "both")) %>%
  group_by(URL, description) %>%
  slice_max(order_by = pub_date, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(title, description) %>%
  slice_max(order_by = pub_date, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(title, source) %>%
  slice_max(., order_by = pub_date, n = 1, with_ties = FALSE) %>%   ungroup() %>%
  filter(URL != "https://www.washingtonpost.com",
         !grepl("opinion", URL),
         !grepl("opinion:", title),
         !grepl("today.com/video", URL))

write_csv(all_feed_final_no_dupes, "C:/AOS_db/data/02_rss_feed_screened_dfs.csv")