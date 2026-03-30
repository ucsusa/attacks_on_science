#### R SCRIPT PURPOSE: 
#### Pulls RSS feeds from Script 01 and filters down with list of search terms. 
#### The filtering is based on the RSS Feed article descriptions.
#### If there is no article description, then the article title is screened.
#### Runs 1X/DAY
###Output (from this script) deletion guidance -- This script runs daily. The next script runs weekly on Wednesdays at 8AM. This next script (03_) reads in the output from this script (02_) and then eliminates any articles that have already been scraped or read in by pdf using a mid point file save from the 05_ script. The rows in this file can be truncated after 03 completes its task. Caution - If we change any search terms, this will need to be rerun. Right now (1/6/2026) we don't have a way to go back and pull old rss feed. For now we could truncate rows of this file right before 03_ is about to run again on Wednesday mornings. Recommendation - Truncate all rows that were in the spreadsheet a week prior (i.e. before 03 started the previous week).

### Logistics for R Script ###

#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data
       readtext, #to import/handle text files and their metadata
       purrr,
       readxl) #to automate/loop specific functions

#garbage collection; removing things from memory that are no longer in use
gc()

#setting working directory
setwd("C:/AOS_db/data/")


### Compiling RSS Feed Data from Previous Three Days ###


#compiling all data files in above folder
all_data_files <- list.files(include.dirs = FALSE)

#narrowing to data files that match what's specified in grepl command
feed_files <- all_data_files[grepl("\\d{4}\\-\\d{2}\\-\\d{2}", all_data_files)]

#eliminating files whose names contain the word "screened"
feed_files <- feed_files[!grepl("screened", feed_files)]

#only including files created from R script 01
feed_files <- feed_files[grepl("01_rss_feed_dfs", feed_files)]

#identifying the last three days
last_3_days <- c(Sys.Date(), Sys.Date() - 1, Sys.Date() - 2, Sys.Date() - 3)

#creating time frame of last three days to filter data files
last_3_days <- paste(last_3_days, collapse = "|")

#only including data files created from R script 01 from the last three days
feed_files <- feed_files[grepl(last_3_days, feed_files)]

#creating data frame of RSS data files (created from R script 01) from the last three days
todays_feed <- map_dfr(feed_files, read_csv) %>% #importing articles from RSS feeds from last three days
  distinct() %>% #eliminating duplicates
  mutate(description = ifelse(is.na(description), title, description), #new column; if there's no description, including the article title in the description column
         description_original = description,
         title_original = title,
         description = tolower(description),
         title = tolower(title),
         title = gsub("- apnews.com|- ap news|stat+", "", title),
         description = gsub("- apnews.com|- ap news|CDATA|<p>|&nbsp;", "", description),
         description_search = paste0(title, ". ", description)) %>%
  unique()#changing article description to all lowercase


### Reading In and Cleaning Up Search Terms ###


#grab all of the search terms
search_terms <- read_excel("C:/AOS_db/info_tables/Search Terms AOS.xlsx")

#format search terms: change to lowercase, remove blank spaces and quotation marks
search_terms <- search_terms %>%
  mutate(search_term = tolower(search_term))

##Break the search terms into 3 categories - government, science, and negative verbs
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

suffix_terms <- search_terms %>%
  filter(category == "suffix")
suffix_terms <- suffix_terms$search_term %>% str_trim(., side = "both")

prefix_terms <- search_terms %>%
  filter(category == "prefix")
prefix_terms <- prefix_terms$search_term %>% str_trim(., side = "both")

##Make the anti topics terms from the prefixes and add to science terms
prefix_topic <- outer(prefix_terms, topic_terms, paste0) %>% c() %>% unique()
prefix_topic_space <- outer(prefix_terms, topic_terms, paste) %>% c() %>% unique()
prefix_topic <- c(prefix_topic, prefix_topic_space) %>% unique()
prefix_topic <- prefix_topic[!grepl("- ", prefix_topic)]
science_terms <- c(science_terms, prefix_topic)

##Make the skeptic and denier topics terms from the suffixes and add to science terms
topic_suffix <- outer(topic_terms, suffix_terms, paste0) %>% c() %>% unique()
topic_suffix_space <- outer(topic_terms, suffix_terms, paste) %>% c() %>% unique()
topic_suffix <- c(topic_suffix, topic_suffix_space) %>% unique()
science_terms <- c(science_terms, topic_suffix)

##Collapse all search terms
science_terms <- paste0(science_terms,  collapse = "|")
topic_terms <- paste0(topic_terms,  collapse = "|")
gov_terms <- paste0(gov_terms,  collapse = "|")
attack_terms <- paste0(attack_terms,  collapse = "|")

##Combine science and government terms
science_gov_terms <- paste0(science_terms, gov_terms, sep = "|")

#searching URL text for words that belong in the four categories
sci_gov_terms_db_text <- keep(todays_feed$description_search, 
function(x) grepl(science_gov_terms, x))
attack_terms_db_text <- keep(todays_feed$description_search, 
function(x) grepl(attack_terms, x))
topic_terms_db_text <- keep(todays_feed$description_search, 
function(x) grepl(topic_terms, x))

#new data set: articles whose text contains at least one word of each category
todays_feed_final <- filter(todays_feed, 
                            description_search %in% sci_gov_terms_db_text,
                            description_search %in% attack_terms_db_text,
                            description_search %in% topic_terms_db_text)

### Screening RSS Feed Articles Using Search Terms ###

### Getting Data Ready for Next Script in Sequence (03) ###


#reading in past articles from RSS feeds that have already been screened for key words
existing_feed <- read_csv("C:/AOS_db/data/02_rss_feed_screened_dfs.csv") %>%
  filter(source != "Gov Info")

#combining screened articles from today with previous screened articles
all_feed_final <- bind_rows(todays_feed_final, existing_feed) %>% 
  unique() #eliminating duplicates


#Eliminate duplicate news stories from feed that have different URLs or titles and eliminate feed where the only URL goes to the top of WAPO's website. Eliminate known opinion articles.
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

#creating csv file with most up to date screened article list
write_csv(all_feed_final_no_dupes, "C:/AOS_db/data/02_rss_feed_screened_dfs.csv")

