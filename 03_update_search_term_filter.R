#### R SCRIPT PURPOSE: 
#### Pulls RSS feeds from Script 01 and filters down with list of search terms. 
#### The filtering is based on the RSS Feed article descriptions.
#### If there is no article description, then the article title is screened.
#### Runs 1X/DAY

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


##Read in screened articles df
descriptions_to_filter <- read_csv("C:/AOS_db/data/02_rss_feed_screened_dfs.csv")

### Reading In and Cleaning Up Search Terms ###


#grab all of the search terms
search_terms <- read_excel("C:/AOS_db/info_tables/Search Terms AOS.xlsx")

#format search terms: change to lowercase, remove blank spaces and quotation marks
search_terms <- search_terms %>%
  mutate(search_term = tolower(search_term),
         search_term = ifelse(search_term_acronym == "yes", paste0(" ", search_term, " "), search_term))

##Break the search terms into 3 categories - government, science, and negative verbs
gov_terms <- search_terms %>%
  filter(category == "government")
gov_terms <- gov_terms$search_term

science_terms <- search_terms %>%
  filter(category == "science")
science_terms <- science_terms$search_term

topic_terms <- search_terms %>%
  filter(category == "topics")
topic_terms <- topic_terms$search_term

attack_terms <- search_terms %>%
  filter(category == "negative verbs")
attack_terms <- attack_terms$search_term

suffix_terms <- search_terms %>%
  filter(category == "suffix")
suffix_terms <- suffix_terms$search_term

prefix_terms <- search_terms %>%
  filter(category == "prefix")
prefix_terms <- prefix_terms$search_term

##Make the anti topics terms from the prefixes and add to science terms
prefix_topic <- outer(prefix_terms, topic_terms, paste0) %>% c() %>% unique()
science_terms <- c(science_terms, prefix_topic)

##Make the skeptic and denier topics terms from the suffixes and add to science terms
topic_suffix <- outer(topic_terms, suffix_terms, paste0) %>% c() %>% unique()
science_terms <- c(science_terms, topic_suffix)

##Collapse all search terms
science_terms <- paste0(science_terms,  collapse = "|")
topic_terms <- paste0(topic_terms,  collapse = "|")
gov_terms <- paste0(gov_terms,  collapse = "|")
attack_terms <- paste0(attack_terms,  collapse = "|")

##Combine science and government terms
science_gov_terms <- paste0(science_terms, gov_terms, sep = "|")

#searching URL text for words that belong in the four categories
sci_gov_terms_db_text <- keep(descriptions_to_filter$description, 
function(x) grepl(science_gov_terms, x))
attack_terms_db_text <- keep(descriptions_to_filter$description, 
function(x) grepl(attack_terms, x))
topic_terms_db_text <- keep(descriptions_to_filter$description, 
function(x) grepl(topic_terms, x))

#new data set: articles whose text contains at least one word of each category
descriptions_to_filter_final <- filter(descriptions_to_filter, 
 description %in% sci_gov_terms_db_text,
 description %in% attack_terms_db_text,
 description %in% topic_terms_db_text)


#creating csv file with most up to date screened article list
write_csv(descriptions_to_filter_final, "C:/AOS_db/data/02_rss_feed_screened_dfs.csv")

