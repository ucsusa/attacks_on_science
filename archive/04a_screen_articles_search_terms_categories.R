## This script pulls in the coded results and checks against search terms for accuracy: false positives, false negatives, etc.

##
library(tidyverse)
library(janitor)
library(purrr)
library(rvest)
library(R.utils)
library(rvest)
library(purrr)
library(readtext)


gc()

aos_pot_urls_read <- read_csv("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/testing_the_script/results/03_coded_url_read.csv") %>%
  filter(source != "Gov Info")

already_read_articles <- read_csv("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/updating_formatting_human_coding_results/results/04_coded_url_read.csv") %>%
  select(`FULL DATE`, HEADLINE, LINK, `AGENCIES INVOLVED`, `ARTICLE SOURCE`, CODERS, `AOS PRESENCE`, title, url_text, file_name) %>%
  filter(source != "Gov Info")

aos_pot_urls_read <- bind_rows(aos_pot_urls_read, already_read_articles) %>%
  select(-1) %>%
  unique() %>%
  mutate(num_symbol = str_count(url_text, "[[:punct:]]")) %>%
  group_by(`FULL DATE`, HEADLINE, LINK, `AGENCIES INVOLVED`, `ARTICLE SOURCE`, CODERS, `AOS PRESENCE`) %>%
  arrange(num_symbol) %>%
  slice(1) %>%
  ungroup()


aos_coded_total <- filter(aos_pot_urls_read, `AOS PRESENCE` == 1)

aos_pot_urls_read <- aos_pot_urls_read %>%
  unique() %>%
  mutate(url_text_original = url_text,
         url_text = tolower(url_text))

#grab all of the search terms
search_terms <- read_excel("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/data/Search Terms AOS.xlsx")

#format search terms: change to lowercase, remove blank spaces and quotation marks
search_terms <- search_terms %>%
  mutate(search_term = tolower(search_term))

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

#searching URL text for words that belong in the three categories
gov_terms_db_text <- keep(aos_pot_urls_read$url_text, 
                          function(x) grepl(gov_terms, x))
science_terms_db_text <- keep(aos_pot_urls_read$url_text, 
                              function(x) grepl(science_terms, x))
attack_terms_db_text <- keep(aos_pot_urls_read$url_text, 
                             function(x) grepl(attack_terms, x))
topic_terms_db_text <- keep(aos_pot_urls_read$url_text, 
                            function(x) grepl(topic_terms, x))

aos_pot_urls_read_screened <- filter(aos_pot_urls_read, 
                  url_text %in% gov_terms_db_text,
                  url_text %in% science_terms_db_text,
                  url_text %in% attack_terms_db_text,
                  url_text %in% topic_terms_db_text)

aos_not_yet <- anti_join(aos_pot_urls_read, aos_pot_urls_read_screened)

aos_pot_urls_review_table <- mutate(aos_pot_urls_read, search_term_guess = ifelse(url_text %in% aos_pot_urls_read_screened$url_text, "guess yes", "guess no"))

write_csv(aos_pot_urls_review_table, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/testing_the_script/results/04_guess_coded_review_table_urls_categories.csv")

##guessed yes, was yes
guess_yes <- aos_pot_urls_read_screened %>%
  filter(`AOS PRESENCE` == 1) %>%
  group_by() %>%
  summarise(yes_yes = n())
yes_yes <- guess_yes$yes_yes

##guessed yes, was no
guess_yes <- aos_pot_urls_read_screened %>%
  filter(`AOS PRESENCE` == 0) %>%
  group_by() %>%
  summarise(yes_no = n())
yes_no <- guess_yes$yes_no


##guessed no, was yes
guess_no <- aos_not_yet %>%
  filter(`AOS PRESENCE` == 1) %>%
  group_by() %>%
  summarise(no_yes = n())
no_yes <- guess_no$no_yes

##guess no, was no
guess_no <- aos_not_yet %>%
  filter(`AOS PRESENCE` == 0) %>%
  group_by() %>%
  summarise(no_no = n())
no_no <- guess_no$no_no


##Now make a table of all counts
key_word_results <- data.frame(yes_yes = yes_yes, yes_no = yes_no, no_yes = no_yes, no_no = no_no, total = nrow(aos_pot_urls_read), stringsAsFactors = FALSE)

write_csv(key_word_results, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/testing_the_script/results/04_screened_urls_categories.csv")


check_missed_sources <- aos_pot_urls_review_table %>%
  filter(`AOS PRESENCE` == 1,
         search_term_guess == "guess no") %>%
  group_by(`ARTICLE SOURCE`) %>%
  summarise(count_source = n()/55)

search_term_categories_missing <- aos_pot_urls_review_table %>%
  filter(`AOS PRESENCE` == 1,
         search_term_guess == "guess no")

gov_terms_db_text <- keep(search_term_categories_missing$url_text, 
                          function(x) grepl(gov_terms, x))
science_terms_db_text <- keep(search_term_categories_missing$url_text, 
                              function(x) grepl(science_terms, x))
attack_terms_db_text <- keep(search_term_categories_missing$url_text, 
                             function(x) grepl(attack_terms, x))
topic_terms_db_text <- keep(search_term_categories_missing$url_text, 
                            function(x) grepl(topic_terms, x))

search_term_categories_missing_topics <- search_term_categories_missing %>%
  mutate(gov_terms = ifelse(url_text %in% gov_terms_db_text, 1, 0),
    science_terms = ifelse(url_text %in% science_terms_db_text, 1, 0),
    attack_terms = ifelse(url_text %in% attack_terms_db_text, 1, 0),
    topic_terms = ifelse(url_text %in% topic_terms_db_text, 1, 0))

science_sum <- sum(search_term_categories_missing_topics$science_terms)

gov_sum <- sum(search_term_categories_missing_topics$gov_terms)

attack_sum <- sum(search_term_categories_missing_topics$attack_terms)

topic_sum <- sum(search_term_categories_missing_topics$topic_terms)

search_term_categories_missing_topics <- search_term_categories_missing_topics %>%
  mutate(missing_terms = 4 - rowSums(across(gov_terms:topic_terms)))

no_attack_verb <- search_term_categories_missing_topics %>%
  filter(!grepl("erify you are|and not a bot|his site can't be reached|could not be found|you have been blocked", url_text))

write_csv(no_attack_verb, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/testing_the_script/results/04_missing_search_terms.csv")

