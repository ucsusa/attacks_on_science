#### R SCRIPT PURPOSE: 
#### Pulls the most recent 2 weeks of RSS feed articles and makes groups of articles based on:
#### articles +/- 2 days of publication (to approximate a news cycle) and 
#### with 30% matching words in the title and/or descriptions to identify potential duplicate attacks.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse, 
       tidytext, 
       quanteda, 
       quanteda.textstats, 
       stopwords,
       stringi) 

aoses_clean <- read_csv("../data/09_aos_clean_gov_si_gss.csv") %>%
  select(title, description, pub_date, URL, source, description_original, url_text, url_text_original, gov_agency, SI_mention, GSS_mention) %>%
  unique()

#Filter data to today's date and the previous 2 weeks to limit pare down articles already run through this script 
#It takes a while to compare all iterations within the estimated news cycle of +/-2 days

todays_date <- Sys.Date()
two_weeks_ago <- as.Date(todays_date) - 14

aoses_clean <- aoses_clean %>%
  mutate(pub_date = as.Date(pub_date)) %>%
  filter(pub_date > two_weeks_ago)

#Filter multiples of articles to those with the highest character url_text, these are the most completely read in
aoses_clean <- aoses_clean %>%
  mutate(num_chars = nchar(url_text)) %>%
  group_by(pub_date, title, source) %>%
  slice_max(num_chars, n = 1) %>%
  ungroup() %>%
  unique() %>%
  select(-num_chars)

#Add an identifier to track description comparisons
aoses_clean <- aoses_clean %>%
  mutate(compareobjectid = 1:nrow(.),
         compareobjectid = paste0("DESC", str_pad(as.character(compareobjectid), width = 5, side = "left", pad = "0")))
  
rows <- seq(1:nrow(aoses_clean))

aoses_clean$title <- str_conv(aoses_clean$title, "UTF-8")
dates <- unique(as.Date(aoses_clean$pub_date))
dates <- dates[dates > as.Date("2025-10-07")] 
all_combos_dates <- data.frame()

for(k in dates){
  aos_date_1 <- as.Date(k) + 2
  aos_date_2 <- as.Date(k) - 2
  
  aoses_clean_split <- filter(aoses_clean, between(as.Date(pub_date), as.Date(aos_date_2), as.Date(aos_date_1)))
  
descriptions <- aoses_clean_split %>%
  select(description, compareobjectid) %>%
  rename(text = description) %>%
  mutate(text = tolower(text)) %>%
  mutate(text = gsub("\uFFFD", " ", text))

descriptions_corpus <- corpus(descriptions)

descriptions_swf <- tokens(descriptions_corpus, 
                     what = "word",
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     remove_separators = TRUE,
                     split_hyphens = TRUE,
                     padding = TRUE)

descriptions_swf <- tokens_select(descriptions_swf, 
                            pattern = stopwords("en", source = "snowball"), 
                            selection = "remove")

descriptions_swf <- tokens_wordstem(
  descriptions_swf,
  language = quanteda_options("language_stemmer"),
  verbose = quanteda_options("verbose"))


compare_1 <- 1:ndoc(descriptions_swf)
compare_2 <- 1:ndoc(descriptions_swf)

all_combos <- data.frame(stringsAsFactors = FALSE)


for(i in compare_1){
  for(j in compare_2){
    if(i < j){
      temp_1 <- descriptions_swf[i] %>% stri_remove_empty()
      temp_2 <- descriptions_swf[j] %>% stri_remove_empty()
      
      compare_single_1 <- data.frame(feature = temp_1, comp = i, compareobjectid_1 = aoses_clean_split$compareobjectid[i], stringsAsFactors = FALSE) %>%
        unique()
      colnames(compare_single_1) <- c("feature", "comp", "compareobjectid_1")
      
      compare_single_2 <- data.frame(feature = temp_2, comp = j, compareobjectid_2 = aoses_clean_split$compareobjectid[j], stringsAsFactors = FALSE) %>%
        unique()
      colnames(compare_single_2) <- c("feature", "comp", "compareobjectid_2")
      
      compare_single <- bind_rows(compare_single_1, compare_single_2)
      
      compare_single <- compare_single %>%
        unite("compareobjectid", compareobjectid_1, compareobjectid_2, na.rm = T, remove = TRUE) %>%
        filter(!is.null(feature),
               nchar(feature) > 1,
               trimws(feature) != "",
               grepl("[A-Za-z0-9]", feature)) %>%
        mutate(total_count = n()/2) %>%
        group_by(feature, total_count) %>%
        mutate(count = n()) %>%
        ungroup() %>%
        filter(count > 1) %>%
        group_by(total_count, count) %>%
        summarise(num_in_both = n()/2,
                  combo = paste0(i, "_and_", j),
                  combo_id = paste0(unique(compareobjectid), collapse = "and")) %>%
        ungroup()
      
      all_combos <- bind_rows(compare_single, all_combos)
    }
  }}

all_combos_dates <- bind_rows(all_combos, all_combos_dates)

}

#write_csv(all_combos_dates, "../data/10_all_combos_dates_12.csv")

# setwd("../data/")
# all_combos_files <- list.files()
# all_combos_files <- all_combos_files[grepl("all_combos_dates", all_combos_files)]
# 
# all_combos_data <- map_dfr(all_combos_files, read_csv) %>%
#   distinct()
# 
# all_combos_dates <- all_combos_data

total_same_words <- all_combos_dates %>%
  mutate(percent_agreement = num_in_both/total_count,
         aggregate = ifelse(percent_agreement >= .3, "aggregate", "individual")) %>%
#  separate_wider_delim(combo, names = c("description_1", "description_2"), delim = "_and_", cols_remove = FALSE) %>%
  separate_wider_delim(combo_id, names = c("combo_id_1", "combo_id_2"), delim = "and", cols_remove = FALSE)

#aoses_clean <- aoses_clean %>%
 # mutate(description_number = 1:nrow(.) %>% 
  #         as.character())

aoses_clean_agg <- left_join(aoses_clean, total_same_words, 
                             by = c("compareobjectid" = "combo_id_1")) %>% select(-c(combo, combo_id_2, combo_id))

aoses_clean_agg_2 <- left_join(aoses_clean, total_same_words, 
                               by = c("compareobjectid" = "combo_id_2")) %>% select(-c(combo, combo_id_1, combo_id))

aoses_clean_agg_final <- bind_rows(aoses_clean_agg, aoses_clean_agg_2) %>%
  #select(-c(description_number, total_count, num_in_both)) %>%
  distinct()

#Where there are multiples, pull the article to go into the database using the source prioritization list
#The New York Times and the Washington Post are legacy sources and are no longer used

priority_sources <- c("The Hill", "Associated Press", "Stat News", "E&E News", "Politico", "Stateline Democracy", "Gov Exec", "National Broadcasting Corporation", "National Public Radio", "New York Times", "Washington Post")

aoses_clean_agg_final <- aoses_clean_agg_final %>%
  mutate(article_source = factor(source, levels = priority_sources)) %>%
  group_by(compareobjectid, gov_agency, aggregate) %>%
  arrange(article_source) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(title, description, pub_date, URL, source, description_original, url_text, url_text_original, gov_agency, SI_mention, GSS_mention) %>%
  distinct()

#Pull in already-tested articles
already_agg_tested <- read_csv("../data/10_aoses_clean_aggregate_aos.csv")

aoses_clean_agg_final <- bind_rows(aoses_clean_agg_final, already_agg_tested) %>%
  distinct() %>%
  mutate(article_source = factor(source, levels = priority_sources)) %>%
  group_by(title, description, pub_date, URL, source) %>%
  arrange(article_source) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(title, description, pub_date, URL, source, description_original, url_text, url_text_original, gov_agency, SI_mention, GSS_mention) %>%
  distinct()

write_csv(aoses_clean_agg_final, "../data/10_aoses_clean_aggregate_aos.csv")
