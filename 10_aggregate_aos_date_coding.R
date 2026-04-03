#### R SCRIPT PURPOSE: 
#### Pulls the most recent 3 days of RSS feed articles and
### makes groups of articles with XX matching words from the past 3 days to determine what articles are discussing similar attacks
#### Runs weekly

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


aoses_clean <- read_csv("C:/AOS_db/data/09_aos_clean_gov_si_gss.csv") %>%
  select(title, description, pub_date, URL, source, description_original, url_text, url_text_original, gov_agency, SI_mention, GSS_mention) %>%
  unique()

#Filter data to after December 19th to eliminate articles that have already been checked for multiple attacks on science
aoses_clean <- aoses_clean %>%
  mutate(pub_date = as.Date(pub_date)) %>%
  filter(pub_date >= "2025-12-19")

#Filter duplicates to articles with highest character url_text, or read in articles
aoses_clean <- aoses_clean %>%
  mutate(num_chars = nchar(url_text)) %>%
  group_by(pub_date, title, source) %>%
  slice_max(num_chars, n = 1) %>%
  ungroup() %>%
  unique() %>%
  select(-num_chars)

rows <- seq(1:nrow(aoses_clean))

aoses_clean$title <- str_conv(aoses_clean$title, "UTF-8")

descriptions <- aoses_clean %>%
  select(description) %>%
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
    date_i <- aoses_clean$pub_date[i]
    date_j <- aoses_clean$pub_date[j]
    date_diff <- date_i - date_j
    if(date_diff != 0 && date_diff < 3 && date_diff > -3){
      temp_1 <- descriptions_swf[i] %>% stri_remove_empty()
      temp_2 <- descriptions_swf[j] %>% stri_remove_empty()
      
      compare_single_1 <- data.frame(feature = temp_1, comp = i, stringsAsFactors = FALSE) %>%
        unique()
      colnames(compare_single_1) <- c("feature", "comp")
      
      compare_single_2 <- data.frame(feature = temp_2, comp = j, stringsAsFactors = FALSE) %>%
        unique()
      colnames(compare_single_2) <- c("feature", "comp")
      
      compare_single <- bind_rows(compare_single_1, compare_single_2)
      
      compare_single <- compare_single %>%
        filter(!is.null(feature),
               nchar(feature) > 1) %>%
        mutate(total_count = n()/2) %>%
        group_by(feature, total_count) %>%
        mutate(count = n(),
               combo = paste0(i, "_and_", j)) %>%
        ungroup() %>%
        filter(count > 1) %>%
        group_by(combo, total_count) %>%
        summarise(num_in_both = n()/2) %>%
        ungroup()
      
      all_combos <- bind_rows(compare_single, all_combos)
    }
  }}


total_same_words <- all_combos %>%
  mutate(aggregate = ifelse(num_in_both > 3, "aggregate", "individual")) %>%
  separate_wider_delim(combo, names = c("description_1", "description_2"), delim = "_and_", cols_remove = FALSE)

aoses_clean <- aoses_clean %>%
  mutate(description_number = 1:nrow(.) %>% 
           as.character())

aoses_clean_agg <- left_join(aoses_clean, total_same_words, 
                             by = c("description_number" = "description_1")) %>% select(-description_2)

aoses_clean_agg_2 <- left_join(aoses_clean, total_same_words, 
                               by = c("description_number" = "description_2")) %>% select(-description_1)

aoses_clean_agg_final <- bind_rows(aoses_clean_agg, aoses_clean_agg_2) %>%
  select(-c(description_number, total_count, num_in_both)) %>%
  unique()

##Where there are multiples, pull the article to go into the database using the source prioritization

priority_sources <- c("The Hill", "Associated Press", "Stat News", "E&E News", "Stateline Democracy", "Gov Exec", "National Broadcasting Corporation", "National Public Radio")

aoses_clean_agg_final <- aoses_clean_agg_final %>%
  mutate(article_source = factor(source, levels = priority_sources)) %>%
  group_by(combo, gov_agency) %>%
  arrange(article_source) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(title, description, pub_date, URL, source, description_original, url_text, url_text_original, gov_agency, SI_mention, GSS_mention) %>%
  unique()

### Get Data Ready for Next Script ###

write_csv(aoses_clean_agg_final, "C:/AOS_db/data/10_aoses_clean_aggregate_aos.csv")
