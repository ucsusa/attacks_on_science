#### R SCRIPT PURPOSE: 
#### Pulls the most recent 3 days of RSS feed articles and
### makes groups of articles with XX matching words from the past 3 days to determine what articles are discussing similar attacks
#### Runs (?) How often will this script run?

##Filtering needs
#1. Filter out articles that were screened out by search terms in previous scripts, or by human coding.
#2. Filter out articles that pre-date December 16, 2025 DONE
#3. Filter out duplicate articles, with a preference to the most number of characters read in or in url_text. DONE


### Logistics for R Script ###


#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data
       tidytext, #to facilitate text mining and create tidy data sets
       quanteda, #to conduct quantitative text analysis
       quanteda.textstats, #lends specific analyses for text
       stopwords,
       stringi) #multiple sources of stopwords

### Import and Organize Necessary Data ###
aoses_clean <- read_csv("C:/AOS_db/data/09_aos_clean_gov_si_gss.csv") %>%
  select(title, description, pub_date, URL, source, description_original, url_text, url_text_original, gov_agency, SI_mention, GSS_mention) %>%
  unique()

#Filter data to after December 16th
aoses_clean <- aoses_clean %>%
  mutate(pub_date = as.Date(pub_date)) %>%
  filter(pub_date >= "2025-12-16")

#Filter duplicates to articles with highest character url_text, or read in articles
aoses_clean <- aoses_clean %>%
  mutate(num_chars = nchar(url_text)) %>%
  group_by(pub_date, title, source) %>%
  slice_max(num_chars, n = 1) %>%
  ungroup() %>%
  unique() %>%
  select(-num_chars)

#create identifier by row number
rows <- seq(1:nrow(aoses_clean))

#format file names
aoses_clean$title <- str_conv(aoses_clean$title, "UTF-8")


### Get Text Data Ready for Analysis ###
## Clean up the titles, make lowercase
descriptions <- aoses_clean %>%
  select(description) %>%
  rename(text = description) %>%
  mutate(text = tolower(text)) %>%
  mutate(text = gsub("\uFFFD", " ", text))

#create a corpus of RSS Feed descriptions
descriptions_corpus <- corpus(descriptions)

#break descriptions down into words and remove punctuation, extra space, etc.
descriptions_swf <- tokens(descriptions_corpus, 
                     what = "word",
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE,
                     remove_separators = TRUE,
                     split_hyphens = TRUE,
                     padding = TRUE)

#remove common stop words from the corpus of RSS Feed descriptions
descriptions_swf <- tokens_select(descriptions_swf, 
                            pattern = stopwords("en", source = "snowball"), 
                            selection = "remove")

#reduce words in corpus down to their stems
descriptions_swf <- tokens_wordstem(
  descriptions_swf,
  language = quanteda_options("language_stemmer"),
  verbose = quanteda_options("verbose"))


compare_1 <- 1:ndoc(descriptions_swf)
compare_2 <- 1:ndoc(descriptions_swf)

#creating a shell to put data into
all_combos <- data.frame(stringsAsFactors = FALSE)


### Match Potential Duplicate AOS by RSS Feed Descriptions ###
##Set up a loop to roll through the pub_date plus or minus 2 days

#new loop
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

##Pull final article to go into database using the source prioritization: The Hill -> AP News -> STAT News -> E&E News -> Stateline-> GovExec -> WAPO -> NYT

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

#create new data set
write_csv(aoses_clean_agg_final, "C:/AOS_db/data/10_aoses_clean_aggregate_aos.csv")
