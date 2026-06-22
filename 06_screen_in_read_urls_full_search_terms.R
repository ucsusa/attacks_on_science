#### R SCRIPT PURPOSE: 
#### Screens full article text (from scraping and pdf saving) using second AOS search term criteria.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse,
       readtext,
       purrr,
       readxl,
       pdftools)

gc()


### Reading In and Organizing RSS and Full Article Text Data from Previous Scripts ###


#Identify and read the text in the saved pdfs into the data frame so that the fully read articles can then be screened with second AOS search term filter
screened_articles <- read_csv("C:/AOS_db/data/02_rss_feed_screened_dfs.csv") %>%
  mutate(clean_title = tolower(title),
         clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = gsub("stat+|.pdf|", "", clean_title),
         clean_title = str_trim(clean_title))

#Titles require adequate cleaning (no punctuation, no capitals, no extra spaces, no source names in them) in order to join with the pdf file names
scraped_articles <- read_csv("C:/AOS_db/data/04_rss_feed_screened_read_articles_dfs.csv") %>%
  mutate(clean_title = tolower(title),
         clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = gsub("stat+|.pdf|", "", clean_title),
         clean_title = str_trim(clean_title)) %>%
  mutate(num_chars = nchar(url_text)) %>%
  filter(source != "Gov Info",
         num_chars > 150,
         !grepl("You have been blocked from The New York Times|Press & Hold to confirm you are|Something went wrong. Please try again later.|This site can’t be reached", url_text),
         URL != "https://washingtonpost.com") 

already_scraped_articles <- unique(scraped_articles$clean_title)

need_pdf <- screened_articles %>%
  filter(!title %in% scraped_articles$title)

articles_already_read_in <- read_csv("C:/AOS_db/data/06_screened_read_articles_complete.csv") %>%
  mutate(num_chars = nchar(url_text)) %>%
  filter(!grepl("You have been blocked from The New York Times|Press & Hold to confirm you are|Something went wrong. Please try again later|This site can’t be reached", url_text),
         source != "Gov Info",
         num_chars > 150,
         !is.na(url_text),
         URL != "https://washingtonpost.com") %>%
  select(-num_chars) %>%
  mutate(clean_title = tolower(title),
         clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = gsub("stat+|.pdf|", "", clean_title),
         clean_title = str_trim(clean_title))

articles_already_read_in_titles <- unique(articles_already_read_in$clean_title)

need_pdf <- filter(need_pdf, !clean_title %in% articles_already_read_in_titles)

pdf_folder <- list.files("C:/AOS_db/pdf_articles")

already_read_pdfs_df <- data.frame(article_names = pdf_folder, stringsAsFactors = FALSE)


#cleaning up PDF files and names
already_read_pdfs_titles <- already_read_pdfs_df %>%
  mutate(title = gsub("AP News|STAT+|.pdf", "", article_names),
         title = str_trim(title, side = "both"),
         clean_title = gsub("[[:punct:]]", "", title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = tolower(clean_title),
         clean_title = str_trim(clean_title))

#Remove pdfs with small file sizes, they are blank
setwd("C:/AOS_db/pdf_articles")

already_read_pdfs_size <- data.frame(stringsAsFactors = FALSE)

for(i in pdf_folder){
  info <- file.info(i)
  size_bytes <- info$size
  pdf_info_df_i <- data.frame(article_names = i, size = size_bytes)
  
  already_read_pdfs_size <- bind_rows(already_read_pdfs_size, pdf_info_df_i)
}

already_read_pdfs_size <- already_read_pdfs_size %>%
  filter(size > 94000)

already_read_pdfs_titles_df <- filter(already_read_pdfs_titles, article_names %in% already_read_pdfs_size$article_names)

already_read_pdfs_titles <- already_read_pdfs_titles_df$clean_title

need_to_save_pdfs <- need_pdf %>%
  filter(!clean_title %in% already_read_pdfs_titles)

write_csv(need_to_save_pdfs, "C:/AOS_db/data/06_screened_articles_still_need_pdf.csv")

need_pdf <- need_pdf %>%
  filter(clean_title %in% already_read_pdfs_titles)

screened_articles_w_pdf <- left_join(need_pdf, already_read_pdfs_titles_df, by = c("clean_title" = "clean_title")) %>%
  rename(title = title.y) %>%
  select(-title.x)

#Join matching pdf from the folder, pull in text into the url_text column for articles not scraped properly.
checking_4_read_all <- data.frame(stringsAsFactors = FALSE)

setwd("C:/AOS_db/pdf_articles")

ticker <- 0

for(i in 1:nrow(screened_articles_w_pdf)){ 
      checking_4_read_split <- screened_articles_w_pdf %>%
    slice(i)
  if(nrow(checking_4_read_split) > 0){
    pdf_to_read <- paste0("C:/AOS_db/pdf_articles/",checking_4_read_split$article_names)
    url_text_read <-  pdftools::pdf_text(pdf_to_read)
    url_text_read <- as.character(url_text_read) %>% 
      paste(., collapse = ". ") %>% 
      unlist() %>% 
      str_trim(., side = "both")
  
    checking_4_read_split <- mutate(checking_4_read_split, 
                                    url_text = url_text_read)
  
    checking_4_read_all <- bind_rows(checking_4_read_all, 
                                     checking_4_read_split)
    
    ticker <- ticker + 1
    print(ticker)
    Sys.sleep(5)
  }}


### Apply Search Terms to Screen Articles for Potential Attacks on Science ###


completed_saved_articles <- read_csv("C:/AOS_db/data/06_screened_read_articles_complete.csv")

screened_rss_feed_db_text <- bind_rows(completed_saved_articles, 
                                       checking_4_read_all, scraped_articles) %>% 
  unique() %>%
  mutate(num_chars = nchar(url_text)) %>%
  filter(!grepl("You have been blocked from The New York Times|Press & Hold to confirm you are|Something went wrong. Please try again later|This site can’t be reached", url_text),
         source != "Gov Info",
         num_chars > 150,
         !is.na(url_text))

screened_rss_feed_db_text <- screened_rss_feed_db_text %>%
  mutate(num_chars = nchar(url_text)) %>%
  group_by(pub_date, source) %>%
  slice_max(num_chars, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(title, description, pub_date, URL, source, description_original, url_text, clean_title) %>%
  distinct()

write_csv(screened_rss_feed_db_text, "C:/AOS_db/data/06_screened_read_articles_complete.csv")

#Read in search terms and create four categories
search_terms <- read_excel("C:/AOS_db/info_tables/Search Terms AOS.xlsx")

search_terms <- search_terms %>%
  mutate(search_term = tolower(search_term),
         search_term = ifelse(search_term_acronym == "yes", paste0(" ", search_term, " "), search_term))

#Break the search terms into four categories - government, topics, science, and negative verbs
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

#Add prefixes (anti, anti-) and suffixes (skeptic, denier) to science terms
suffix_terms <- search_terms %>%
  filter(category == "suffix")
suffix_terms <- suffix_terms$search_term

prefix_terms <- search_terms %>%
  filter(category == "prefix")
prefix_terms <- prefix_terms$search_term

prefix_topic <- outer(prefix_terms, topic_terms, paste) %>% c() %>% unique()
science_terms <- c(science_terms, prefix_topic)

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

#Screen full article text (or RSS descriptions/titles) using article text search term criteria
#Article text search term criteria = [government] AND [science] AND [topic] AND [negative verb]
gov_terms_db_text <- keep(screened_rss_feed_db_text$url_text, 
                          function(x) grepl(gov_terms, x))
science_terms_db_text <- keep(screened_rss_feed_db_text$url_text, 
                              function(x) grepl(science_terms, x))
attack_terms_db_text <- keep(screened_rss_feed_db_text$url_text, 
                             function(x) grepl(attack_terms, x))
topic_terms_db_text <- keep(screened_rss_feed_db_text$url_text, 
                            function(x) grepl(topic_terms, x))

aos_raw <- filter(screened_rss_feed_db_text, 
                            url_text %in% gov_terms_db_text,
                            url_text %in% science_terms_db_text,
                            url_text %in% attack_terms_db_text,
                            url_text %in% topic_terms_db_text)

#Create data frame to use in next R scripts
aos_raw <- aos_raw %>%
  filter(URL != "https://washingtonpost.com") %>%
  group_by(title, URL, source) %>%
  slice_max(order_by = nchar(url_text), with_ties = FALSE) %>%
  ungroup() %>%
  group_by(title, description, source) %>%
  slice_max(order_by = nchar(url_text), with_ties = FALSE) %>%
  ungroup() %>%
  distinct()

write_csv(aos_raw, "C:/AOS_db/data/06_aos_raw.csv")


#Create data frame of "screened out," fully scraped articles
screened_fully_read <- aos_raw
fully_read_articles <- screened_rss_feed_db_text

screened_out_fully_read <- fully_read_articles %>%
  filter(!title %in% screened_fully_read$title)

write_csv(screened_out_fully_read, "C:/AOS_db/data/06_aos_screenedout.csv")