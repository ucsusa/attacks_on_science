#### R SCRIPT PURPOSE: 
#### Pulls in read URLs from both scraping and pdf saving and filters the
#### Full text articles for all of the search terms using the "AND" clause
#### Runs 1X/WEEK

##Filtering needs
# 1. Filter out any articles that have been screened out by human coders.
# 2. Filter out any articles that have already been screened by this script previously.
# 3. Filter out any duplicates with a preference for most number of characters read in (url_text), or rss feed with the most recent pub_date. DONE
# 4. Screen out pdfs that have already been read in using this script.

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
       readxl,
       pdftools) #to automate/loop specific functions

#garbage collection; removing things from memory that are no longer in use
gc()


### Identify and read in the pdfs of full articles that have been saved, eliminate the articles that have been fully scraped, bind all the articles that are fully read in an screened them for the search terms as categories ###
screened_articles <- read_csv("C:/AOS_db/data/02_rss_feed_screened_dfs.csv") %>%
  group_by(title, URL, source) %>%
  arrange(desc(pub_date)) %>%#id'ing duplicates in these columns
  slice(1) %>% #eliminating duplicates
  ungroup()  %>%
  mutate(clean_title = str_replace_all(title, ",", ""),
         clean_title = gsub("-", " ", clean_title),
         clean_title = gsub("The New York Times|The Washington Post|Washington Post|E&E News|POLITICO Pro _ Article _|The White House|_ STAT|EHN| - |.pdf|$| _ article _ |:|_|//,|//-|//'", "", clean_title),
         clean_title = str_replace_all(clean_title, "[^[:alnum:]///' ]", ""),
         clean_title = tolower(clean_title),
         clean_title = gsub("article", "", clean_title),
         clean_title = gsub("america's", "americas", clean_title),
         clean_title = str_trim(clean_title))
  

#read in the dataframe with all of the properly scraped articles.
scraped_articles <- read_csv("C:/AOS_db/data/03_rss_feed_screened_read_articles_dfs.csv") %>%
  mutate(clean_title = str_replace_all(title, ",", ""),
         clean_title = gsub("-", " ", clean_title),
         clean_title = gsub("The New York Times|The Washington Post|Washington Post|E&E News|POLITICO Pro _ Article _|The White House|_ STAT|EHN| - |.pdf|$| _ article _ |:|_|//,|//-|//'", "", clean_title),
         clean_title = str_replace_all(clean_title, "[^[:alnum:]///' ]", ""),
         clean_title = tolower(clean_title),
         clean_title = gsub("article", "", clean_title),
         clean_title = gsub("america's", "americas", clean_title),
         clean_title = str_trim(clean_title)) %>%
  mutate(num_chars = nchar(url_text)) %>%
  filter(source != "Gov Info",
         num_chars > 150,
         !grepl("You have been blocked from The New York Times|Press & Hold to confirm you are|Something went wrong. Please try again later.|This site can’t be reached", url_text)) 

#Filter duplicates to articles with highest character url_text, or read in articles
scraped_articles <- scraped_articles %>%
  group_by(pub_date, title, source) %>%
  slice_max(num_chars, n = 1) %>%
  ungroup() %>%
  unique() %>%
  select(-num_chars)

already_scraped_articles <- unique(scraped_articles$clean_title)

##Reduce the screened articles by those that were fully scraped
need_pdf <- screened_articles %>%
  filter(!title %in% scraped_articles$title)

##Eliminate pdfs to be read in that have already been read in fully.
articles_already_read_in <- read_csv("C:/AOS_db/data/05_screened_read_articles_complete.csv") %>%
  mutate(num_chars = nchar(url_text)) %>%
  filter(!grepl("You have been blocked from The New York Times|Press & Hold to confirm you are|Something went wrong. Please try again later|This site can’t be reached", url_text),
         source != "Gov Info",
         num_chars > 150,
         !is.na(url_text)) %>%
  select(-num_chars) %>%
  mutate(clean_title = str_replace_all(title, ",", ""),
         clean_title = gsub("-", " ", clean_title),
         clean_title = gsub("The New York Times|The Washington Post|Washington Post|E&E News|POLITICO Pro _ Article _|The White House|_ STAT|EHN| - |.pdf|$| _ article _ |:|_|//,|//-|//'", "", clean_title),
         clean_title = str_replace_all(clean_title, "[^[:alnum:]///' ]", ""),
         clean_title = tolower(clean_title),
         clean_title = gsub("article", "", clean_title),
         clean_title = gsub("america's", "americas", clean_title),
         clean_title = str_trim(clean_title))

articles_already_read_in_titles <- unique(articles_already_read_in$clean_title)

need_pdf <- filter(need_pdf, !clean_title %in% articles_already_read_in_titles)

##Remove any pdfs in the pdf folder that are too small and therefore likely blank.
pdf_folder <- list.files("C:/AOS_db/pdf_articles")

already_read_pdfs_df <- data.frame(article_names = pdf_folder, stringsAsFactors = FALSE)

already_read_pdfs_titles <- already_read_pdfs_df %>%
  mutate(title = str_replace_all(article_names, ",", ""),
         title = gsub("-", " ", title),
         title = gsub("The New York Times|The Washington Post|Washington Post|E&E News|POLITICO Pro _ Article _|The White House|_ STAT| - |.pdf|$| _ article _ |:|_|//,|//-|//'", "", title),
         title = str_replace_all(title, "[^[:alnum:]///' ]", ""),
         title = tolower(title),
         title = gsub("reuters|article  ", "", title),
         title = gsub("america's", "americas", title),
         title = str_trim(title))

##Remove any articles to be scraped if they already have pdfs saved in the pdf folder
pdf_folder <- list.files("C:/AOS_db/pdf_articles")

already_read_pdfs_df <- data.frame(article_names = pdf_folder, stringsAsFactors = FALSE)

already_read_pdfs_titles <- already_read_pdfs_df %>%
  mutate(title = str_replace_all(article_names, ",", ""),
         title = gsub("-", " ", title),
         title = gsub("The New York Times|The Washington Post|Washington Post|E&E News|POLITICO Pro _ Article _|The White House|_ STAT| - |.pdf|$| _ article _ |:|_|//,|//-|//'", "", title),
         title = str_replace_all(title, "[^[:alnum:]///' ]", ""),
         title = tolower(title),
         title = gsub("reuters|article  ", "", title),
         title = gsub("america's", "americas", title),
         title = str_trim(title))

##Remove pdfs with small file sizes, they are blank
##Set working directory to pull in file size
setwd("C:/AOS_db/pdf_articles")

already_read_pdfs_size <- data.frame(stringsAsFactors = FALSE)

for(i in pdf_folder){
  # Get file information
  info <- file.info(i)
  # Extract and print the size in bytes
  size_bytes <- info$size
  pdf_info_df_i <- data.frame(article_names = i, size = size_bytes)
  
  already_read_pdfs_size <- bind_rows(already_read_pdfs_size, pdf_info_df_i)
}

already_read_pdfs_size <- already_read_pdfs_size %>%
  filter(size > 94000)

already_read_pdfs_titles_df <- filter(already_read_pdfs_titles, article_names %in% already_read_pdfs_size$article_names)

already_read_pdfs_titles <- already_read_pdfs_titles_df$title

##Save a csv of articles that need to be read in as pdfs and haven't been yet, or are not joining correctly.

need_to_save_pdfs <- need_pdf %>%
  filter(!clean_title %in% already_read_pdfs_titles)

write.csv(need_to_save_pdfs, "C:/AOS_db/data/05_screened_articles_still_need_pdf.csv")

##Narrow does the list of pdfs to be read in where there are actually pdfs saved in the pdf folder.
need_pdf <- need_pdf %>%
  filter(clean_title %in% already_read_pdfs_titles)

screened_articles_w_pdf <- left_join(need_pdf, already_read_pdfs_titles_df, by = c("clean_title" = "title"))

### Write Functions and Create Structures to Scrape URL PDFs ###

                                           
#creating a shell to put scraped pdf data into
checking_4_read_all <- data.frame(stringsAsFactors = FALSE)

#set working directory to make sure pdfs are being pulled and placed into correct place
setwd("C:/AOS_db/pdf_articles")

#creating a label to track the number of articles scraped
ticker <- 0

#new loop to read in pdf text data
for(i in 1:nrow(screened_articles_w_pdf)){ #for each article that needs pdf text
  #put one row of data (or one article) into split data set at a time
  checking_4_read_split <- screened_articles_w_pdf %>%
    slice(i)
  #paste file name for each row that needs pdf text into new data set
  pdf_to_read <- paste0("C:/AOS_db/pdf_articles/",checking_4_read_split$article_names)
  #extract text from each pdf file
    url_text_read <-  pdftools::pdf_text(pdf_to_read)
    #ensure pdf text are characters and clean up format (trim blank space, etc.)
    url_text_read <- as.character(url_text_read) %>% 
      paste(., collapse = ". ") %>% 
      unlist() %>% 
      str_trim(., side = "both")
    #in split data set, copy and paste pdf text into url_text column
    checking_4_read_split <- mutate(checking_4_read_split, 
                                    url_text = url_text_read)
    #bind these rows together in the bigger dataset
    checking_4_read_all <- bind_rows(checking_4_read_all, 
                                     checking_4_read_split)
    #keep track of the number of pdfs successfully scraped
    ticker <- ticker + 1
    print(ticker)
    Sys.sleep(60)
    }


### Use Search Terms to Narrow Down Articles by URL Text ###


#gather the full spreadsheet with accurately read articles (scraped or pdf)

#new csv file: contains completed articles (either scraped or read pdf)
completed_saved_articles <- read_csv("C:/AOS_db/data/05_screened_read_articles_complete.csv")

screened_rss_feed_db_text <- bind_rows(completed_saved_articles, 
                                       checking_4_read_all, scraped_articles) %>% 
  unique() %>%
  mutate(num_chars = nchar(url_text)) %>%
  filter(!grepl("You have been blocked from The New York Times|Press & Hold to confirm you are|Something went wrong. Please try again later|This site can’t be reached", url_text),
         source != "Gov Info",
         num_chars > 150,
         !is.na(url_text))

#Filter duplicates to articles with highest character url_text, or read in articles
screened_rss_feed_db_text <- screened_rss_feed_db_text %>%
  mutate(num_chars = nchar(url_text)) %>%
  group_by(pub_date, title, source) %>%
  slice_max(num_chars, n = 1) %>%
  ungroup() %>%
  select(title, description, pub_date, URL, source, description_original, url_text, clean_title) %>%
  unique()

#write new csv with full spreadsheet of scraped and pdf-read articles
write_csv(screened_rss_feed_db_text, "C:/AOS_db/data/05_screened_read_articles_complete.csv")


#grab all of the search terms
search_terms <- read_excel("C:/AOS_db/info_tables/Search Terms AOS.xlsx")

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
prefix_topic <- outer(prefix_terms, topic_terms, paste) %>% c() %>% unique()
science_terms <- c(science_terms, prefix_topic)

##Make the anti topics terms from the prefixes and add to science terms
prefix_topic_hyphen <- outer(prefix_terms, topic_terms, paste0) %>% c() %>% unique()
science_terms <- c(science_terms, prefix_topic_hyphen)

##Make the skeptic and denier topics terms from the suffixes and add to science terms
topic_suffix <- outer(topic_terms, suffix_terms, paste) %>% c() %>% unique()
science_terms <- c(science_terms, topic_suffix)

##Collapse all search terms
science_terms <- paste0(science_terms,  collapse = "|")
topic_terms <- paste0(topic_terms,  collapse = "|")
gov_terms <- paste0(gov_terms,  collapse = "|")
attack_terms <- paste0(attack_terms,  collapse = "|")

#new data set: articles whose text contains at least one word of each category
#searching URL text for words that belong in the four categories
gov_terms_db_text <- keep(screened_rss_feed_db_text$url_text, 
                          function(x) grepl(gov_terms, x))
science_terms_db_text <- keep(screened_rss_feed_db_text$url_text, 
                              function(x) grepl(science_terms, x))
attack_terms_db_text <- keep(screened_rss_feed_db_text$url_text, 
                             function(x) grepl(attack_terms, x))
topic_terms_db_text <- keep(screened_rss_feed_db_text$url_text, 
                            function(x) grepl(topic_terms, x))

#new data set: articles whose text contains at least one word of each category
aos_raw <- filter(screened_rss_feed_db_text, 
                            url_text %in% gov_terms_db_text,
                            url_text %in% science_terms_db_text,
                            url_text %in% attack_terms_db_text,
                            url_text %in% topic_terms_db_text)

### Get Data Ready for Script 06 ###

#Create new CSV with above data

write_csv(aos_raw, "C:/AOS_db/data/05_aos_raw.csv")


###Create a data frame of articles that are screened out and were read in fully.

screened_fully_read <- aos_raw
fully_read_articles <- screened_rss_feed_db_text

screened_out_fully_read <- fully_read_articles %>%
  filter(!title %in% screened_fully_read$title)

write_csv(screened_out_fully_read, "C:/AOS_db/data/05_aos_screenedout.csv")
