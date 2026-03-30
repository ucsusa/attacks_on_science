#### R SCRIPT PURPOSE: 
#### Pulls the text from RSS Feed articles whose descriptions contain key word(s) and reads in the full text.
#### Runs weekly

#### The user must have Chrome installed since it opens a Chrome instance to read each article.

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse, 
       janitor, 
       purrr, 
       rvest, 
       R.utils,
       keyring, 
       polite, 
       httr, 
       chromote,
       readxl)

gc()


unread_screened_feed <- read_csv("C:/AOS_db/data/02_rss_feed_screened_dfs.csv")

##Create df of urls from Gov Exec and Stateline Democracy to add back in at the end since they already have full text in the rss feed.
govex_sl <- unread_screened_feed %>%
  filter(source %in% c("Gov Exec", "Stateline Democracy")) %>%
  mutate(url_text = description)


##Remove any articles to be scraped if they already have pdfs saved in the pdf folder
pdf_folder <- list.files("C:/AOS_db/pdf_articles")

already_read_pdfs_df <- data.frame(article_names = pdf_folder, stringsAsFactors = FALSE)

already_read_pdfs_titles <- already_read_pdfs_df %>%
  mutate(title = gsub("AP News|STAT+|.pdf", "", article_names),
         title = str_trim(title, side = "both"),
         clean_title = gsub("[[:punct:]]", "", title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = tolower(clean_title),
         clean_title = str_trim(clean_title))

##Remove pdfs with small file sizes, they are blank
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

already_read_pdfs_titles <- filter(already_read_pdfs_titles, article_names %in% already_read_pdfs_size$article_names)

already_read_pdfs_clean_titles <- already_read_pdfs_titles$clean_title

already_read_pdfs_titles <- already_read_pdfs_titles$title

unread_screened_feed <- unread_screened_feed %>%
  mutate(clean_title = tolower(title),
         clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = gsub("stat+|.pdf|", "", clean_title),
         clean_title = str_trim(clean_title))

unread_screened_feed_no_pdf <- unread_screened_feed %>%
  filter(!clean_title %in% already_read_pdfs_clean_titles,
         !title_original %in% already_read_pdfs_titles)

##Pull in and eliminate from the list any articles that were already scraped in the 04 scripts. Filter out articles that weren't read in fully (generally under 150 characters).
scraped_articles <- read_csv("C:/AOS_db/data/04_rss_feed_screened_read_articles_dfs.csv") %>%
  group_by(title_original, description, URL, source) %>%
  slice_max(., order_by = pub_date) %>%
  ungroup() %>%
  select(title, description, pub_date, URL, source, description_original, url_text, title_original) %>%
  unique() %>%
  mutate(num_chars = nchar(url_text)) %>%
  unique() %>%
  filter(!grepl("You have been blocked from The New York Times|Press & Hold to confirm you are|Something went wrong. Please try again later.", url_text),
         source != "Gov Info",
         num_chars > 150,
         URL != "https://washingtonpost.com") %>%
  mutate(clean_title = tolower(title_original),
         clean_title = gsub("stat+|ap news|pdf|", "", clean_title),
         clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = str_trim(clean_title))

already_scraped_articles <- unique(scraped_articles$clean_title)


screened_feed_to_read <- unread_screened_feed_no_pdf %>%
  filter(!clean_title %in% already_scraped_articles)

##Eliminate Gov Exec and Stateline from the automated search since rss feeds have the full text included, and that is the description now.
screened_feed_to_read <- screened_feed_to_read %>%
  filter(!source %in% c("Gov Exec", "Stateline Democracy"))

##Eliminate articles already screened out by human coding
human_coding_spreadsheet <- read_excel("C:/AOS_db/data/11_coding_spreadsheet.xlsx") %>%
  mutate(clean_title = tolower(HEADLINE),
         clean_title = gsub("stat+|ap news|pdf|", "", clean_title),
         clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = str_trim(clean_title))


human_coded_titles <- unique(human_coding_spreadsheet$HEADLINE)
human_coded_clean_titles <- unique(human_coding_spreadsheet$clean_title)

screened_feed_to_read <- screened_feed_to_read %>%
  filter(!title_original %in% human_coded_titles,
         !clean_title %in% human_coded_clean_titles)

write_csv(screened_feed_to_read, "C:/AOS_db/data/04_screened_feed_to_read.csv")

already_read_urls <- unread_screened_feed %>%
  filter(!title %in% screened_feed_to_read$title)

#Read in full articles from E&E News
grab_text1 <- function(url_test) {
  try({login_url_test <- paste0("https://login.politicopro.com/?redirect=", url_test, "&s=eenews")
  b <- read_html_live(login_url_test)
  Sys.sleep(4)
  b$session$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(4)
  b$type("#email", key_list(service = "eandenews")$username)
  Sys.sleep(4)
  b$type("#password", key_get(service = "eandenews", key_list(service = "eandenews")$username))
  b$click("#pro > div > div > div.page__form > div > form > fieldset > div.form-section.button")
  Sys.sleep(4)
  with_user_agent <- b$session$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  Sys.sleep(4)
  url_test <- read_html(with_user_agent) %>%
    html_nodes("p") %>% #select specific website html code labeled as "p"
    html_text() %>% 
    as.character() %>% 
    paste(., collapse = ". ") 
  b$session$close(wait = FALSE) 
  url_test
})
  b <- ChromoteSession$new() 
  Sys.sleep(4) 
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(4)
  b$Page$navigate(url_test)
  Sys.sleep(4)
  with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  Sys.sleep(4)
  url_test <- read_html(with_user_agent) %>%
    html_nodes("p") %>% 
    html_text() %>% 
    as.character() %>% 
    paste(., collapse = ". ") 
  b$close(wait = FALSE) 
  url_test 
}

##Read STAT+ articles
grab_text2 <- function(url_test) {
  try({stat_login <- "https://www.statnews.com/login/"
    b <- read_html_live(stat_login)
  Sys.sleep(4)
  b$session$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(4)
  b$type("#login-email", key_list(service = "statnews")$username)
  Sys.sleep(4)
  b$type("#login-password", key_get(service = "statnews", key_list(service = "statnews")$username))
  b$click("#login > form:nth-child(2) > div:nth-child(3) > input")
  Sys.sleep(4)
  
  b$session$close(wait = FALSE)
  })
  b <- ChromoteSession$new() 
  Sys.sleep(4)  
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(4)
  b$Page$navigate(url_test)
  Sys.sleep(4)
  with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  Sys.sleep(4)
  url_test <- read_html(with_user_agent) %>%
    html_nodes("p") %>% 
    html_text() %>% 
    as.character() %>% 
    paste(., collapse = ". ") 
  b$close(wait = FALSE) 
  url_test 
  }

#Save The Hill articles as pdfs
grab_text3 <- function(url_test) {
  b <- ChromoteSession$new() 
  Sys.sleep(4)  
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(4)
  b$Page$navigate(url_test)
  Sys.sleep(4)
  with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  Sys.sleep(4)
  pdf_data <- b$Page$printToPDF(printBackground = TRUE)
  
  filename <- paste0("C:/AOS_db/pdf_articles/", 
                     screened_rss_feed_db_split$title, 
                     ".pdf")
  
  writeBin(base64enc::base64decode(pdf_data$data), filename)
  
}

#Read in articles from all other sources.
grab_text4 <- function(url_test) {
  b <- ChromoteSession$new()
  Sys.sleep(4)
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(4)
  b$Page$navigate(url_test)
  Sys.sleep(4)
  with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  Sys.sleep(4)
  url_test <- read_html(with_user_agent) %>% 
    html_node("body") %>%
    html_elements("p") %>%
    html_text() %>%
    as.character() %>%
    paste(., collapse = ". ")
  b$close(wait = FALSE)
  url_test
}


screened_rss_feed_db_all <- data.frame()

ticker <- 0

for(i in 1:nrow(screened_feed_to_read)){ 
  screened_rss_feed_db_split <- slice(screened_feed_to_read, i) 
  if (screened_rss_feed_db_split$source == "E&E News") {
    screened_rss_feed_db_split <- mutate(
      screened_rss_feed_db_split,                                         url_text = map(URL, possibly(grab_text1)))
  } else if(screened_rss_feed_db_split$source == "Stat News") {screened_rss_feed_db_split <- mutate(screened_rss_feed_db_split, url_text = map(URL, possibly(grab_text2)))
  } else if(screened_rss_feed_db_split$source == "The Hill"){screened_rss_feed_db_split <- mutate(screened_rss_feed_db_split, url_text = map(URL, possibly(grab_text3)))} else{screened_rss_feed_db_split <- mutate(screened_rss_feed_db_split, url_text = map(URL, possibly(grab_text4)))}
  
  screened_rss_feed_db_all <- bind_rows(screened_rss_feed_db_split, screened_rss_feed_db_all)
  
  ticker <- ticker + 1
  print(ticker)
  Sys.sleep(1)
}

screened_rss_feed_db_text <- screened_rss_feed_db_all %>%
  rowwise() %>%
  mutate(url_text = unlist(paste(as.character(url_text), collapse = ". ")),
         url_text = ifelse(is.na(url_text), description_original, url_text),
         url_text = ifelse(is.null(url_text), description_original, url_text))


#Combine previously attempted scraped URL and the newly scraped URLs
screened_rss_feed_db_text <- bind_rows(already_read_urls, screened_rss_feed_db_text) %>% 
  group_by(title, URL, source) %>%
  slice_max(., order_by = desc(pub_date), n = 1, with_ties = FALSE) %>%
  group_by(description, URL, source) %>%
  slice_max(., order_by = desc(pub_date), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  unique() %>%
  filter(source != "Gov Info",
         URL != "https://washingtonpost.com")

##Add stateline and gov exec back in
screened_rss_feed_db_text <- bind_rows(screened_rss_feed_db_text, govex_sl) %>%
  distinct()

#Creating csv file with all scraped URL text
write_csv(screened_rss_feed_db_text, "C:/AOS_db/data/04_rss_feed_screened_read_articles_dfs.csv")
