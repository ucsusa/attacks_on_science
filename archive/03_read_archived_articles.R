##This script reads in any articles on archive.ph (an internet archive)

##Filtering needs:
#1. any articles already read fully (take out very small pdfs that weren're read in properly and retry) DONE
#2. Filter out any articles that were screened out in human coding.
#3. Lingering articles from Gov Info. DONE
#4. Filter out duplicate articles, with a preference to the most recent pub_date. DONE
#5. Filter out articles that were screened out with search terms AND were fully read in.



#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data
       janitor, #to make cleaning and organizing noisy data easier
       purrr, #to automate/loop specific functions
       rvest, #to extract, wrangle, and clean web data
       R.utils, #expanded utility functions
       polite, #web scraping using "polite" principles
       httr, #to pull and organize HTTP content from news websites
       chromote) #to facilitate web scraping/interactions on Chrome

#garbage collection; removing things from memory that are no longer in use
gc()

### Import and Organize Data from R Script 02 ###

#importing all RSS Feed articles that have been screened with key words
unread_screened_feed <- read_csv("C:/AOS_db/data/02_rss_feed_screened_dfs.csv") %>%
  mutate(title = gsub("- apnews.com|- AP News", "", title),
         description = gsub("- apnews.com|- ap news", "", description)) %>%
  group_by(title, URL, source) %>%
  slice_max(., order_by = desc(pub_date), n = 1, with_ties = FALSE) %>% #id'ing duplicates in these columns
  ungroup() %>%
  mutate(description_original = description, #new column; duplicating article description
         description = tolower(description)) %>% #changing article description to all lowercase
  filter(source != "Gov Info")

#remove articles that already have pdfs from the list to attempt

pdf_folder <- list.files("C:/AOS_db/pdf_articles/")

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

already_read_pdfs_titles <- filter(already_read_pdfs_titles, article_names %in% already_read_pdfs_size$article_names)

already_read_pdfs_titles <- already_read_pdfs_titles$title

unread_screened_feed <- unread_screened_feed %>%
  mutate(clean_title = str_replace_all(title, ",", ""),
         clean_title = gsub("-", " ", clean_title),
         clean_title = gsub("The New York Times|The Washington Post|Washington Post|E&E News|POLITICO Pro _ Article _|The White House|_ STAT|EHN| - |.pdf|$| _ article _ |:|_|//,|//-|//'", "", clean_title),
         clean_title = str_replace_all(clean_title, "[^[:alnum:]///' ]", ""),
         clean_title = tolower(clean_title),
         clean_title = gsub("article", "", clean_title),
         clean_title = gsub("america's", "americas", clean_title),
         clean_title = str_trim(clean_title))


unread_screened_feed_need_pdf <- unread_screened_feed %>%
  filter(!clean_title %in% already_read_pdfs_titles)

##Pull in and eliminate from the list any articles that were already scraped in the 04 scripts. Filter out articles that weren't read in properly and are under 150 characters
scraped_articles <- read_csv("C:/AOS_db/data/04_rss_feed_screened_read_articles_dfs.csv") %>%
  select(title, description, pub_date, URL, source, description_original, url_text) %>%
  unique() %>%
  mutate(num_chars = nchar(url_text),
         title = gsub("- apnews.com|- AP News", "", title),
         description = gsub("- apnews.com|- ap news", "", description)) %>%
  filter(!grepl("You have been blocked from The New York Times|Press & Hold to confirm you are|Something went wrong. Please try again later.", url_text),
         source != "Gov Info",
         num_chars > 150) %>%
  mutate(clean_title = str_replace_all(title, ",", ""),
         clean_title = gsub("-", " ", clean_title),
         clean_title = gsub("The New York Times|The Washington Post|Washington Post|E&E News|POLITICO Pro _ Article _|The White House|_ STAT|EHN| - |.pdf|$| _ article _ |:|_|//,|//-|//'", "", clean_title),
         clean_title = str_replace_all(clean_title, "[^[:alnum:]///' ]", ""),
         clean_title = tolower(clean_title),
         clean_title = gsub("article", "", clean_title),
         clean_title = gsub("america's", "americas", clean_title),
         clean_title = str_trim(clean_title))

already_scraped_articles <- unique(scraped_articles$clean_title)

unread_screened_feed_need_pdf <- unread_screened_feed_need_pdf %>%
  filter(!clean_title %in% already_scraped_articles)

##Eliminate Gov Exec and Stateline from the automated search since rss feeds have the full text included, and that is the description now.
unread_screened_feed_need_pdf <- unread_screened_feed_need_pdf %>%
  filter(!source %in% c("Gov Exec", "Stateline Democracy"))


##Folder where pdfs are saved

pdf_file_folder <- "C:/AOS_db/pdf_articles/"

##Needed functions
sanitize_filename <- function(x) {
  str_replace_all(x, '[\\\\/:*?"<>|]', "_")
}

#grab_text0 <- function(x) {
for(i in 1:nrow(unread_screened_feed_need_pdf)){ 
  url_test <- unread_screened_feed_need_pdf$URL[i]
  filename <- paste0(pdf_file_folder, sanitize_filename(unread_screened_feed_need_pdf$title[i]), ".pdf")
  tryCatch({
  b <- ChromoteSession$new()
  Sys.sleep(2)
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36")
  Sys.sleep(3)
      b$Page$navigate(paste0("https://archive.is/", url_test))
    target_selector <- "div.THUMBS-BLOCK"
    Sys.sleep(4)
    with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
    Sys.sleep(6)
    url_test <- read_html(with_user_agent) %>% html_elements("div.THUMBS-BLOCK") %>% html_nodes("a") %>% html_attr("href")
    if(nzchar(url_test[1])){
      url_test <- url_test[1]
      c <- ChromoteSession$new()
      Sys.sleep(2)
      c$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36")
      Sys.sleep(3)
      c$Page$navigate(url_test)
      Sys.sleep(4)
      with_user_agent <- c$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
      Sys.sleep(5)
      pdf_data <- c$Page$printToPDF(printBackground = TRUE)
      writeBin(base64enc::base64decode(pdf_data$data), filename)
      
      b$close(wait = FALSE)
      b$close(wait = FALSE)
      
      c$close(wait = FALSE)
      c$close(wait = FALSE)
      url_test}
    
    return(filename)
  }, error = function(e) {
    return(NA)
  })
  Sys.sleep(20)
  print(i)
}

