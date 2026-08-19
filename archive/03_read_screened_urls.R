#### R SCRIPT PURPOSE: 
#### Pulls the text from RSS Feed articles whose descriptions contain key word(s).
#### Runs 1X/DAY

#### In order for this script to work: 
#### The user must have accounts and access to all news sources and be logged into each account.
#### The user must have Chrome installed since it opens a Chrome instance to read each article.

#### NOTES:
#### Takes about 20 minutes for 500 articles.
#### Counted 50 articles read in 4 minutes on 8/15/25

#### SOURCES: https://cran.r-project.org/web/packages/chromote/vignettes/example-custom-user-agent.html#:~:text=Synchronous%20version,screenshot(show%20=%20TRUE)%20%7D

###Output (from this script) deletion guidance -- This script runs weekly on Wednesday at 8AM Pacific time. The next script (04_) runs weekly on Thursday's at 8AM. This next script (04_) reads in the output from this script (03_), eliminates any articles that already exist using a mid point file save from the 05_ script, and then saves article websites as pdfs for the remaining articles. The rows in this file can be truncated after 05 completes its task of screening articles based on AND statements in the search term categories. Caution - If we change any search terms, this will need to be rerun. Right now (1/6/2026) we don't have a way to go back and pull old rss feed. For now we could truncate rows of this file right after 05_ is completed (starts on on Friday mornings at 8AM. Recommendation - Truncate all rows that are in the spreadsheet once 05_ has completed its final save and one week prior to the 03_run since 04_ picks that up Thursday am and if 03 still hasn't updated, we need to be able to have 04 grab a full 03 the following week. Run this truncation Saturday mornings at 8AM and take out rows from 2 weeks prior, just in case 03 is taking an extra long time.

##Filtering needs
#1. Take out any article that was already fully read in. DONE
#2. Take out any article that has a fully read in pdf DONE
#3. Take out any article that was fully scraped in by this script previously. DONE
#4. Take out any article that is screened out by human coding.
#5. Filter out duplicate articles, with a preference to the most number of characters read in (url_text), or the most recent pub_date for duplicate feeds. DONE


### Logistics for R Script ###


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
  group_by(title, source) %>%
  slice_max(., order_by = desc(pub_date), n = 1, with_ties = FALSE) %>% #sorting the data frame from newest to oldest) %>% #eliminating duplicates
  ungroup() %>%
  mutate(description_original = description, #new column; duplicating article description
         description = tolower(description)) %>% #changing article description to all lowercase
  filter(source != "Gov Info")

##Remove any articles to be scraped if they already have pdfs saved in hte pdf folder
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


unread_screened_feed_no_pdf <- unread_screened_feed %>%
  filter(!clean_title %in% already_read_pdfs_titles)

##Pull in and eliminate from the list any articles that were already scraped in the 03 scripts. Filter out articles that weren't read in properly and are under 150 characters
scraped_articles <- read_csv("C:/AOS_db/data/03_rss_feed_screened_read_articles_dfs.csv") %>%
  mutate(num_chars = nchar(url_text),
         title = gsub("- apnews.com|- AP News", "", title),
         description = gsub("- apnews.com|- ap news", "", description)) %>%
  unique() %>%
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

screened_feed_to_read <- unread_screened_feed_no_pdf %>%
  filter(!clean_title %in% already_scraped_articles)

### Writing Functions to Scrape Article Text ###


#creating new function to scrape web data
grab_text1 <- function(url_test) {
  #creates a new `ChromoteSession` object, which is an instance of a Chrome browser
  b <- ChromoteSession$new() 
  Sys.sleep(1) #pause execution for one second
  #tells Chrome exactly what type of single browser instance you want to search. 
  #It helps if you have different defaults from what is desired, or to control a mobile vs desktop environment, 
  #or to avoid having your site realize it’s a script navigating to their environment
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(1)
  #navigates to the assigned URL (in the data frame) based on the assignment of a Chrome browser with the specific user agent
  b$Page$navigate(url_test)
  Sys.sleep(1)
  #new function:Runtime$evaluate executes JavaScript in the Chrome session with the assigned user agent. 
  #‘document.querySelector('html').outerHTML’ selects the entire HTML document and returns its full source code as a string. 
  #$result$value extracts that string value from the R object 
  with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  Sys.sleep(1)
  #new function: reads only the paragraph nodes from the html from the Chrome session, 
  #reformats to a character and collapses the lists of characters into one character separated by “. “ (to make sentences) 
  url_test <- read_html(with_user_agent) %>%
    html_nodes("p") %>% #select specific website html code labeled as "p"
    html_text() %>% #retrieve text from specified html code
    as.character() %>% #ensure it's in character format
    paste(., collapse = ". ") #put all scraped html text into one data point per article
  b$close(wait = FALSE) #closes the Chrome session that used the specified user agent
  url_test #returns desired text
}

#creating new function to scrape web data
grab_text2 <- function(url_test) {
  #creates a new `ChromoteSession` object, which is an instance of a Chrome browser
  b <- ChromoteSession$new()
  Sys.sleep(1) #pause execution for one second
  #tells Chrome exactly what type of single browser instance you want to search. 
  #It helps if you have different defaults from what is desired, or to control a mobile vs desktop environment, 
  #or to avoid having your site realize it’s a script navigating to their environment
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(1)
  #navigates to the assigned URL (in the data frame) based on the assignment of a Chrome browser with the specific user agent
  b$Page$navigate(url_test)
  Sys.sleep(1)
  #new function:Runtime$evaluate executes JavaScript in the Chrome session with the assigned user agent. 
  #‘document.querySelector('html').outerHTML’ selects the entire HTML document and returns its full source code as a string. 
  #$result$value extracts that string value from the R object 
  with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  Sys.sleep(1)
  #new function: reads only the paragraph nodes from the html from the Chrome session, 
  #reformats to a character and collapses the lists of characters into one character separated by “. “ (to make sentences)  
  url_test <- read_html(with_user_agent) %>% 
    html_node("body") %>%  #select specific website html code sections labeled as "body"
    html_elements("p") %>% #select specific website html code labeled as "p"
    html_text() %>% #retrieve text from specified html code
    as.character() %>% #ensure it's in character format
    paste(., collapse = ". ") #put all scraped html text into one data point per article
  b$close(wait = FALSE) #closes the Chrome session that used the specified user agent
  url_test #returns desired text
}

#creating a shell to put scraped data into
screened_rss_feed_db_all <- data.frame()

#creating a label to track the number of articles scraped
ticker <- 0


### Scraping Web Text of New RSS Feed Articles from Last Three Days ###


#new loop:
for(i in 1:nrow(screened_feed_to_read)){ #for each row in data frame of RSS Feed articles from past three days
  screened_rss_feed_db_split <- slice(screened_feed_to_read, i) #put each row into a new data frame
  #only for articles from WAPO, The Hill, AP, or EE
  if(screened_rss_feed_db_split$source %in% c("Washington Post", "The Hill", "Associated Press", "E&E News")){
    #and scrape article text using grab_text1
    screened_rss_feed_db_split <- mutate(screened_rss_feed_db_split, 
                                         url_text = map(URL, possibly(grab_text1)))
  }
  #if grab_text1 doesn't work, run grab_text2 
  else{screened_rss_feed_db_split <- mutate(screened_rss_feed_db_split, url_text = map(URL, possibly(grab_text2)))}
  #combine all rows of data
  screened_rss_feed_db_all <- bind_rows(screened_rss_feed_db_split, screened_rss_feed_db_all)
  #keep track of the number of articles successfully scraped
  ticker <- ticker + 1
  print(ticker)
  Sys.sleep(1)
}

#new data frame
screened_rss_feed_db_text <- screened_rss_feed_db_all %>%
  rowwise() %>% #for each row
  #change scraped URL text into a vector
  mutate(url_text = unlist(paste(as.character(url_text), collapse = ". ")),
         #if web scrape was unsuccessful or URL text is missing, fill in with RSS Feed article description
         url_text = ifelse(is.na(url_text), description_original, url_text),
         url_text = ifelse(is.null(url_text), description_original, url_text))


### Getting Data Ready for Next Script in Sequence (04) ###


#combine previously attempted scraped URL and the newly scraped URLs
screened_rss_feed_db_text <- bind_rows(already_read_urls, screened_rss_feed_db_text) %>% 
  group_by(title, URL, source) %>%
  slice_max(., order_by = desc(pub_date), n = 1, with_ties = FALSE) %>%#id'ing duplicates in these columns
  ungroup() %>%
  unique() #eliminate duplicates

#creating csv file with most up to date scraped URL text
write.csv(screened_rss_feed_db_text, "C:/AOS_db/data/03_rss_feed_screened_read_articles_dfs.csv")