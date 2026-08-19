#### R SCRIPT PURPOSE: 
#### Filters out unread/misread articles and saves pdfs in a folder for the misread/unread articles.
#### Runs 1X/DAY

##Filtering needs
#1. Filter out duplicate articles, with a preference to the most number of characters read in or in url_text or most recent pub_date. DONE
#2. Filter out articles that already have full pdfs read in. DONE
#3. Filter out articles that were fully scraped. DONE
#4. Filter out articles that were screened out with human coding.
#5. Filter out articles that were screened out with search terms AND were fully read in.



### Logistics for R Script ###


#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data 
       pdftools,
       chromote) #to work with, create, etc. pdf documents

#garbage collection; removing things from memory that are no longer in use
gc()


### Import and Organize Data from R Script 03 ###


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
  group_by(title, URL, pub_date) %>%
  slice_max(., order_by = desc(pub_date), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
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

screened_articles_no_pdf <- unread_screened_feed_no_pdf %>%
  filter(!clean_title %in% already_scraped_articles)

#create item where downloaded pdfs live
pdf_folder <- "C:/AOS_db/pdf_articles/"


### Write Functions to Scrape URL PDFs ###

#new function: replace punctuation from pdf file names with _
sanitize_filename <- function(x) {
  str_replace_all(x, '[\\\\/:*?"<>|]', "_")
}

#item to start chrome session 
b <- ChromoteSession$new()

#new function to pull text from PDFs
save_pdf_2 <- function(x) {
  #new variable: identify URLs that need to be supplemented by pdfs
  link <- screened_articles_no_pdf$URL[x]
  #new variable: paste pdf title name + .pdf with punctuation changed for "_"
  filename <- paste0(pdf_folder, 
                     sanitize_filename(screened_articles_no_pdf$title[x]), 
                     ".pdf")
  #to catch errors, warnings, etc.
  tryCatch({
    #open each article URL
    b$Page$navigate(link)
    #pause R for 10 seconds while the page loads
    Sys.sleep(60) 
    #save the URL page as PDF
    pdf_data <- b$Page$printToPDF(printBackground = TRUE)
    #printtoPDF returns base64 strings, convert back to binary so it's readable
    writeBin(base64enc::base64decode(pdf_data$data), filename)
    return(filename)
  }, error = function(e) {
    return(NA)
  })
Sys.sleep(60)}


#pull article text using save_pdf function for each row until complete
results <- map_chr(1:nrow(screened_articles_no_pdf), save_pdf_2)

#Log successes/failures
log <- data.frame(
  HEADLINE = screened_articles_no_pdf$title,
  LINK = screened_articles_no_pdf$URL,
  PDF_saved = !is.na(results),
  Saved_name = results
)

#close chrome session
b$close()
