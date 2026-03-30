#### R SCRIPT PURPOSE: 
#### Filters out unread/misread articles and saves pdfs in a folder for the misread/unread articles.
#### Runs 1X/DAY

#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data 
       pdftools,
       chromote,
       readxl) #to work with, create, etc. pdf documents

#garbage collection; removing things from memory that are no longer in use
gc()


### Import and Organize Data from R Script 03 ###


#importing all RSS Feed articles that have been screened with key words
unread_screened_feed <- read_csv("C:/AOS_db/data/02_rss_feed_screened_dfs.csv")

##Eliminate Gov Exec and Stateline from the automated search since rss feeds have the full text included, and that is the description now.
unread_screened_feed <- unread_screened_feed %>%
  filter(!source %in% c("Gov Exec", "Stateline Democracy"))


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

##Pull in and eliminate from the list any articles that were already scraped in the 04 scripts. Filter out articles that weren't read in properly and are under 150 characters
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
  mutate(clean_title = tolower(title),
         clean_title = gsub("stat+|ap news|pdf|", "", clean_title),
         clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = str_trim(clean_title))

already_scraped_articles <- unique(scraped_articles$clean_title)

screened_articles_no_pdf <- unread_screened_feed_no_pdf %>%
  filter(!clean_title %in% already_scraped_articles)

##Eliminate articles already screened out by human coding
human_coding_spreadsheet <- read_excel("C:/AOS_db/data/11_coding_spreadsheet.xlsx") %>%
  mutate(clean_title = tolower(HEADLINE),
         clean_title = gsub("stat+|ap news|pdf|", "", clean_title),
         clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = str_trim(clean_title))


human_coded_titles <- unique(human_coding_spreadsheet$HEADLINE)
human_coded_clean_titles <- unique(human_coding_spreadsheet$clean_title)

screened_articles_no_pdf <- screened_articles_no_pdf %>%
  filter(!tolower(title_original) %in% human_coded_titles,
         !clean_title %in% human_coded_clean_titles)

write_csv(screened_articles_no_pdf, "C:/AOS_db/testing_the_script/results/05_screened_feed_to_read_not_scraped.csv")


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
