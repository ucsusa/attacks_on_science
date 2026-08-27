##This is a scheduled script that deleted pdfs that have been screened out using the categories of search terms.

#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data
       readtext, #to import/handle text files and their metadata
       purrr,
       readxl) #to automate/loop specific functions

#garbage collection; removing things from memory that are no longer in use
gc()

##Read in screened in articles
screened_in_articles <- read_csv("../data/06_aos_raw.csv") %>%
  mutate(clean_title = tolower(title),
         clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = str_trim(clean_title))


##Read in the articles with pdfs saved (to be screened)
completed_saved_articles <- read_csv("../data/06_screened_read_articles_complete.csv") %>%
  mutate(clean_title = tolower(title),
         clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = str_trim(clean_title))


##Filter out the articles that have been screened in.
screened_out_articles <- completed_saved_articles %>%
  filter(!clean_title %in% screened_in_articles$clean_title)

##Filter out the articles that have been human coded.
coded_articles <- read_excel("../data/11_coding_spreadsheet.xlsx") %>%
  filter(`AOS PRESENCE` %in% c(0,1)) %>%
  mutate(clean_title = tolower(HEADLINE),
         clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title),
         clean_title = str_trim(clean_title)) %>%
  select(HEADLINE, clean_title, `FULL DATE`, LINK, `ARTICLE SOURCE`) %>%
  rename(title = HEADLINE,
         pub_date = `FULL DATE`,
         URL = LINK,
         source = `ARTICLE SOURCE`)


coded_screened_out_articles <- bind_rows(screened_out_articles, coded_articles)


pdf_folder <- setwd("../pdf_articles/")

pdf_files <- list.files(pdf_folder)

pdf_df <- data.frame(files = pdf_files)

pdf_df <- pdf_df %>%
  mutate(filename = tolower(files)) %>%
  mutate(filename = gsub("[[:punct:]]", "", filename),
         filename = gsub("pdf|stat |ap news", "", filename),
         filename = gsub("  ", " ", filename),
         filename = str_trim(filename))

##Make a list of the files to delete
pdf_df <- pdf_df %>%
  filter(filename %in% coded_screened_out_articles$clean_title)

file.remove(pdf_df$files)

##Remove pdfs that are a month old

pdf_df_c_date <- data.frame(stringsAsFactors = FALSE)

for(i in pdf_files){
  info <- file.info(i)
  c_date <- info$ctime
  pdf_info_df_i <- data.frame(article_names = i, creation_date = c_date)
  pdf_df_c_date <- bind_rows(pdf_df_c_date, pdf_info_df_i)
}

pdf_df_c_date <- pdf_df_c_date %>%
  mutate(c_date = as.Date(creation_date, format = "%Y-%m-%d"),
         date_diff = today() - c_date,
         date_diff_num = as.numeric(date_diff)) %>%
  filter(date_diff_num > 60)

file.remove(pdf_df_c_date$article_names)

