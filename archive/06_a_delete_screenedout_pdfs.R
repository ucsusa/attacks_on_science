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
screened_in_articles <- read_csv("C:/AOS_db/data/05_aos_raw.csv")

##Read in the articles with pdfs saved (to be screened)
completed_saved_articles <- read_csv("C:/AOS_db/data/05_screened_read_articles_complete.csv")

##Filter out the articles that have been screened in.
screened_out_articles <- completed_saved_articles %>%
  filter(!title %in% screened_in_articles$title)

pdf_folder <- setwd("C:/AOS_db/pdf_articles")

file.remove(screened_out_articles$file_name)