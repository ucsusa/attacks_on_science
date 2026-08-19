#### R SCRIPT PURPOSE: 
#### Clean up article text to prepare for summaries
#### Runs 1X/WEEK

##Filtering needs
#1. Filter out any articles that have been screened out by human coding.
#2. Filter out duplicate articles, with a preference to the most number of characters read in or in url_text. DONE


### Logistics for R Script ###


#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse) #to wrangle and organize noisy data

#garbage collection; removing things from memory that are no longer in use
gc()


### Clean Up Article Text Data ###


#read in file with scraped article text that matches search term criteria
filtered_read_articles <- read_csv("C:/AOS_db/data/05_aos_raw.csv") %>%
  filter(source != "Gov Info") %>%
  select(title, description, pub_date, URL, source, description_original, url_text) %>%
  unique()

#Filter duplicates to articles with highest character url_text, or read in articles
filtered_read_articles <- filtered_read_articles %>%
  mutate(num_chars = nchar(url_text)) %>%
  group_by(pub_date, title, source) %>%
  slice_max(num_chars, n = 1) %>%
  ungroup() %>%
  unique() %>%
  select(-num_chars)

#format data set to clean text
aoses_raw <- filtered_read_articles %>%
  unique() %>% #eliminate duplicates
  mutate(url_text_original = url_text, #duplicate article text column
         url_text = tolower(url_text))

#start cleaning article text
aoses_clean <- aoses_raw %>%
  #wrangle messy text data
  mutate(url_text = tolower(url_text), #change to lowercase
         url_text = str_trim(url_text, side = "both"), #remove blank spaces on either side of text
         url_text = str_remove_all(url_text, "\\\n"), #remove paragraph spacing
         #remove numbers, punctuation, special characters
         url_text = str_remove_all(url_text, "[0|1|2|3|4|5|6|7|8|9|-|?|#|%|\\\\,|=|_|\\\\&|:|â|€|™|`|'|}|{]|!|/*|â€œ|\\\\&|â€|â€˜|â€™|â–ª|â€œ|â€|Ã©]"),
         #remove various types of hard returns
         url_text = str_remove_all(url_text, "\\\r"),
         url_text = str_remove_all(url_text, "\""),
         url_text = str_remove_all(url_text, "\\\\"),
         url_text = str_remove_all(url_text, "\\/"),
         url_text = str_remove_all(url_text, "\\\r"),
         #remove specific boilerplate and article text language
         url_text = str_remove_all(url_text, "skip to content"),
         url_text = str_remove_all(url_text, "skip to main content"),
         url_text = str_remove_all(url_text, "enter search here"),
         url_text = str_remove_all(url_text, "close search bar"),
         url_text = str_remove_all(url_text, "accessibility link"),
         url_text = str_remove_all(url_text, "copyright 2025 the associated press.|copyright  the associated press. .|copyright 2026 the associated press.|all rights reserved."),
         url_text = str_remove_all(url_text, "\\\\&nbsp|ap news"),
         url_text = str_remove_all(url_text, ". .             . ."))


### Get Data Ready for Next R Script (07) ###


#write new dataset with clean text data
write_csv(aoses_clean, "C:/AOS_db/data/06_aos_clean.csv")
