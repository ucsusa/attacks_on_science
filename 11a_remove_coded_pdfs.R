#### R SCRIPT PURPOSE: 
####This script deletes saved pdfs forms of the articles that have been human-coded as a data hygiene measure.
#### Runs monthly

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse, 
       janitor,
       readxl)

human_coding_spreadsheet <- read_excel("C:/AOS_db/data/11_coding_spreadsheet.xlsx")

hc_ss_fil <- human_coding_spreadsheet %>%
  filter(`AOS PRESENCE` %in% c(0,1))

hc_ss_titles <- unique(hc_ss_fil$HEADLINE)
hc_ss_titles <- map(hc_ss_titles, ~paste0(., ".pdf"))

pdf_articles <- list.files("C:/AOS_db/pdf_articles")

pdf_articles_coded <- pdf_articles[pdf_articles %in% hc_ss_titles]

setwd("C:/AOS_db/pdf_articles")
file.remove(pdf_articles_coded)
