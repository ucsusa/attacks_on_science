#### R SCRIPT PURPOSE: 
#### Clean up article text that contain potential attacks on science to prepare for subsequent analysis.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 

p_load(tidyverse) 

gc()


### Clean up Article Text Before Next Data Processing Step ###


filtered_read_articles <- read_csv("C:/AOS_db/data/06_aos_raw.csv") %>%
  filter(source != "Gov Info") %>%
  select(title, description, pub_date, URL, source, description_original, url_text) %>%
  unique()

filtered_read_articles <- filtered_read_articles %>%
  mutate(num_chars = nchar(url_text)) %>%
  group_by(pub_date, title, source) %>%
  slice_max(num_chars, n = 1) %>%
  ungroup() %>%
  unique() %>%
  select(-num_chars)

aoses_raw <- filtered_read_articles %>%
  unique() %>% 
  mutate(url_text_original = url_text, 
         url_text = tolower(url_text))

aoses_clean <- aoses_raw %>%
  mutate(url_text = tolower(url_text), 
         url_text = str_trim(url_text, side = "both"), 
         url_text = str_remove_all(url_text, "\\\n"), 
         url_text = str_remove_all(url_text, "[0|1|2|3|4|5|6|7|8|9|-|?|#|%|\\\\,|=|_|\\\\&|:|â|€|™|`|'|}|{]|!|/*|â€œ|\\\\&|â€|â€˜|â€™|â–ª|â€œ|â€|Ã©]"),
         url_text = str_remove_all(url_text, "\\\r"),
         url_text = str_remove_all(url_text, "\""),
         url_text = str_remove_all(url_text, "\\\\"),
         url_text = str_remove_all(url_text, "\\/"),
         url_text = str_remove_all(url_text, "\\\r"),
         url_text = str_remove_all(url_text, "skip to content"),
         url_text = str_remove_all(url_text, "skip to main content"),
         url_text = str_remove_all(url_text, "enter search here"),
         url_text = str_remove_all(url_text, "close search bar"),
         url_text = str_remove_all(url_text, "accessibility link"),
         url_text = str_remove_all(url_text, "copyright 2025 the associated press.|copyright  the associated press. .|copyright 2026 the associated press.|all rights reserved."),
         url_text = str_remove_all(url_text, "\\\\&nbsp|ap news"),
         url_text = str_remove_all(url_text, ". .             . ."))

#Create data frame for next R script
write_csv(aoses_clean, "C:/AOS_db/data/07_aos_clean.csv")
