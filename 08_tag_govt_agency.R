#### R SCRIPT PURPOSE: 
#### Documents federal agencies mentioned in first 1/3 of article text.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 

p_load(tidyverse,
       rebus,
       readxl)

clean_aos <- read_csv("C:/AOS_db/data/07_aos_clean.csv")

#Read in spreadsheet with government agencies UCS tracks
gov_agencies <- read_excel("C:/AOS_db/info_tables/Search Terms AOS.xlsx") %>%
  filter(category == "government") %>%
  mutate(search_term_lower = tolower(search_term))

### Documenting Government Agencies ###  

#Identify agencies in the first third of article text 
#This portion of article text includes most of the content
#(The rest may include more context less directly related to the attack if present)
clean_aos_third  <- clean_aos %>%
  mutate(char_length = str_length(url_text), 
         third_length = ceiling(char_length/3) + 1, 
         use_text = str_sub(url_text, 1, third_length),
         use_text = tolower(use_text))

#Writing a function to:
#Search the truncated url_text for each of the government agencies,
#return the government agencies as a comma delimited list, and
#removes agencies that appear in the article multiple times
aos_clean_agency <- data.frame()


for(i in 1:nrow(clean_aos_third)){
  clean_aos_third_split <- clean_aos_third %>%
    slice(i)
  all_words <- c()
  
  for(j in 1:nrow(gov_agencies)){
    words_to_search <- gov_agencies %>%
      slice(j)
    single_word <- str_extract(clean_aos_third_split$use_text, words_to_search$search_term_lower)
    single_word_acr <- words_to_search$label[words_to_search$search_term_lower ==single_word]
    if(!is.na(single_word_acr)){
      all_words <- paste(single_word_acr, all_words, sep = ",")
    }
  }
  clean_aos_third_split <- mutate(clean_aos_third_split, gov_agency = all_words)
  aos_clean_agency <- bind_rows(clean_aos_third_split, aos_clean_agency)
}


remove_duplicates_in_cell <- function(cell_string) {
  items <- unlist(strsplit(cell_string, ","))
  unique_items <- unique(items)
  cleaned_string <- paste(unique_items, collapse = ", ")
  return(cleaned_string)
}

aos_clean_agency$gov_agency <- sapply(aos_clean_agency$gov_agency, remove_duplicates_in_cell)

write_csv(aos_clean_agency, "C:/AOS_db/data/08_aos_clean_gov.csv")


