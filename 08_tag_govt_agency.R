#### R SCRIPT PURPOSE: 
#### Tags government agencies mentioned in article text
#### Runs (?) How often will this script run?

##Filtering needs
#1. Filter out articles that were screened out by search terms or human coding.
#2. Filter out articles before December 16, 2026
#3. Filter out duplicates using the agreed upon source prioritization, while maintaining the unique ID.

### Logistics for R Script ###


#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data
       rebus,
       readxl) #to more easily work with regex expressions

#read in data
df0 <- read_csv("C:/AOS_db/data/07_aos_clean.csv")

#read in spreadsheet with government agencies
gov_agencies <- read_excel("C:/AOS_db/info_tables/Search Terms AOS.xlsx") %>%
  filter(category == "government") %>%
  mutate(search_term_lower = tolower(search_term))

### Tagging Government Agencies in First 1/3 of Article Text ###

#cut URL text down by 1/3
df1  <-  df0 %>%
  mutate(char_length = str_length(url_text), #counting full length of each article by character
         third_length = ceiling(char_length/3) + 1, #create cut off of one third
         use_text = str_sub(url_text, 1, third_length),
         use_text = tolower(use_text)) #cut each article into first third

##Search the truncated url_text for each of the government agencies and return the government agencies as a comma delimited list.
df2 <- data.frame()


for(i in 1:nrow(df1)){
  df1_split <- df1 %>%
    slice(i)
  all_words <- c()
  
  for(j in 1:nrow(gov_agencies)){
    words_to_search <- gov_agencies %>%
      slice(j)
    single_word <- str_extract(df1_split$use_text, words_to_search$search_term_lower)
    single_word_acr <- words_to_search$label[words_to_search$search_term_lower ==single_word]
    if(!is.na(single_word_acr)){
      all_words <- paste(single_word_acr, all_words, sep = ",")
    }
  }
  df1_split <- mutate(df1_split, gov_agency = all_words)
  df2 <- bind_rows(df1_split, df2)
}


remove_duplicates_in_cell <- function(cell_string) {
  items <- unlist(strsplit(cell_string, ","))
  unique_items <- unique(items)
  cleaned_string <- paste(unique_items, collapse = ", ")
  return(cleaned_string)
}

# Apply the function to the 'values' column
df2$gov_agency <- sapply(df2$gov_agency, remove_duplicates_in_cell)

write_csv(df2, "C:/AOS_db/data/08_aos_clean_gov.csv")


