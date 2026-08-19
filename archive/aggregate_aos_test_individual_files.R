library(tidyverse)
library(janitor)
library(purrr)
library(LSAfun)
library(rvest)
library(R.utils)
library(rvest)
library(polite)
library(readxl)
library(openxlsx)
library(pdftools)
library(stringi)
library(chatgpt)
#library(Microsoft365R)
#Used this help - https://github.com/jschiffman248/PQS/blob/main/optimize/Google%20News%20Scraping%20in%20R%3A%20A%20Step-by-Step%20Guide.md
#And this help - https://r4ds.hadley.nz/webscraping.html

setwd("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/data")

csvs <- list.files()

csvs <- csvs[grepl(".csv", csvs)]
csvs <- csvs[grepl("AOS Coding", csvs)]

aoses <- map_dfr(csvs, read.csv) %>%
  select(1:28) %>%
  filter(row_number() %in% c(1:nrow(.)))

column_names <- c(names(aoses[1:4]), aoses[2, 5:16] %>% as.character(), aoses[1, 17:25] %>% as.character(), names(aoses[26:28]))

colnames(aoses) <- column_names

aoses <- aoses %>%
  filter(`FULL.DATE` != "")


grab_text <- function(url_test){
  url_test <- polite::bow(url_test, force = TRUE) %>% 
    scrape(.) %>%
    html_node("body") %>%
    html_text() %>%
    as.character()}

grab_text2 <- possibly(grab_text, otherwise = NA)

aoses_all <- data.frame()
ticker <- 0

for(i in 1:nrow(aoses)){
aoses_split <- slice(aoses, i)
aoses_split <- mutate(aoses_split, url_text = map(LINK, grab_text2, .progress = TRUE)) 
aoses_all <- bind_rows(aoses_all, aoses_split)
ticker <- ticker + 1
print(ticker)
Sys.sleep(10)
}

aoses_raw <- aoses_all %>%
  mutate(url_text = unlist(url_text),
         url_text = as.character(url_text))

#write_csv(aoses_raw, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/results/aoses_raw.csv")

aoses_raw <- read_csv("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/results/aoses_raw.csv")

aoses_raw <- aoses_raw %>%
  mutate(HEADLINE = gsub("\x91", "‘", HEADLINE, useBytes=TRUE),
         HEADLINE = gsub("\x92", "’", HEADLINE, useBytes=TRUE),
         HEADLINE = gsub("\x93", "“", HEADLINE, useBytes=TRUE),
         HEADLINE = gsub("\x94", "”", HEADLINE, useBytes=TRUE))

##Pull in saved articles from Jules's Sharepoint drive

setwd("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/pdf_articles")

saved_pdfs <- list.files() 
saved_pdfs_title <- gsub(".pdf", "", saved_pdfs)
saved_pdfs_title <- str_extract(saved_pdfs_title, "[^-]+")
saved_pdfs_title <- str_trim(saved_pdfs_title)

pdfs_missing_db <- aoses_raw %>%
  filter(is.na(url_text))

pdfs_missing <- unique(pdfs_missing_db$HEADLINE)
pdfs_missing <- pdfs_missing[pdfs_missing %in% saved_pdfs_title]
pdfs_still_missing <- filter(pdfs_missing_db, !HEADLINE %in% pdfs_missing)

pdfs_not_missing <- data.frame(stringsAsFactors = FALSE)

##Remove punctuation from titles inline: gsub("[[:punct:]]", " ",
for(i in pdfs_missing){
  missing_pdf <- pdf_text(saved_pdfs[grepl(i, saved_pdfs)])
  missing_pdf <- unlist(paste(missing_pdf, collapse = ". "))
  pdfs_missing_i <- filter(pdfs_missing_db, str_replace_all(HEADLINE, "[^[:alnum:]]", "") == str_replace_all(i, "[^[:alnum:]]", ""))
  pdfs_missing_i <- mutate(pdfs_missing_i, url_text = missing_pdf)
  pdfs_not_missing <- bind_rows(pdfs_not_missing, pdfs_missing_i)
}

aoses_raw_no_missing_pdfs <- filter(aoses_raw, !is.na(url_text))

#aoses_raw_combined <- bind_rows(aoses_raw_no_missing_pdfs, pdfs_not_missing, pdfs_still_missing)
aoses_raw_combined <- pdfs_not_missing[1,]


aoses_clean <- aoses_raw_combined %>%
  mutate(url_text = ifelse(is.na(url_text), HEADLINE, url_text))

aoses_clean <- aoses_clean %>%
  mutate(url_text = str_remove_all(url_text, "\\\n"),
         url_text = str_remove_all(url_text, "[0|1|2|3|4|5|6|7|8|9|-|?|#|%|,|=|_|&|:|â|€|™|`|'|}|{|]|!|/*]"),
         url_text = str_remove_all(url_text, "\""),
         url_text = str_remove_all(url_text, " AM | PM "),
         url_text = str_remove_all(url_text, "\\\\"),
         url_text = str_remove_all(url_text, "\\/"),
         url_text = str_remove_all(url_text, "\\\r"),
         url_text = str_remove_all(url_text, "Skip to content"),
         url_text = str_remove_all(url_text, "Skip to main content"),
         url_text = str_remove_all(url_text, "Enter search here"),
         url_text = str_remove_all(url_text, "Close search bar"),
         url_text = str_remove_all(url_text, "Accessibility link"),
         url_text = str_remove_all(url_text, "Democracy Dies in Darkness"))
                                  

#write_csv(aoses_clean, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/results/aoses_clean.csv")

aoses_clean <- read_csv("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/results/aoses_clean.csv")


## Extract all text after the first occurrence of the headline
aoses_clean <- aoses_clean %>%
  mutate(HEADLINE = gsub("\x91", "‘", HEADLINE, useBytes=TRUE),
         HEADLINE = gsub("\x92", "’", HEADLINE, useBytes=TRUE),
         HEADLINE = gsub("\x93", "“", HEADLINE, useBytes=TRUE),
         HEADLINE = gsub("\x94", "”", HEADLINE, useBytes=TRUE),
         headline_no_spec_characters = str_remove_all(HEADLINE, ","),
         is_it_there = paste0(str_escape(headline_no_spec_characters)) %in% url_text,
         url_text = ifelse(grepl(paste0(str_escape(headline_no_spec_characters)), url_text), str_extract(url_text, paste0(str_escape(headline_no_spec_characters), ".*$")), url_text),
         url_text = ifelse(grepl(paste0(str_escape(headline_no_spec_characters)), url_text), str_remove_all(url_text, paste0(str_escape(headline_no_spec_characters))), url_text)) %>%
  select(-c(is_it_there, headline_no_spec_characters))

##Remove non-ascii characters
nchar(aoses_clean$url_text)
Encoding(aoses_clean$url_text) <- "latin1"  # (just to make sure)

aoses_clean$url_text <- iconv(aoses_clean$url_text, "latin1", "ASCII", sub="")

##Remove everything between parentheses.
aoses_clean <- aoses_clean %>%
  mutate(url_text = str_remove_all(url_text, " \\s*\\([^\\)]+\\)"))

##Remove everything between brackets.
##This doesn't do anything for URL #1 or #17
aoses_clean <- aoses_clean %>%
  mutate(url_text = gsub("\\[[^][]*]", " ", url_text))

##Remove everything that is over 11 characters long between two spaces.
##This removes nearly everything.
nchar(aoses_clean$url_text)
aoses_clean <- aoses_clean %>%
  mutate(url_text = gsub("(\\s\\S{11,})", " ", url_text))
nchar(aoses_clean$url_text)

words <- " NewspubDate--pubTimesectionPoliticssubBrandnonetitle he is nice  NewspubDate--pubTimesectionPoliticssubBrandnonetitle "
words <- gsub("(\\s\\S{11,}\\s)", " ", words)
##Remove everything after the word "Copyright"
aoses_clean <- aoses_clean %>%
  mutate(url_text = str_remove_all(url_text, paste0(str_escape("Copyright"), ".*$")))

##Remove all words that are longer than 15 characters long
nchar(aoses_clean$url_text)
aoses_clean <- aoses_clean %>%
  mutate(url_text = str_remove_all(url_text, '\\w{15,}'))
nchar(aoses_clean$url_text)

aoses_longer <- aoses_clean %>%
#  select(-`Other (write-in)`) %>%
  pivot_longer(., cols = c(`Agency Appointments`:`Targeting Scientists Based on Identity`), names_to = "attack_type", values_to = "type_response") %>%
  pivot_longer(., cols = c(`Climate Change`:`Public Health & Safety`), names_to = "attack_topic", values_to = "topic_response") %>%
  pivot_longer(., cols = c(Threatened:Completed), names_to = "attack_completion", values_to = "topic_enacted") %>%
  filter(type_response == 1, topic_response == 1, topic_enacted == 1) %>%
  unique()

aoses_grouped <- aoses_longer %>%
  clean_names() %>%
  filter(aos_presence == 1) %>%
  mutate(agencies_involved = toupper(agencies_involved)) %>%
  group_by(full_date, headline, link, attack_on_science_reversal, agencies_involved, attack_completion) %>%
  reframe(attack_topic_variable = paste0(unique(attack_topic), collapse = "_"),
          attack_type_variable = paste0(unique(attack_type), collapse = "_"),
          url_text = paste0(unique(url_text), collapse = ", ")) %>%
  ungroup() %>%
  select(full_date, attack_topic_variable, attack_type_variable, attack_on_science_reversal, agencies_involved, headline, link, attack_completion, url_text) %>%
  mutate(date = mdy(full_date)) %>%
  mutate(two_days_prior = date - 2,
         two_days_after = date + 2,
         plus_minus_2_days = paste(as.character(two_days_prior), "and", as.character(two_days_after))) %>%
  unique() %>%
  group_by(plus_minus_2_days, attack_on_science_reversal, agencies_involved, attack_type_variable, attack_topic_variable, attack_completion) %>%
  mutate(aos_object_id = paste(plus_minus_2_days, attack_on_science_reversal, agencies_involved, attack_type_variable, attack_topic_variable, attack_completion), sep = "_") %>%
  ungroup() %>%
  select(aos_object_id, plus_minus_2_days, attack_on_science_reversal, agencies_involved, attack_type_variable, attack_topic_variable, attack_completion, url_text) %>%
  distinct() %>%
  mutate(objectid = 0 + row_number(),
         objectid = str_pad(as.character(objectid), width = 5, side = "left", pad = "0"),
         url_text = str_trim(url_text))


#write_csv(aoses_grouped, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/results/aoses_grouped.csv")

##Read in pdfs with same title for missing text.


add_gpt_summary <- function(url_text){
  url_text <- genericSummary(url_text, k = 2, min = 5)
}


add_gpt_summary2 <- possibly(add_gpt_summary)

aoses_all_gpt <- data.frame()
ticker <- 0

for(i in 1:nrow(aoses_grouped)){
  aoses_split <- slice(aoses_grouped, i)
  aoses_split <- mutate(aoses_split, url_summary = ifelse(nchar(url_text) > 200,
                        map(url_text, add_gpt_summary2),
                        url_text))
  aoses_split <- mutate(aoses_split, 
                        url_summary = unlist(paste(url_summary, collapse = ". ")))
  aoses_all_gpt <- bind_rows(aoses_all_gpt, aoses_split)
  ticker <- ticker + 1
  print(ticker)
}

just_gpt <- unique(aoses_all_gpt$url_summary)

aoses_all_gpt_test <- mutate(aoses_all_gpt, url_summary = paste(unlist(url_summary), sep = ". "))

aoses_all_gpt_test <- aoses_all_gpt_test %>%
  mutate(url_summary = gsub("c\\(", "", url_summary),
         url_summary = gsub("\\(b)", "", url_summary),
         url_summary = gsub("\\(a)", "", url_summary),
         url_summary = str_remove_all(url_summary, '^"|"$'),
         url_summary = str_trim(url_summary),
         url_summary = ifelse(url_summary == "NULL", url_text, url_summary))

#write_csv(aoses_all_gpt_test, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/results/clean_aoses_gpt_test.csv")

#clean_aoses_gpt <- read_csv("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/results/clean_aoses_gpt.csv")

aoses_tableau <- aoses_all_gpt_test %>%
  separate_longer_delim(., cols = attack_type_variable, delim = "_") %>%
  separate_longer_delim(., cols = attack_topic_variable, delim = "_") %>%
  separate_wider_delim(., cols = plus_minus_2_days, delim = "and", names = c("day_minus_2", "day_plus_2")) %>%
  mutate(day_minus_2 = str_trim(day_minus_2) %>% ymd(.),
         day_plus_2 = str_trim(day_plus_2) %>% ymd(.),
         date = day_minus_2 + 2) %>%
  distinct()

#write_csv(aoses_tableau, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/results/clean_aoses_tableau.csv")
