library(tidyverse)
library(janitor)
library(purrr)
library(rvest)
library(R.utils)
library(rvest)
library(polite)

setwd("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/data")

csvs <- list.files()

csvs <- csvs[grepl(".csv", csvs)]
csvs <- csvs[!grepl("rss|aos_raw", csvs)]

aoses <- map_dfr(csvs, read.csv) %>%
  select(1:28) %>%
  filter(row_number() %in% c(1:nrow(.)))

column_names <- c(names(aoses[1:4]), aoses[2, 5:16] %>% as.character(), aoses[1, 17:25] %>% as.character(), names(aoses[26:28]))

colnames(aoses) <- column_names

grab_text <- function(url_test){
  url_test <- polite::bow(url_test, force = TRUE) %>% 
    scrape(.) %>%
    html_node("body") %>%
    html_text2() %>%
    as.character()}

grab_text2 <- possibly(grab_text, otherwise = NA)

aoses <- aoses %>%
  filter(`FULL.DATE` != "")

aoses_all <- data.frame()
ticker <- 0

for(i in 1:nrow(aoses)){
  aoses_split <- slice(aoses, i)
  aoses_split <- withTimeout(mutate(aoses_split, url_text = map(LINK, grab_text2, .progress = TRUE)), timeout = 100)
  aoses_all <- bind_rows(aoses_all, aoses_split)
  ticker <- ticker + 1
  print(ticker)
}

aoses_text <- aoses_all %>%
  mutate(url_text = ifelse(url_text == "NA", HEADLINE, url_text),
         url_text = ifelse(is.null(url_text), HEADLINE, url_text))

aoses_text <- aoses_text %>%
  mutate(url_text = str_remove_all(url_text, "\\\n"),
         url_text = str_remove_all(url_text, "[0|1|2|3|4|5|6|7|8|9|-|?|#|%|,|=|_|&|:|â|€|™|`|'|}|{]"),
         url_text = str_remove_all(url_text, "\\\r"))

write_csv(aoses_text, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/results/words_for_text_analysis.csv")