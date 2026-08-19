library(tidyverse)
library(janitor)
library(purrr)
library(rvest)
library(R.utils)
library(rvest)
library(polite)
require(httr)
library(chromote)

screened_feed_files <- all_data_files[grepl("\\d{4}\\-\\d{2}\\-\\d{2}", all_data_files)]
screened_feed_files <- screened_feed_files[grepl("screened", screened_feed_files)]

last_3_days <- c(Sys.Date(), Sys.Date() - 1, Sys.Date() - 2)
last_3_days <- paste(last_3_days, collapse = "|")

screened_feed_files <- screened_feed_files[grepl(last_3_days, screened_feed_files)]

newscycle_screened_feed <- map_dfr(screened_feed_files, read_csv) %>%
  mutate(description_original = description,
         description = tolower(description))

ap_science <- "http://associated-press.s3-website-us-east-1.amazonaws.com/science.xml"
ap_health <- "http://associated-press.s3-website-us-east-1.amazonaws.com/health.xml"