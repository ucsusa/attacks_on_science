#### R SCRIPT PURPOSE: 
#### Removes articles whose RSS descriptions/titles did not meet first search term criteria (after script 02 runs).
#### Runs 2x/month

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 

p_load(tidyverse,
       readtext, 
       purrr,
       readxl)

gc()


### Create Time Frame to Delete Screened Out Article Data ###


#Time frame = first of Jan 2025 (prior to start of AOS 2.0 data collection) to two weeks before the system (or present) date
date_2_wks_ago <- Sys.Date() - 14
start_date <- as.Date("2025-01-01", format = "%Y-%m-%d")

date_series <- seq.Date(start_date, date_2_wks_ago, by = "days")
date_series_del <- paste0(date_series, collapse = "|")

all_data_files <- list.files("C:/AOS_db/data/")
rss_feed_files <- all_data_files[grepl("_rss_feed_dfs", all_data_files)]
rss_feed_files <- rss_feed_files[!grepl(date_series_del, rss_feed_files)]


### Remove Articles Whose RSS Descriptions/Titles Did Not Meet First AOS Search Term Criteria ###


files_to_delete <- map(rss_feed_files, ~paste0("C:/AOS_db/data/", .))
##file.remove(files_to_delete)