##The purpose of this script is to delete rss feed that has already been screened after the 02 script runs. We will give a 2 week lag just in case.

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

##Create a series of dates, delimited by a bar, that starts with the first of Jan 2025 and ends with two weeks from the system date (present date)

date_2_wks_ago <- Sys.Date() - 14
start_date <- as.Date("2025-01-01", format = "%Y-%m-%d")

date_series <- seq.Date(start_date, date_2_wks_ago, by = "days")
date_series_del <- paste0(date_series, collapse = "|")

##List rss files
all_data_files <- list.files("C:/AOS_db/data/")
rss_feed_files <- all_data_files[grepl("_rss_feed_dfs", all_data_files)]
rss_feed_files <- rss_feed_files[!grepl(date_series_del, rss_feed_files)]

##Delete the rss files that have already screened out.
files_to_delete <- map(rss_feed_files, ~paste0("C:/AOS_db/data/", .))
##file.remove(files_to_delete)