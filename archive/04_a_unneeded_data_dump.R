##This script deletes files once they are screened out and/or backed up on Onedrive.

library(tidyverse)

##Delete 01 rss files.

##Pull in 01 rss files
data_files <- list.files("C:/AOS_db/data/")
rss_files <- data_files[grepl("01_rss", data_files)]

##Delete all 01_rss files pulled in up to the time of deletion. 
file.remove(paste0("C:/AOS_db/data/", rss_files))


##Delete unneeded rows in the scanned 02_ file.

##Grab the date before which the rows can be truncated.
todays_date <- Sys.Date()
last_week_today <- Sys.Date() - 7
last_week_today <- ymd(last_week_today)

##Pull in the 02 file
screened_rss_feed <- read_csv("C:/AOS_db/data/02_rss_feed_screened_dfs.csv")
screened_rss_feed <- screened_rss_feed %>%
  mutate(pub_date = substr(pub_date, 1, 10),
         pub_date = ymd(pub_date))

screened_rss_feed_fil <- screened_rss_feed %>%
  filter(pub_date > last_week_today)

screened_rss_feed_new <- anti_join(screened_rss_feed_fil, screened_rss_feed)

write_csv("C:/AOS_db/data/02_rss_feed_screened_dfs.csv")

##Delete unneeded rows in the scanned 03_ file.

##Grab the date before which the rows can be truncated.
todays_date <- Sys.Date()
two_weeks_ago_today <- Sys.Date() - 14
two_weeks_ago_today <- ymd(two_weeks_ago_today)

##Pull in the 03 file to be truncated
screened_read_articles <- read_csv("C:/AOS_db/data/03_rss_feed_screened_read_articles_dfs.csv")

##Pull in the 05 file containing all read and saved pdf read articles
readin_pdf_articles <- read_csv("C:/AOS_db/data/05_screened_read_articles_complete.csv")

##Eliminate rows with pubdates earlier than 2 weeks prior to the current date.
screened_read_articles <- screened_read_articles %>%
  mutate(pub_date = substr(pub_date, 1, 10),
         pub_date = ymd(pub_date))

screened_read_articles_fil <- screened_read_articles %>%
  filter(pub_date > two_weeks_ago_today)

##Eliminate rows with articles that have already been read in by scraping or saving a pdf
screened_read_articles_new <- anti_join(screened_read_articles_fil, readin_pdf_articles)

write_csv("C:/AOS_db/data/03_rss_feed_screened_read_articles_dfs.csv")

