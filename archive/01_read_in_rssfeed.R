#### R SCRIPT PURPOSE: 
#### Grabs RSS feeds from our targeted news sources and creates a table updated daily.
#### Runs 1X/DAY
###Output (from this script) deletion guidance -- This script runs daily. The next script also runs daily an hour after this script runs in the morning.All 01_ files in data can be deleted once the 02 script runs at 7AM Pacific time on a daily basis. Caution - we need to be able to recreate the rss feed if we make any changes. Right now (1/6/2026) we don't have a way to go back and pull old rss feed. For now we could do a weekly dump after one of the backup scripts run and the rss feed is on Jule's Onedrive. Recommendation - A Friday night rss feed dump.



### Logistics for R Script ###


#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(httr, #to pull and organize HTTP content from news websites
       tidyRSS, #to pull and organize news outlet RSS feeds
       XML, #to parse XML files created by news outlet webpages
       tidyverse) #to wrangle and organize noisy data


### Pulling RSS Feeds From Each News Source ###


##Daily Washington Post (WAPO) RSS Feed


#Pulling daily WAPO RSS Feed
wapo_feed <- GET("https://feeds.washingtonpost.com/rss/national?itid=lk_inline_manual_30", verbose())

#Creating XML document from WAPO RSS Feed
wapo_feed <- xmlParse(wapo_feed)

#Compiling titles of each news article from WAPO RSS Feed
titles    <- xpathSApply(wapo_feed,'//item/title',xmlValue)

#Compiling article descriptions of each news article from WAPO RSS Feed
descriptions    <- xpathSApply(wapo_feed,'//item/description',xmlValue)

#Compiling date of publication of each news article from WAPO RSS Feed
pubdates <- xpathSApply(wapo_feed,'//item/pubDate',xmlValue)

#Compiling URLs of each news article from WAPO RSS Feed
links <- xpathSApply(wapo_feed,'//item/link',xmlValue)

#Creating a data frame that contains each WAPO article's title, description, date of pub, and URL
wapo_feed_df <- data.frame(title = titles, description = descriptions, pub_date = pubdates, URL = links, 
                           source = "Washington Post", #adding a column to specify news source
                           stringsAsFactors = FALSE) #NOT changing text values to categorical variables

#Changing format of date of pub variable
wapo_feed_df <- wapo_feed_df %>%
  mutate(pub_date = str_extract(pub_date, "\\,\\s*(.*?)\\s*\\+") %>% #removing special characters
           str_trim(., side = "both") %>% #removing white space on either side of text
           dmy_hms(.)) #changing date format (into date, month, year, hour, minute, second)


##Daily New York Times (NYT) RSS Feed


#Pulling daily NYT RSS Feed and cleaning it up
nyt_feed <- tidyfeed("https://rss.nytimes.com/services/xml/rss/nyt/US.xml") %>% 
  mutate(title = item_title, #renaming NYT news article title column
         description = item_description, #renaming NYT news article description column
         pub_date = item_pub_date, #renaming NYT news article date of pub column
         URL = item_link, #renaming NYT news article URL column
         source = "New York Times") %>% #adding a column to specify news source
  select(title:source) #only including created columns


##Daily AP News (AP) RSS Feed


#Pulling daily AP RSS Feed from Google News and cleaning it up
ap_news_feed <- tidyfeed("https://news.google.com/rss/search?q=when:24h+allinurl:apnews.com&hl=en-US&gl=US&ceid=US:en") %>% 
  mutate(title = item_title, #renaming AP news article title column
         description = item_description, #renaming AP news article description column
         pub_date = item_pub_date, #renaming AP news article date of pub column
         URL = item_link, #renaming AP news article URL column
         source = "Associated Press") %>% #adding a column to specify news source
  select(title:source) #only including created columns


##Daily E&E News (EE) RSS Feed


#Pulling daily EE RSS Feed and cleaning it up
e_and_e_news_feed <- tidyfeed("https://rss.politico.com/eenews-eed") %>% 
  mutate(title = item_title, #renaming EE news article title column
         description = item_description, #renaming EE news article description column
         pub_date = item_pub_date, #renaming EE news article date of pub column
         URL = item_link, #renaming EE news article URL column
         source = "E&E News") %>% #adding a column to specify news source
  select(title:source) #only including created columns


##Daily The Hill RSS Feed


#Pulling daily Hill RSS Feed
the_hill_feed <- tidyfeed("https://thehill.com/homenews/feed/")  %>% 
  mutate(title = item_title, #renaming Hill news article title column
         description = item_description, #renaming Hill news article description column
         pub_date = item_pub_date, #renaming Hill news article date of pub column
         URL = item_link, #renaming Hill news article URL column
         source = "The Hill") %>% #adding a column to specify news source
  select(title:source) #only including created columns


#Pulling daily Gov Exec RSS Feed and cleaning it up
govexec_feed <- tidyfeed("https://www.govexec.com/rss/all/") %>% 
  mutate(title = item_title, #renaming Gov Exec news article title column
         description = item_description, #renaming Gov Exec news article description column
         pub_date = item_pub_date, #renaming Gov Exec news article date of pub column
         URL = item_link, #renaming Gov Exec news article URL column
         source = "Gov Exec") %>% #adding a column to specify news source
  select(title:source) #only including created columns

#Pulling daily Stateline Democracy RSS Feed and cleaning it up
stateline_demo_feed <- tidyfeed("https://stateline.org/category/democracy/feed/") %>% 
  mutate(title = item_title, #renaming Stateline Democracy news article title column
         description = item_description, #renaming Stateline Democracy news article description column
         pub_date = item_pub_date, #renaming Stateline Democracy news article date of pub column
         URL = item_link, #renaming Stateline Democracy news article URL column
         source = "Stateline Democracy") %>% #adding a column to specify news source
  select(title:source) #only including created columns


#Pulling daily Stat News RSS Feed and cleaning it up
statnews_demo_feed <- tidyfeed("https://www.statnews.com/feed/") %>% 
  mutate(title = item_title, #renaming Stat News article title column
         description = item_description, #renaming Stat News article description column
         pub_date = item_pub_date, #renaming Stat News article date of pub column
         URL = item_link, #renaming Stat News article URL column
         source = "Stat News") %>% #adding a column to specify news source
  select(title:source) #only including created columns


### Getting Data Ready for Next Script in Sequence (02) ###


#Create a data frame with all RSS feeds pulled/created above
all_feed <- bind_rows(wapo_feed_df, 
                      nyt_feed,
                      ap_news_feed, 
                      the_hill_feed, 
                      e_and_e_news_feed,
                      govexec_feed,
                      stateline_demo_feed,
                      statnews_demo_feed) %>%
  unique() #deleting duplicate stories/entries

#Only including stories/entries from yesterday's date
todays_feed <- all_feed %>%
  filter(pub_date > (Sys.Date() - 1))

#Creating a CSV to save todays_feed to be used in next R script
write_csv(todays_feed, paste0("C:/AOS_db/data/01_rss_feed_dfs_", Sys.Date(), ".csv"))
