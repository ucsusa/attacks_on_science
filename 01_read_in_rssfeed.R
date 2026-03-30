#### R SCRIPT PURPOSE: 
#### Grabs RSS feeds from our targeted news sources and creates a table updated daily.
#### Runs 1X/DAY


#Installing (if not already installed) and loading necessary R packages
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(httr, 
       tidyRSS, 
       XML, 
       tidyverse,
       xml2,
       rvest) 


### Pulling RSS Feeds From Each News Source ###


#Pulling daily NBC RSS Feed and cleaning it up
nbc_feed <- tidyfeed("https://feeds.nbcnews.com/nbcnews/public/news") %>% 
  mutate(title = item_title, 
         description = item_description, 
         pub_date = item_pub_date, 
         URL = item_link, 
         source = "National Broadcasting Corporation") %>% 
  select(title:source) 


#Pulling daily National Public Radio RSS Feed and cleaning it up
npr_feed <- tidyfeed("https://feeds.npr.org/1003/rss.xml") %>% 
  mutate(title = item_title, 
         description = item_description, 
         pub_date = item_pub_date, 
         URL = item_link, 
         source = "National Public Radio") %>% 
  select(title:source) 


#Pulling daily AP RSS Feed from Google News and cleaning it up
ap_feed <- tidyfeed("https://news.google.com/rss/search?q=when:24h+allinurl:apnews.com&hl=en-US&gl=US&ceid=US:en") %>% 
  mutate(title = item_title, 
         description = item_description, 
         pub_date = item_pub_date, 
         URL = item_link, 
         source = "Associated Press") %>% 
  select(title:source) 


#Pulling daily E&E News RSS Feed and cleaning it up
e_and_e_news_feed <- tidyfeed("https://rss.politico.com/eenews-eed") %>% 
  mutate(title = item_title, 
         description = item_description, 
         pub_date = item_pub_date, 
         URL = item_link, 
         source = "E&E News") %>% 
  select(title:source) 


#Pulling daily The Hill RSS Feed and cleaning it up
the_hill_feed <- tidyfeed("https://thehill.com/homenews/feed/")  %>% 
  mutate(title = item_title, #renaming Hill news article title column
         description = item_description, #renaming Hill news article description column
         pub_date = item_pub_date, #renaming Hill news article date of pub column
         URL = item_link, #renaming Hill news article URL column
         source = "The Hill") %>% #adding a column to specify news source
  select(title:source) #only including created columns

#Pulling daily Gov Exec RSS Feed from Google News and cleaning it up
govexec_feed <- GET("https://www.govexec.com/rss/all/", verbose())

#Creating XML document from WAPO RSS Feed
govexec_feed <- read_xml("https://www.govexec.com/rss/all/", options = "NOCDATA", encoding = "UTF-8")
items <- xml_find_all(govexec_feed, "//item")

#Compiling titles of each news article from WAPO RSS Feed
titles <- xml_text(xml_find_all(items, "title"))

#Compiling article descriptions of each news article from WAPO RSS Feed
descriptions <- xml_text(xml_find_all(items, "content:encoded"), trim = TRUE)


all_descriptions_text <- vector()

for(i in 1:length(descriptions)) {
  first_item_html <- descriptions[i]
  html_body <- read_html(first_item_html) %>% minimal_html() %>% html_elements("p") %>% html_text()
  descriptions_text <- paste(html_body, collapse = " ")
  all_descriptions_text <- c(all_descriptions_text, descriptions_text)
}


#Compiling date of publication of each news article from WAPO RSS Feed
pub_dates <- xml_text(xml_find_all(items, "pubDate"))

#Compiling URLs of each news article from GovExec RSS Feed
links <- xml_text(xml_find_all(items, "link"))

#Creating a data frame that contains each WAPO article's title, description, date of pub, and URL
govexec_feed_df <- data.frame(title = titles, description = all_descriptions_text, pub_date = pub_dates, URL = links, 
                         source = "Gov Exec", #adding a column to specify news source
                         stringsAsFactors = FALSE) #NOT changing text values to categorical variables

#Changing format of date of pub variable
govexec_feed_df <- govexec_feed_df %>%
  mutate(pub_date = as.Date(pub_date, format = "%a, %d %b %Y %H:%M:%S"))


#Pulling daily Stateline Democracy RSS Feed and cleaning it up
stateline_feed <- read_xml("https://stateline.org/category/democracy/feed/", encoding = "UTF-8")
items <- xml_find_all(stateline_feed, "//item")

titles <- xml_text(xml_find_all(items, "title"))

descriptions <- xml_text(xml_find_all(items, "content:encoded"), trim = TRUE)


all_descriptions_text <- vector()

for(i in 1:length(descriptions)) {
  first_item_html <- descriptions[i]
  html_body <- read_html(first_item_html) %>% minimal_html() %>% html_elements("p") %>% html_text()
  descriptions_text <- paste(html_body, collapse = " ")
  all_descriptions_text <- c(all_descriptions_text, descriptions_text)
}

pub_dates <- xml_text(xml_find_all(items, "pubDate"))

links <- xml_text(xml_find_all(items, "link"))

stateline_feed_df <- data.frame(title = titles, description = all_descriptions_text, pub_date = pub_dates, URL = links, 
                              source = "Stateline Democracy", 
                              stringsAsFactors = FALSE)

#Changing format of date of pub variable
stateline_demo_feed <- stateline_feed_df %>%
  mutate(pub_date = as.Date(pub_date, format = "%a, %d %b %Y %H:%M:%S"))


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
all_feed <- bind_rows(nbc_feed, 
                      npr_feed,
                      ap_feed, 
                      the_hill_feed, 
                      e_and_e_news_feed,
                      govexec_feed_df,
                      stateline_demo_feed,
                      statnews_demo_feed) %>%
  unique()

#Only including stories/entries from yesterday's date
todays_feed <- all_feed %>%
  filter(pub_date > (Sys.Date() - 1))

write_csv(unique(todays_feed), paste0("C:/AOS_db/data/01_rss_feed_dfs_", Sys.Date(), ".csv"))
