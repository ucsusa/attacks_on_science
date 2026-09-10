#### R SCRIPT PURPOSE: 
#### Grabs RSS feeds from our targeted news sources daily and puts into a table.
#### Runs 1x/day

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


#Pulling daily National Public Radio (NPR) RSS Feed and cleaning it up
npr_feed <- tidyfeed("https://rss.app/feeds/bMSQqU14WeS5WGus.xml") %>% 
  mutate(title = item_title, 
         description = item_description, 
         pub_date = item_pub_date, 
         URL = item_link, 
         source = "National Public Radio") %>% 
  select(title:source) 


#Pulling daily AP RSS Feed from Google News and cleaning it up

ap_feed <- tidyfeed("https://rss.app/feeds/6t9bqguHo638jpxk.xml") %>% 
  mutate(title = item_title, 
         description = item_description, 
         pub_date = item_pub_date, 
         URL = item_link, 
         source = "Associated Press") %>% 
  select(title:source) 


#Pulling daily Politico rss feed and cleaning it up

feed_categories <- c("congress", "healthcare", "defense", "economy", "energy", "politics-news")

feed_urls <- sapply(feed_categories, function(w) {
paste0("https://rss.politico.com/", w, ".xml")
})

names(feed_urls) <- NULL

politico_feeds <- data.frame()
for(i in feed_urls){
  politico_feed <- tidyfeed(i) %>%
    mutate(title = item_title, 
         description = item_description, 
         pub_date = item_pub_date, 
         URL = item_link, 
         source = "Politico") %>% 
  select(title:source) 
  
  politico_feeds <- bind_rows(politico_feed, politico_feeds) %>%
    distinct()
}



#Pulling daily The Hill RSS Feed and cleaning it up
the_hill_feed <- tidyfeed("https://thehill.com/homenews/feed/")  %>% 
  mutate(title = item_title,
         description = item_description,
         pub_date = item_pub_date,
         URL = item_link,
         source = "The Hill") %>%
  select(title:source)


#Pulling daily Gov Exec RSS Feed from Google News, including the full text, and cleaning it up
govexec_feed <- GET("https://www.govexec.com/rss/all/", verbose())

govexec_feed <- read_xml("https://www.govexec.com/rss/all/", options = "NOCDATA", encoding = "UTF-8")
items <- xml_find_all(govexec_feed, "//item")

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

govexec_feed_df <- data.frame(title = titles, description = all_descriptions_text, pub_date = pub_dates, URL = links, 
                         source = "Gov Exec", 
                         stringsAsFactors = FALSE) 

govexec_feed_df <- govexec_feed_df %>%
  mutate(pub_date = as.Date(pub_date, format = "%a, %d %b %Y %H:%M:%S"))


#Pulling daily Stateline Democracy RSS Feed, including the full text, and cleaning it up
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

stateline_demo_feed <- stateline_feed_df %>%
  mutate(pub_date = as.Date(pub_date, format = "%a, %d %b %Y %H:%M:%S"))


#Pulling daily Stat News RSS Feed and cleaning it up
statnews_demo_feed <- tidyfeed("https://www.statnews.com/feed/") %>% 
  mutate(title = item_title, 
         description = item_description, 
         pub_date = item_pub_date, 
         URL = item_link, 
         source = "Stat News") %>% 
  select(title:source) 


### Getting Data Ready for Next Script in Sequence ###


all_feed <- bind_rows(nbc_feed, 
                      npr_feed,
                      ap_feed, 
                      the_hill_feed, 
                      politico_feeds,
                      govexec_feed_df,
                      stateline_demo_feed,
                      statnews_demo_feed) %>%
  unique()

#Only including items from the previous day
todays_feed <- all_feed %>%
  filter(pub_date > (Sys.Date() - 1))

write_csv(unique(todays_feed), paste0("../data/01_rss_feed_dfs_", Sys.Date(), ".csv"))
