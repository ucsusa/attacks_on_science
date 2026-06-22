#### R SCRIPT PURPOSE: 
#### Formats screened articles containing a potential attack on science into a spreadsheet for human coders to review.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 

p_load(tidyverse, 
       readxl,
       openxlsx) 

the_data_to_code <- read_csv("C:/AOS_db/data/10_aoses_clean_aggregate_aos.csv")

the_data_to_code <- the_data_to_code %>%
  mutate(pub_date = as.Date(pub_date))

#Filter out multiples that have lower numbers of characters because they were likely not fully read in
the_data_to_code <- the_data_to_code %>%
  mutate(num_chars = nchar(url_text)) %>%
  group_by(pub_date, title, source) %>%
  slice_max(num_chars, n = 1) %>%
  ungroup() %>%
  unique() %>%
  select(-num_chars)

#Add in articles that were never read in by scraping or automated pdf save
articles_to_scrape <- read_csv("C:/AOS_db/data/04_screened_feed_to_read.csv")
articles_read_in <- read_csv("C:/AOS_db/data/06_screened_read_articles_complete.csv")

articles_never_scraped <- articles_to_scrape %>%
  filter(!clean_title %in% unique(articles_read_in$clean_title)) %>%
  mutate(url_text = description_original,
         title = title_original,
         pub_date = as.Date(pub_date)) %>%
  select(title, description, pub_date, URL, source, url_text)

the_data_to_code <- bind_rows(the_data_to_code, articles_never_scraped)


#Set up columns and formatting for human coding
#AOS Presence of NA means nobody has yet coded that article
human_coding_spreadsheet <- the_data_to_code %>%
  select(-c(url_text, description, url_text_original)) %>%
 rename(`FULL DATE` = pub_date,
         HEADLINE = title,
         LINK = URL,
         `ARTICLE SOURCE` = source,
         `AGENCIES INVOLVED` = gov_agency,
         `ARTICLE DESCRIPTION` = description_original) %>%
  mutate(`FULL DATE` = as.Date(`FULL DATE`),
         CODERS = NA,
         `AOS PRESENCE` = NA,                             
         `Agency Appointments` = 0,                      
         `Rules/Regulations/Orders` = 0,                 
         `Scientific Advisory Committees/Boards` = 0,    
         `Altering Study Results` = 0,                   
         `Data Accessibility` = 0,                       
         `Data Collection` = 0,                          
         `Politicization of Grants and Funding` = 0,     
         Censorship = 0,                               
         `Losing Positions` = 0,                         
         `Restrictions from Professional Engagement` = 0,
         `Targeting Scientists Based on Identity` = 0,   
         `Climate Science` = 0,                          
         `Elections and Voting` = 0,                     
         Energy = 0,                                   
         Environment = 0,                              
         Equity = 0,                                   
         `Health & Safety` = 0,                   
         Any = 0,                                      
         `Other (write-in)` = "0",                         
         Threatened = 0,                               
         Completed = 0)


#Bring in existing human coding spreadsheet so that coded articles are not overwritten
aos_dataframe <- read_excel("C:/AOS_db/data/11_coding_spreadsheet.xlsx") %>%
  mutate(`FULL DATE` = as.Date(`FULL DATE`, format = "%m/%d/%Y"))

#Save a copy of the existing human coding spreadsheet until the process is more stable.
#write_csv(aos_dataframe, paste0("C:/AOS_db/data/11_coding_spreadsheet_", Sys.Date(), ".csv"))

human_coding_spreadsheet <- bind_rows(aos_dataframe, human_coding_spreadsheet)

#Also removing Stateline articles from before 1/20/2025
#And truncating character count in url_text to 32767 which is the character limit in an Excel cell
human_coding_spreadsheet <- human_coding_spreadsheet %>%
  mutate(HEADLINE = str_remove_all(HEADLINE, "- AP News"),
         HEADLINE = str_remove_all(HEADLINE, "STAT+"),
         HEADLINE = str_remove_all(HEADLINE, "\\+:"),
         HEADLINE = str_trim(HEADLINE, side = "both")) %>%
  group_by(`FULL DATE`, tolower(HEADLINE), LINK, `ARTICLE SOURCE`) %>%
  slice_max(., order_by = `AOS PRESENCE`, with_ties = FALSE) %>%
  ungroup() %>%
  select(HEADLINE, `FULL DATE`, LINK, `ARTICLE SOURCE`, SI_mention, GSS_mention, `AGENCIES INVOLVED`, CODERS, `ARTICLE DESCRIPTION`, everything()) %>%
  select(-`tolower(HEADLINE)`) %>%
  group_by(`FULL DATE`, `LINK`) %>%
  slice_max(., order_by = `AOS PRESENCE`, with_ties = FALSE) %>%
  slice_max(order_by = str_count(`ARTICLE DESCRIPTION`, "[A-Z]")) %>%
  ungroup() %>%
  filter(`FULL DATE` > "2025-01-19") %>%
  mutate(`ARTICLE DESCRIPTION` = str_trunc(`ARTICLE DESCRIPTION`, 32000, side = "right")) %>%
  distinct()
  
wb <- createWorkbook()
addWorksheet(wb, "coding")

writeData(wb, "coding", human_coding_spreadsheet)

saveWorkbook(wb, "C:/AOS_db/data/11_coding_spreadsheet.xlsx", overwrite = TRUE)
