#### R SCRIPT PURPOSE: 
#### Formats the screened (2x) articles into a spreadsheet for human coders
#### Runs 1X/WEEK

### Logistics for R Script ###


#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data
       readxl,
       openxlsx) #to read, format, create etc. excel files


### Format Data for Human Coding ###


#import data from previous R Script
the_data_to_code <- read_csv("C:/AOS_db/data/10_aoses_clean_aggregate_aos.csv")

### Get Data Ready for Next Script ###

#create new data set


#Filter data to after December 16th
the_data_to_code <- the_data_to_code %>%
  mutate(pub_date = as.Date(pub_date))

#Filter duplicates to articles with highest character url_text, or read in articles
the_data_to_code <- the_data_to_code %>%
  mutate(num_chars = nchar(url_text)) %>%
  group_by(pub_date, title, source) %>%
  slice_max(num_chars, n = 1) %>%
  ungroup() %>%
  unique() %>%
  select(-num_chars)

#Add in the articles that were never read in by scraping or automated pdf save
articles_to_scrape <- read_csv("C:/AOS_db/data/04_screened_feed_to_read.csv")
articles_read_in <- read_csv("C:/AOS_db/data/06_screened_read_articles_complete.csv")
articles_never_scraped <- articles_to_scrape %>%
  filter(!clean_title %in% unique(articles_read_in$clean_title)) %>%
  mutate(url_text = description_original,
         title = title_original) %>%
  select(title, description, pub_date, URL, source, url_text)

the_data_to_code <- bind_rows(the_data_to_code, articles_never_scraped)


#formatting data into human coding spreadsheet
human_coding_spreadsheet <- the_data_to_code %>%
  select(-c(url_text, description, url_text_original)) %>%
  #changing names of the following columns
 rename(`FULL DATE` = pub_date,
         HEADLINE = title,
         LINK = URL,
         `ARTICLE SOURCE` = source,
         `AGENCIES INVOLVED` = gov_agency,
         `ARTICLE DESCRIPTION` = description_original) %>%
  #ensure date of publication is formatted as a date
  mutate(`FULL DATE` = as.Date(`FULL DATE`),
         #agencies involved and coders are blank at this stage
         CODERS = NA,
         #mark each of these columns as 0 that coders can change to one when present
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


#Bring in existing human coding spreadsheet
aos_dataframe <- read_excel("C:/AOS_db/data/11_coding_spreadsheet.xlsx") %>%
  mutate(`FULL DATE` = as.Date(`FULL DATE`, format = "%m/%d/%Y"))

#Save a copy of the existing human coding spreadsheet until the process is more stable.
write_csv(aos_dataframe, paste0("C:/AOS_db/data/11_coding_spreadsheet_", Sys.Date(), ".xlsx"))

human_coding_spreadsheet <- bind_rows(aos_dataframe, human_coding_spreadsheet)

##Remove duplicates if they show up

human_coding_spreadsheet <- human_coding_spreadsheet %>%
  mutate(HEADLINE = str_remove_all(HEADLINE, "- AP News"),
         HEADLINE = str_trim(HEADLINE, side = "both")) %>%
  group_by(`FULL DATE`, tolower(HEADLINE), LINK, `ARTICLE SOURCE`) %>%
  slice_max(., order_by = `AOS PRESENCE`, with_ties = FALSE) %>%
  ungroup() %>%
  select(HEADLINE, `FULL DATE`, LINK, `ARTICLE SOURCE`, SI_mention, GSS_mention, `AGENCIES INVOLVED`, CODERS, `ARTICLE DESCRIPTION`, everything()) %>%
  select(-`tolower(HEADLINE)`) %>%
  group_by(`FULL DATE`, `LINK`) %>%
  slice_max(order_by = str_count(HEADLINE, "[A-Z]")) %>%
  ungroup()
  
### Write Data for Humans to Code ###
#create new dataset with formatted spreadsheet
# Create a new workbook and add a sheet
wb <- createWorkbook()
addWorksheet(wb, "coding")

# Write data to the sheet
writeData(wb, "coding", human_coding_spreadsheet)

saveWorkbook(wb, "C:/AOS_db/data/11_coding_spreadsheet.xlsx", overwrite = TRUE)
