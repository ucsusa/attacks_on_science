#### R SCRIPT PURPOSE: 
#### Formats the screened (2x) articles into a spreadsheet for human coders
#### Runs 1X/WEEK

##Filtering needs
#1. Screen out articles previous to December 16, 2025 DONE
#2. Filter out duplicate articles, with a preference to the most number of characters read in or in url_text. DONE
#3. Filter out articles that were screened out by search terms. THIS SHOULD BE DONE IN PREVIOUS SCRIPTS, but we can monitor
#4. Filter out articles that were screened out by human coding. DONE

### Logistics for R Script ###


#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data
       readxl) #to read, format, create etc. excel files


### Format Data for Human Coding ###


#import data from previous R Script
the_data_to_code <- read_csv("C:/AOS_db/data/07_aoses_clean_aggregate_aos.csv")

### Get Data Ready for Next Script ###

#create new data set


#Filter data to after December 16th
the_data_to_code <- the_data_to_code %>%
  mutate(pub_date = as.Date(pub_date)) %>%
  filter(pub_date >= "2025-12-16")

#Filter duplicates to articles with highest character url_text, or read in articles
the_data_to_code <- the_data_to_code %>%
  mutate(num_chars = nchar(url_text)) %>%
  group_by(pub_date, title, source) %>%
  slice_max(num_chars, n = 1) %>%
  ungroup() %>%
  unique() %>%
  select(-num_chars)

#formatting data into human coding spreadsheet
human_coding_spreadsheet <- the_data_to_code %>%
  select(-c(url_text, description, url_text_original)) %>%
  #changing names of the following columns
 rename(`FULL DATE` = pub_date,
         HEADLINE = title,
         LINK = URL,
         `ARTICLE SOURCE` = source,
         `ARTICLE DESCRIPTION` = description_original) %>%
  #ensure date of publication is formatted as a date
  mutate(`FULL DATE` = as.Date(`FULL DATE`),
         #agencies involved and coders are blank at this stage
         `AGENCIES INVOLVED` = gov_agency,
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
         `Other (write-in)` = 0,                         
         Threatened = 0,                               
         Completed = 0,                                
         `Attack on Science Reversal` = 0)


#Bring in existing human coding spreadsheet
aos_dataframe <- read_csv("C:/AOS_db/data/07_coding_spreadsheet.csv") %>%
  mutate(`FULL DATE` = as.Date(`FULL DATE`, format = "%m/%d/%Y"))

human_coding_spreadsheet <- bind_rows(aos_dataframe, human_coding_spreadsheet)

##Remove duplicates if they show up

human_coding_spreadsheet <- human_coding_spreadsheet %>%
  group_by(`FULL DATE`, HEADLINE, LINK, `ARTICLE SOURCE`, `ARTICLE DESCRIPTION`) %>%
  arrange(desc(`AOS PRESENCE`)) %>%
  slice_head(n = 1)
  
### Write Data for Humans to Code ###
#create new dataset with formatted spreadsheet
write_csv(human_coding_spreadsheet, "C:/AOS_db/data/07_coding_spreadsheet.csv")
