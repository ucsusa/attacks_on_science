#### R SCRIPT PURPOSE: 
#### Establishes TaskScheduler to automate the rest of the script train 
#### Runs (?) How often will this script run?


### Logistics for R Script ###


#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(taskscheduleR)

#establish today's date
current_date <- format(Sys.Date(), "%m/%d/%Y")

# Daily grab rss feed
# Define the path to your R script
myscript <- "C:/AOS_db/r_scripts/01_read_in_rssfeed.R"

taskscheduler_create(taskname = "01_ReadInrssfeed_jbd",
                     rscript = myscript,
                     schedule = "DAILY",
                     startdate = current_date,
                     starttime = "06:30")

#taskscheduler_delete(taskname = "01_ReadInrssfeed_jbd")

# Daily screening of rss feed
myscript <- "C:/AOS_db/r_scripts/02_scan_descriptions_keywords.R"
taskscheduler_create(taskname = "02_Screenrssfeed_jbd",
                     rscript = myscript,
                     schedule = "DAILY",
                     startdate = current_date,
                     starttime = "07:30")

#taskscheduler_delete(taskname = "02_Screenrssfeed_jbd")

# Daily reading articles from URL for full text screening
myscript <- "C:/AOS_db/r_scripts/03_read_screened_urls.R"
taskscheduler_create(taskname = "03_ReadScreened_urls_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     days = "WED",
                     startdate = current_date,
                     starttime = "08:30")

#taskscheduler_delete(taskname = "03_ReadScreened_urls_jbd")

# Daily reading articles from URL for full text screening
myscript <- "C:/AOS_db/r_scripts/03_5_read_archived_articles.R"
taskscheduler_create(taskname = "03_5_ReadArchivedArticles_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     days = "TUE",
                     startdate = current_date,
                     starttime = "08:30")

#taskscheduler_delete(taskname = "03_5_ReadArchivedArticles_jbd")

# Daily saving pdfs for articles that weren't properly scraped for full text scraping
myscript <- "C:/AOS_db/r_scripts/04_read_in_saved_pdfs.R"
taskscheduler_create(taskname = "04_read_in_saved_pdfs_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     days = "THU",
                     startdate = current_date,
                     starttime = "08:30")

#taskscheduler_delete(taskname = "04_read_in_saved_pdfs_jbd")

# Weekly full text screen of all scraped and pdf articles
myscript <- "C:/AOS_db/r_scripts/05_screen_in_read_urls_full_search_terms.R"
taskscheduler_create(taskname = "05_screen_articles_full_search_terms_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:30",
                     days = "FRI")

#taskscheduler_delete(taskname = "05_screen_articles_full_search_terms_jbd")

# Weekly cleaning of full text screen of all scraped and pdf articles
myscript <- "C:/AOS_db/r_scripts/06_clean_up_raw_aos_text.R"
taskscheduler_create(taskname = "06_clean_up_raw_aos_text_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:30",
                     days = "SAT")

#taskscheduler_delete(taskname = "06_clean_up_raw_aos_text_jbd")

# Weekly tagging of full text screen for mentioned gov agencies
myscript <- "C:/AOS_db/r_scripts/06_tag_govt_agency.R"
taskscheduler_create(taskname = "06_tag_govt_agency_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:15",
                     days = "SAT")

#taskscheduler_delete(taskname = "06_tag_govt_agency_jbd")

# Weekly tagging of  full text screen for gold standard science and scientific integrity mentions
myscript <- "C:/AOS_db/r_scripts/06_tag_SI_GSS.R"
taskscheduler_create(taskname = "06_tag_SI_GSS_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:45",
                     days = "SAT")

#taskscheduler_delete(taskname = "06_tag_SI_GSS_jbd")

# Weekly cleaning of full text screen of all scraped and pdf articles
myscript <- "C:/AOS_db/r_scripts/07_human_coding_spreadsheet.R"
taskscheduler_create(taskname = "07_human_coding_spreadsheet_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "09:30",
                     days = "SAT")

#taskscheduler_delete(taskname = "07_human_coding_spreadsheet_jbd")

# Biweekly back up of data and r scripts -- Friday edition
current_date <- format(Sys.Date(), "%m/%d/%Y")
myscript <- "C:/AOS_db/r_scripts/999_save_to_back_up.R"
taskscheduler_create(taskname = "Fridaybackup_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     days = "FRI",
                     starttime = "01:30")

#taskscheduler_delete(taskname = "Fridaybackup_jbd")

# Biweekly back up of data and r scripts -- Monday edition

myscript <- "C:/AOS_db/r_scripts/999_save_to_back_up.R"
taskscheduler_create(taskname = "Mondaybackup_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     days = "MON",
                     starttime = "01:30")

#taskscheduler_delete(taskname = "Mondaybackup_jbd")