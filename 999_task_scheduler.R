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

taskscheduler_create(taskname = "01_ReadInrssfeed",
                     rscript = myscript,
                     schedule = "DAILY",
                     startdate = current_date,
                     starttime = "06:00")

#taskscheduler_delete(taskname = "ReadInrssfeed")

# Daily screening of rss feed
myscript <- "C:/AOS_db/r_scripts/02_scan_descriptions_keywords.R"
taskscheduler_create(taskname = "02_Screenrssfeed",
                     rscript = myscript,
                     schedule = "DAILY",
                     startdate = current_date,
                     starttime = "07:00")

# Daily reading articles from URL for full text screening
myscript <- "C:/AOS_db/r_scripts/04_read_screened_urls.R"
taskscheduler_create(taskname = "04_ReadScreened_urls",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     days = "WED",
                     startdate = current_date,
                     starttime = "22:00")

# Daily saving pdfs for articles that weren't properly scraped for full text scraping
myscript <- "C:/AOS_db/r_scripts/05_read_in_saved_pdfs.R"
taskscheduler_create(taskname = "05_read_in_saved_pdfs",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     days = "THU",
                     startdate = current_date,
                     starttime = "22:00")

# Weekly full text screen of all scraped and pdf articles
myscript <- "C:/AOS_db/r_scripts/06_screen_in_read_urls_full_search_terms.R"
taskscheduler_create(taskname = "06_screen_articles_full_search_terms",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:00",
                     days = "FRI")

# Weekly cleaning of full text screen of all scraped and pdf articles
myscript <- "C:/AOS_db/r_scripts/07_clean_up_raw_aos_text.R"
taskscheduler_create(taskname = "07_clean_up_raw_aos_text",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:00",
                     days = "SAT")

# Weekly tagging of full text screen for mentioned gov agencies
myscript <- "C:/AOS_db/r_scripts/08_tag_govt_agency.R"
taskscheduler_create(taskname = "08_tag_govt_agency",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:10",
                     days = "SAT")

# Weekly tagging of full text screen for mention of gold standar science and scientifi integrity
myscript <- "C:/AOS_db/r_scripts/09_tag_SI_GSS.R"
taskscheduler_create(taskname = "09_tag_SI_GSS",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:40",
                     days = "SAT")

# Weekly tagging of full text screen for mention of gold standar science and scientifi integrity
myscript <- "C:/AOS_db/r_scripts/10_aggregate_aos_date_coding.R"
taskscheduler_create(taskname = "10_aggregate_aos_date_coding",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:50",
                     days = "SAT")

# Weekly cleaning of full text screen of all scraped and pdf articles
myscript <- "C:/AOS_db/r_scripts/11_human_coding_spreadsheet.R"
taskscheduler_create(taskname = "11_human_coding_spreadsheet",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "09:00",
                     days = "SUN")

# Weekly cleaning of full text screen of all scraped and pdf articles
myscript <- "C:/AOS_db/r_scripts/11a_remove_coded_pdfs.R"
taskscheduler_create(taskname = "11a_remove_coded_pdfs",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "18:00",
                     days = "SUN")

# Weekly assign unique IDs to coding spreadsheet
myscript <- "C:/AOS_db/r_scripts/12_flag_aos_multiples.R"
taskscheduler_create(taskname = "12_flag_aos_multiples",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:00",
                     days = "SUN")


# Weekly assign unique IDs to coding spreadsheet
myscript <- "C:/AOS_db/r_scripts/13_assign_unique_id.R"
taskscheduler_create(taskname = "13_assign_unique_id",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "09:00",
                     days = "SUN")

# Weekly saving citations
myscript <- "C:/AOS_db/r_scripts/14_save_aos_citations.R"
taskscheduler_create(taskname = "14_save_aos_citations",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "11:00",
                     days = "SUN")


# Weekly tag potential si violations
myscript <- "C:/AOS_db/r_scripts/15_tag_si_violations.R"
taskscheduler_create(taskname = "15_tag_si_violations",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "10:30",
                     days = "SUN")



# Weekly add summaries and titles
myscript <- "C:/AOS_db/r_scripts/16_add_title_summary_lists.R"
taskscheduler_create(taskname = "16_add_title_summary_lists",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "11:30",
                     days = "SUN")


# Weekly add formatted date
myscript <- "C:/AOS_db/r_scripts/17_modify_date.R"
taskscheduler_create(taskname = "17_modify_date",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "12:00",
                     days = "SUN")

myscript <- "C:/AOS_db/r_scripts/18_combine_human_rss_coding_data_sets.R"
taskscheduler_create(taskname = "18_combine_human_rss_coding_data_sets",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "13:00",
                     days = "SUN")


myscript <- "C:/AOS_db/r_scripts/19_make_a_workbook.R"
taskscheduler_create(taskname = "19_make_a_workbook",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "14:00",
                     days = "SUN")


# Biweekly back up of data and r scripts -- Friday edition
current_date <- format(Sys.Date(), "%m/%d/%Y")
myscript <- "C:/AOS_db/r_scripts/999_save_to_back_up.R"
taskscheduler_create(taskname = "Fridaybackup",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     days = "FRI",
                     starttime = "01:00")

# Biweekly back up of data and r scripts -- Monday edition

myscript <- "C:/AOS_db/r_scripts/999_save_to_back_up.R"
taskscheduler_create(taskname = "Mondaybackup",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     days = "MON",
                     starttime = "01:00")


# Monthly pdf deletion

myscript <- "C:/AOS_db/r_scripts/999_delete_screenedout_pdfs.R"
taskscheduler_create(taskname = "pdfcleanup",
                     rscript = myscript,
                     schedule = "MONTHLY",
                     startdate = current_date,
                     days = 1,
                     starttime = "17:00")

