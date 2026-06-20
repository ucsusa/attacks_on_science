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
myscript <- "C:/AOS_db/r_scripts/04_read_screened_urls.R"
taskscheduler_create(taskname = "04_ReadScreened_urls_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     days = "WED",
                     startdate = current_date,
                     starttime = "22:15")

# Daily saving pdfs for articles that weren't properly scraped for full text scraping
myscript <- "C:/AOS_db/r_scripts/05_read_in_saved_pdfs.R"
taskscheduler_create(taskname = "05_read_in_saved_pdfs_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     days = "THU",
                     startdate = current_date,
                     starttime = "22:15")

# Weekly full text screen of all scraped and pdf articles
myscript <- "C:/AOS_db/r_scripts/06_screen_in_read_urls_full_search_terms.R"
taskscheduler_create(taskname = "06_screen_articles_full_search_terms_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:15",
                     days = "FRI")

# Weekly cleaning of full text screen of all scraped and pdf articles
myscript <- "C:/AOS_db/r_scripts/07_clean_up_raw_aos_text.R"
taskscheduler_create(taskname = "07_clean_up_raw_aos_text_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:15",
                     days = "SAT")

# Weekly tagging of full text screen for mentioned gov agencies
myscript <- "C:/AOS_db/r_scripts/08_tag_govt_agency.R"
taskscheduler_create(taskname = "08_tag_govt_agency_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:20",
                     days = "SAT")

# Weekly tagging of full text screen for mention of gold standard science and scientific integrity
myscript <- "C:/AOS_db/r_scripts/09_tag_SI_GSS.R"
taskscheduler_create(taskname = "09_tag_SI_GSS_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:50",
                     days = "SAT")

# Aggregate/organize potential AOS articles by date
myscript <- "C:/AOS_db/r_scripts/10_aggregate_aos_date_coding.R"
taskscheduler_create(taskname = "10_aggregate_aos_date_coding_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:55",
                     days = "SAT")

# Formatting potential AOS into human coding spreadsheet
myscript <- "C:/AOS_db/r_scripts/11_human_coding_spreadsheet.R"
taskscheduler_create(taskname = "11_human_coding_spreadsheet_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "09:15",
                     days = "SUN")

# Weekly cleaning of full text screen of all scraped and pdf articles
myscript <- "C:/AOS_db/r_scripts/11a_remove_coded_pdfs.R"
taskscheduler_create(taskname = "11a_remove_coded_pdfs_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "18:00",
                     days = "SUN")


# Weekly assign unique IDs to coding spreadsheet
myscript <- "C:/AOS_db/r_scripts/12_flag_aos_multiples.R"
taskscheduler_create(taskname = "12_flag_aos_multiples_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "08:15",
                     days = "SUN")

#taskscheduler_delete(taskname = "12_flag_aos_multiples_jbd")

# Weekly assign unique IDs to coding spreadsheet
myscript <- "C:/AOS_db/r_scripts/13_assign_unique_id.R"
taskscheduler_create(taskname = "13_assign_unique_id_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "09:45",
                     days = "SUN")

#taskscheduler_delete(taskname = "13_assign_unique_id_jbd")

# Weekly saving citations
myscript <- "C:/AOS_db/r_scripts/14_save_aos_citations.R"
taskscheduler_create(taskname = "14_save_aos_citations_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "10:15",
                     days = "SUN")

#taskscheduler_delete(taskname = "14_save_aos_citations_jbd")

# Weekly tag potential si violations
myscript <- "C:/AOS_db/r_scripts/15_tag_si_violations.R"
taskscheduler_create(taskname = "15_tag_si_violations_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "10:45",
                     days = "SUN")

#taskscheduler_delete(taskname = "15_tag_si_violations_jbd")

# Weekly add summaries and titles
myscript <- "C:/AOS_db/r_scripts/16_add_title_summary_lists.R"
taskscheduler_create(taskname = "16_add_title_summary_lists_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "11:15",
                     days = "SUN")

#taskscheduler_delete(taskname = "16_add_title_summary_lists_jbd")

# Weekly add formatted date
myscript <- "C:/AOS_db/r_scripts/17_modify_date.R"
taskscheduler_create(taskname = "17_modify_date_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "11:45",
                     days = "SUN")

#taskscheduler_delete(taskname = "17_modify_date_jbd")

# Combine rss and human coded data
myscript <- "C:/AOS_db/r_scripts/18_combine_human_rss_coding_data_sets.R"
taskscheduler_create(taskname = "18_combine_human_rss_coding_data_sets_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "13:30",
                     days = "SUN")

#taskscheduler_delete(taskname = "18_combine_human_rss_coding_data_sets_jbd")

#Creating dataset for Power BI
myscript <- "C:/AOS_db/r_scripts/19_make_a_workbook.R"
taskscheduler_create(taskname = "19_make_a_workbook_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     starttime = "14:30",
                     days = "SUN")

#taskscheduler_delete(taskname = "19_make_a_workbook_jbd")

# Biweekly back up of data and r scripts -- Friday edition
current_date <- format(Sys.Date(), "%m/%d/%Y")
myscript <- "C:/AOS_db/r_scripts/999_save_to_back_up.R"
taskscheduler_create(taskname = "Fridaybackup_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     days = "FRI",
                     starttime = "01:15")

# Biweekly back up of data and r scripts -- Monday edition

myscript <- "C:/AOS_db/r_scripts/999_save_to_back_up.R"
taskscheduler_create(taskname = "Mondaybackup_jbd",
                     rscript = myscript,
                     schedule = "WEEKLY",
                     startdate = current_date,
                     days = "MON",
                     starttime = "01:15")

# Monthly pdf deletion

myscript <- "C:/AOS_db/r_scripts/999_delete_screenedout_pdfs.R"
taskscheduler_create(taskname = "pdfcleanup_jbd",
                     rscript = myscript,
                     schedule = "MONTHLY",
                     startdate = current_date,
                     days = 1,
                     starttime = "17:00")