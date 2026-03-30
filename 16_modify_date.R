##This script modifies the date to a week, month, and a year.

#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data
       scales) #to read, format, create etc. excel files

fully_coded_read <- read_csv("C:/AOS_db/data/15_aos_titles_summaries.csv")


fully_coded_read <- fully_coded_read %>%
  select(headline, full_date, link, article_source, article_description, si_mention, gss_mention, agencies_involved, coders, aos_presence, agg_objectid, potential_si_violation, attack_topic_list, attack_type_list, enacted_list, agencies_involved_list, attack_title, attack_summary) %>%
  rowwise() %>%
  mutate(date = as.Date(full_date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")),
         week_of_year_date = isoweek(date),
         date = as.Date(date, format = "%Y-%m-%d"),
         full_date = format(date, "%m/%d/%Y"),
         first_day_of_month = floor_date(date, "month"),
         week_of_year_first_day = isoweek(first_day_of_month),
         week_of_month = week_of_year_date - week_of_year_first_day + 1,
         week_month_year = paste0(week_of_month %>% ordinal(), ", ", month(date, label = TRUE), ", ", year(date))) %>%
  select(-c(week_of_year_date, date, first_day_of_month, week_of_month, week_of_year_first_day))

write_csv(fully_coded_read, "C:/AOS_db/data/16_fully_coded_url_read_clean_tagged_dates.csv")
