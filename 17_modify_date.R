#### R SCRIPT PURPOSE: 
#### Modifies the date of reporting to a week, month, and a year.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse, 
       scales) 

fully_coded_read <- read_csv("C:/AOS_db/data/16_aos_titles_summaries.csv")

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

write_csv(fully_coded_read, "C:/AOS_db/data/17_fully_coded_url_read_clean_tagged_dates.csv")
