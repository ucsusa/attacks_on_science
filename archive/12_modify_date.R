##This script modifies the date to a week, month, and a year.

library(tidyverse)
library(scales)

fully_coded_read <- read_csv("C:/AOS_db/data/11_aoses_summarized_process_defs.csv")


fully_coded_read <- fully_coded_read %>%
  rowwise() %>%
  mutate(date = as.Date(`FULL DATE`, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")),
         week_of_year_date = isoweek(date),
         date = as.Date(date, format = "%Y-%m-%d"),
         `FULL DATE` = format(date, "%m/%d/%Y"),
         first_day_of_month = floor_date(date, "month"),
         week_of_year_first_day = isoweek(first_day_of_month),
         week_of_month = week_of_year_date - week_of_year_first_day + 1,
         week_month_year = paste0(week_of_month %>% ordinal(), ", ", month(date, label = TRUE), ", ", year(date))) %>%
  select(-c(week_of_year_date, date, first_day_of_month, week_of_month, week_of_year_first_day))

write_csv(fully_coded_read, "C:/AOS_db/data/12_fully_coded_url_read_clean_tagged_dates.csv")
