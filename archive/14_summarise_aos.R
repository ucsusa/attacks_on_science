##This script pulls in the fully human-coded results, that have reformatted dates and potential SI violations coded, and uses the rss feed description together with our definitions to summarize each attack.

library(tidyverse)
library(scales)
library(janitor)

df0 <- read_csv("C:/AOS_db/data/12_fully_coded_url_read_clean_tagged_dates.csv") 

df1 <- df0 %>%
  mutate(aos_summary = sub("\\\\&nbsp|AP News|ap news", "", `ARTICLE DESCRIPTION`)) %>%
  clean_names()

#This function makes a list of strings comma delimited and adds and if more than 2 in a list.
make_a_list <- function(x){
  if (length(unique(x)) == 0) {
    formatted_string <- ""
  } else if (length(unique(x)) == 1) {
    formatted_string <- x[1]
  } else {
    # Paste all elements except the last one with ", " as a separator
    first_part <- paste(x[-length(unique(x))], collapse = ", ")
    # Combine the first part with "and" and the last element
    formatted_string <- paste0(first_part, ", and ", x[length(unique(x))])
  }
}

#This function makes the first letter in a sentence lower case.
make_first_letter_lowercase <- function(x) {
  first_letter <- tolower(substr(x, 1, 1))
  rest_of_string <- substr(x, 2, nchar(x))
  return(paste0(first_letter, rest_of_string, collapse = " "))
}

df2_type <- df1 %>%
  group_by(headline, full_date, link, article_source, article_description, si_mention, gss_mention, agencies_involved, coders, aos_presence, agg_objectid, potential_si_violation, week_month_year) %>%
  filter(type_response == 1) %>%
  summarise(attack_type = paste(unique(type), collapse = ", "))


df2_topic <- df1 %>%
  group_by(headline, full_date, link, article_source, article_description, si_mention, gss_mention, agencies_involved, coders, aos_presence, agg_objectid, potential_si_violation, week_month_year) %>%
  filter(topic_response == 1) %>%
  summarise(attack_topic = paste(unique(topic), collapse = ", "))

df2_enacted <- df1 %>%
  group_by(headline, full_date, link, article_source, article_description, si_mention, gss_mention, agencies_involved, coders, aos_presence, agg_objectid, potential_si_violation, week_month_year) %>%
  filter(enacted_value == 1) %>%
  summarise(enacted = paste(unique(enacted), collapse = ", "))


df2 <- left_join(df2_topic, df2_type)

df2 <- left_join(df2, df2_enacted)


write.csv(df2, "C:/AOS_db/data/14_aos_summaries.csv", fileEncoding = "UTF-8")
