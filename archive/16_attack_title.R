#loading packages
pacman::p_load(tidyverse, stringi)

#read in data
df0 <- read_csv("C:/AOS_db/data/14_aos_summaries.csv")

df1 <- df0 %>% 
  mutate(agencies_involved = str_replace_all(agencies_involved, ";", ","),
         agencies_involved = str_replace_all(agencies_involved, "CONGRESS", "Congress"), 
         agencies_involved = str_replace_all(agencies_involved, "WH", "White House"),
         attack_type = str_replace(attack_type, "(.*),", "\\1 &"),
         attack_type = str_replace_all(attack_type, pattern = c('Agency Appointments'= 'anti-science agency appointment',
                                                                                   'Rules/Regulations/Orders' = 'anti-science rule, regulation, or order',
                                                                                   'Scientific Advisory Committees/Boards' = 'conflicted or delayed science advisory committees',
                                                                                   'Altering Study Results' = 'altering study results',
                                                                                   'Data Accessibility' = 'reducing data accessibility',
                                                                                   'Data Collection' = 'stopping or modifying data collection',
                                                                                   'Politicization of Grants and Funding' = 'politicizing grants or funding',
                                                                                   'Censorship' = 'censoring federal scientists',
                                                                                   'Losing Positions' = 'terminating or demoting federal scientists',
                                                                                   'Restrictions from Professional Engagement' = 'restricting professional engagement',
                                                                                   'Targeting Scientists based on Identity' = 'targeting scientists based on their identities')),
         attack_topic = tolower(attack_topic),
         attack_topic = str_replace_all(attack_topic, "public health & safety", "health & safety"),
         attack_topic = str_replace_all(attack_topic, "environmental", "the environment"),
         attack_topic = str_replace_all(attack_topic, "environment", "the environment"))

##Make a dataframe for any
any_df <- data.frame(any_variable = "any", attack_topic = c("climate science", "elections and voting", "energy", "the environment", "equity", "health & safety"), stringsAsFactors = FALSE)

df2 <- left_join(df1, any_df, by = c("attack_topic" = "any_variable"), relationship = "many-to-many")

df3 <- df2 %>%
  mutate(attack_topic = ifelse(attack_topic == "any", attack_topic.y, attack_topic)) %>%
  select(-attack_topic.y)

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

firstup <- function(x) {
  # Convert the entire string to lowercase first, if needed
  # x <- tolower(x) 
  # Capitalize the first character and combine with the rest of the string
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

##Work with comma delimited lists and add oxford comma where appropriate or 'and'
df4 <- df3 %>%
  group_by(agg_objectid) %>%
  mutate(attack_summary_topic_list = map(attack_topic, ~make_a_list(unique(attack_topic))),
         attack_summary_type_list = map(attack_type, ~make_a_list(unique(attack_type))),
         enacted_list = map(enacted, ~make_a_list(unique(enacted))),
         agencies_involved_list = map(agencies_involved, ~make_a_list(agencies_involved)),
         attack_attacks = ifelse(str_count(agencies_involved, ",") > 1, 'attack', 'attacks')) %>%
  ungroup()

df5 <- df4 %>% 
  group_by(agg_objectid, si_mention, gss_mention, potential_si_violation, week_month_year) %>%
  unique() %>%
  summarise(attack_title = paste(unique(agencies_involved_list), unique(attack_attacks), "science with", unique(attack_summary_type_list), "negatively impacting", unique(attack_summary_topic_list)),
            agencies_involved = paste0(unique(agencies_involved), collapse = ", "),
            enacted_list = paste0(unique(enacted_list), collapse = ", "),
            attack_topic = paste0(unique(attack_summary_topic_list), collapse = ", "),
            attack_type = paste0(unique(attack_summary_type_list), collapse = ", "),
            attack_summary = paste0(firstup(unique(article_description)), ".")) %>%
  ungroup()

write.csv(df5, "C:/AOS_db/data/16_aos_titles.csv")
