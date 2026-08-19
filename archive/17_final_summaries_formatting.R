library(tidyverse)

df0 <- read_csv("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/updating_formatting_human_coding_results/results/12_attack_title.csv")

##Add functions

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

df1 <- df0 %>%
  group_by(agg_objectid) %>%
  mutate(attack_summary_topic_list = map(attack_summary_topic, ~make_a_list(attack_summary_topic)),
         attack_summary_type_list = map(attack_summary_type, ~make_a_list(attack_summary_type)),
         attack_summary_completion_list = map(attack_summary_completion, ~make_a_list(attack_summary_completion))) %>%
  ungroup()

df2 <- df1 %>%
  rowwise() %>%
  mutate(attack_summary = paste0(map(attack_summary, ~make_first_letter_lowercase(unique(attack_summary))), collapse = " ")) %>%
  group_by(agg_objectid, agencies_involved, si_mention, gss_mention, potential_si_violation, attack_title) %>%
  summarise(attack_summary_final = paste("This attack includes", unique(attack_summary_type_list), ". This reduces or undermines federal scientific capacity or knowledge", unique(attack_summary_topic_list), ". Those at risk of being impacted by this attack", unique(attack_summary_completion_list), ". Looking more specifically, ", unique(attack_summary)),
            attack_summary_generic = paste("This attack includes", unique(attack_summary_type_list), ". This reduces or undermines federal scientific capacity or knowledge", unique(attack_summary_topic_list), ". Those at risk of being impacted by this attack", unique(attack_summary_completion_list), ".")) %>%
  ungroup() %>%
  distinct()

##Reformat to include all topics and types with coding.
df11 <- read_csv("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/updating_formatting_human_coding_results/results/11_aoses_summarized_process_defs.csv") %>%
  select(agg_objectid, attack_type_variable, attack_topic_variable, attack_completion) %>%
  mutate(attack_type_code = 1) %>%
  pivot_wider(names_from = attack_type_variable, values_from = attack_type_code, values_fill = 0) %>%
  mutate(attack_topic_code = 1) %>%
  pivot_wider(names_from = attack_topic_variable, values_from = attack_topic_code, values_fill = 0) %>%
  mutate(attack_completion_code = 1) %>%
  pivot_wider(names_from = attack_completion, values_from = attack_completion_code, values_fill = 0)

df3 <- left_join(df2, df11)

write.csv(df3, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/updating_formatting_human_coding_results/results/13_attack_title_summary.csv")
