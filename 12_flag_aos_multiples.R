#### R SCRIPT PURPOSE: 
#### Runs a second duplicate check: flags articles coded in the same way and occur within +/- 2 days.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse,
       janitor,
       readxl,
       scales,
       blastula,
       quanteda,
       keyring,
       gridExtra,
       grid) 


all_the_data <- read_excel("C:/AOS_db/data/11_coding_spreadsheet.xlsx") %>%
  mutate(`FULL DATE` = as.Date(`FULL DATE`, format = "%m/%d/%Y"),
         news_cycle = interval(as.Date(`FULL DATE`) - 2, as.Date(`FULL DATE`) + 2)) %>%
  filter(`AOS PRESENCE` == 1,
         `FULL DATE` > "2025-12-19") %>%
  clean_names()

all_the_data_groups <- all_the_data %>%
  group_by(agencies_involved, si_mention, gss_mention, agency_appointments, rules_regulations_orders, scientific_advisory_committees_boards, altering_study_results, data_accessibility, data_collection, politicization_of_grants_and_funding, censorship, losing_positions, restrictions_from_professional_engagement, targeting_scientists_based_on_identity, climate_science, elections_and_voting, energy, environment, equity, health_safety, any, threatened, completed) %>%
  mutate(group_count = n(),
         group_num = 1:n(),
         group_id = cur_group_id(),
         article_description = ifelse(is.na(article_description), headline, article_description),
         article_description = gsub("ap news|AP News|nbsp", "", article_description)) %>%
  ungroup()

all_the_data_agg <- all_the_data_groups %>%
  filter(group_count > 1)

all_the_data_no_agg  <- all_the_data_groups %>%
  filter(group_count == 1)
  

#Check if the groups overlap in news cycle
filtered_groups <- unique(all_the_data_agg$group_id)

grouped_aoses_all <- data.frame()

for(i in filtered_groups){
  all_the_data_fil <- all_the_data_groups %>%
    filter(group_id == i)
  grp_nums <- unique(all_the_data_fil$group_num)
  for(j in grp_nums){
    all_the_data_fil_j <- all_the_data_fil %>%
      filter(group_num == j)
    for(k in grp_nums){
      all_the_data_fil_k <- all_the_data_fil %>%
        filter(group_num == k)
      if(j != k){
        if(int_overlaps(all_the_data_fil_j$news_cycle, all_the_data_fil_k$news_cycle)){
          grouped_aoses_all <- bind_rows(grouped_aoses_all, all_the_data_fil) %>% distinct()
        }}}}}

#Add the groups not in the same news cycle back into the non-grouped data frame
ungrouped_aoses_all <- all_the_data_agg %>%
  filter(!link %in% grouped_aoses_all$link)

all_the_data_no_agg <- bind_rows(all_the_data_no_agg, ungrouped_aoses_all)


#Add an identifier to track description comparisons
aoses_clean <- grouped_aoses_all %>%
  mutate(compareobjectid = paste0("DESC", str_pad(as.character(group_id), width = 5, side = "left", pad = "0")))

compareobjectid <- aoses_clean$compareobjectid
all_combos_groups <- data.frame()

for(k in compareobjectid){
  aoses_clean_split <- filter(aoses_clean, compareobjectid == k)
  
  descriptions <- aoses_clean_split %>%
    select(article_description, compareobjectid) %>%
    rename(text = article_description) %>%
    mutate(text = tolower(text)) %>%
    mutate(text = gsub("\uFFFD", " ", text))
  
  descriptions_corpus <- corpus(descriptions)
  
  descriptions_swf <- tokens(descriptions_corpus, 
                             what = "word",
                             remove_punct = TRUE,
                             remove_symbols = TRUE,
                             remove_numbers = TRUE,
                             remove_separators = TRUE,
                             split_hyphens = TRUE,
                             padding = TRUE)
  
  descriptions_swf <- tokens_select(descriptions_swf, 
                                    pattern = stopwords("en", source = "snowball"), 
                                    selection = "remove")
  
  descriptions_swf <- tokens_wordstem(
    descriptions_swf,
    language = quanteda_options("language_stemmer"),
    verbose = quanteda_options("verbose"))
  
  num_docs <- ndoc(descriptions_swf)
  
  descriptions_long <- data.frame(
    doc_id = rep(names(descriptions_swf), lengths(descriptions_swf)),
    text = unlist(descriptions_swf, use.names = FALSE),
    row.names = NULL, stringsAsFactors = FALSE)
  
  descriptions_long <- descriptions_long %>%
    filter(text != "") %>%
    group_by(doc_id) %>%
    mutate(total_num_text = n()) %>%
    ungroup() %>%
    group_by(text) %>%
    mutate(grp_num_text = n()) %>%
    ungroup() %>%
    filter(grp_num_text > 1) %>%
    group_by(grp_num_text) %>%
    mutate(grp_num_words = length(unique(text)),
           pct_same_words = grp_num_words/total_num_text)
  
  if(max(descriptions_long$pct_same_words) >= 0.3){
    all_combos_groups <- bind_rows(aoses_clean_split, all_combos_groups)
      }}


all_combos_groups <- all_combos_groups %>%
  unique()



#Where there are multiples, pull the article to go into the database using the source prioritization list
#The New York Times and the Washington Post are legacy sources and are no longer used
#If the sources are the same, choose the chronologically first article

priority_sources <- c("The Hill", "Associated Press", "Stat News", "E&E News", "Stateline Democracy", "Gov Exec", "National Broadcasting Corporation", "National Public Radio", "New York Times", "Washington Post")

aoses_clean_agg_final <- all_combos_groups %>%
  mutate(article_source = factor(article_source, levels = priority_sources)) %>%
  group_by(compareobjectid, agencies_involved) %>%
  slice_max(order_by = article_source) %>%
  ungroup() %>%
  group_by(compareobjectid, agencies_involved) %>%
  slice_min(order_by = full_date) %>%
  ungroup()

#Pull in already-tested articles
aoses_clean_agg_final <- bind_rows(aoses_clean_agg_final, all_the_data_no_agg) %>%
  distinct() %>%
  mutate(article_source = factor(article_source, levels = priority_sources)) %>%
  group_by(headline, article_description, full_date, link, article_source) %>%
  arrange(article_source) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-c(group_count, group_num, group_id, compareobjectid, news_cycle)) %>%
  distinct()

write_csv(aoses_clean_agg_final, "C:/AOS_db/data/12_aoses_clean_no_multiples.csv")
