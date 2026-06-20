#### R SCRIPT PURPOSE: 
#### Tags articles (TRUE/FALSE) with mentions of gold standard science or scientific integrity in article text.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 

p_load(tidyverse)

aos_clean_agency <- read_csv("C:/AOS_db/data/08_aos_clean_gov.csv")

aos_si_gss <- aos_clean_agency %>%  
  mutate(SI_mention = str_detect(url_text, "scientific integrity"), 
         GSS_mention = str_detect(url_text, "gold standard science|gold-standard science|golden standard of science")) 

write_csv(aos_si_gss, "C:/AOS_db/data/09_aos_clean_gov_si_gss.csv")

