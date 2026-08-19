##This script tags article text that mentions gold standard science or scientific integrity and saves a csv with those columns

library(tidyverse)

#Read in data
df0 <- read_csv("C:/AOS_db/data/06_aos_clean_gov.csv")


#tagging when SI and GSS are mentioned in the article text
df1  <-  df0 %>%  
  mutate(SI_mention = str_detect(url_text, "scientific integrity"), #writing code to detect mentions of SI in text
         GSS_mention = str_detect(url_text, "gold standard science|gold-standard science|golden standard of science")) #writing code to detect mentions of GSS in text

write_csv(df1, "C:/AOS_db/data/06_aos_clean_gov_si_gss.csv")

