#loading packages
pacman::p_load(tidyverse, tidytext, janitor, purrr, rvest, R.utils, polite)

#read in data
aos_0 <- readxl::read_excel("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/data/Trump 2.0 Potential AOS.xlsx") %>% 
  janitor::clean_names() %>% 
  select(headline, link) %>% 
  mutate(caseid = row_number()) %>% 
  filter(caseid != 37,
         caseid != 49, 
         caseid != 68,
         caseid != 83,
         caseid != 95,
         caseid != 104,
         caseid != 166,
         caseid != 168,
         caseid != 179)

grab_text <- function(url_test){
  url_test <- polite::bow(url_test, force = TRUE) %>% 
    scrape(.) %>%
    html_node("body") %>%
    html_text2() %>%
    as.character()}

grab_text2 <- possibly(grab_text, otherwise = NA)

aos_all <- data.frame()
ticker <- 0

for(i in 1:nrow(aos_0)){
  aoses_split <- slice(aos_0, i)
  aoses_split <- withTimeout(mutate(aoses_split, url_text = map(link, grab_text2, .progress = TRUE)), timeout = 100)
  aos_all <- bind_rows(aos_all, aoses_split)
  ticker <- ticker + 1
  print(ticker)
  Sys.sleep(5)
}


aoses_text <- aos_all %>%
  mutate(url_text = str_remove_all(url_text, "\\\n"),
         url_text = str_remove_all(url_text, "[0|1|2|3|4|5|6|7|8|9|-|?|#|%|,|=|_|&|:|â|€|™|`|'|}|{|]|!|/*]"),
         url_text = str_remove_all(url_text, "\""),
         url_text = str_remove_all(url_text, "\\\\"),
         url_text = str_remove_all(url_text, "\\/"),
         url_text = str_remove_all(url_text, "\\\r"))

write_csv(aoses_text, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/data/jules_csv.csv")

write_csv(aos_all, "C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/data/jules_csv_all_jic.csv")
