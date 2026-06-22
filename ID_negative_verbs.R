#### R SCRIPT PURPOSE: 
#### Identified most common verbs used in articles (and their headlines) from Jan-Apr 2025 that humans confirmed contained an attack on science. This was used to inform the initial list of negative verb search terms.

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 

p_load(tidyverse,
       rebus, 
       tidytext,
       quanteda,
       udpipe) 

#read in data: Sample of AOS from Jan 20 to April-ish
aos_0 <- read_csv("/Attacks on Science Database/First 6 Months.csv") %>% 
  janitor::clean_names()


### Annotating Parts of Speech ###


## Focusing on article text

#Cleaning up text - getting rid of punctuation and changing to lowercase
aos <- aos_0 %>% 
  mutate(desc = str_remove_all(url_text, "[[:punct:]]"),
         desc = tolower(desc)) %>% 
  unnest_tokens(word, desc) %>% 
  anti_join(stop_words) %>% 
  select(word)

#Downloading UD model trained on English web language and appending it to df
#(Chose English web language since text is from online news articles)
ud_model <- udpipe_download_model(language = "english-ewt")

ud_model <- udpipe_load_model(ud_model$file_model)

#Annotating parts of speech of each word in article text
x <- udpipe_annotate(ud_model, x = aos$word)

x <- as.data.frame(x)

#only keeping verbs since focus is on negative verbs
url_textstats <- subset(x, xpos %in% "VB")

#Identifying frequency of each verb
url_textstats <- txt_freq(x = url_textstats$lemma)

## Focusing on article headlines

#Cleaning up text - getting rid of punctuation and changing to lowercase
#(Chose English web language since text is from online news articles)
aos_headlines <- aos_0 %>% 
  mutate(desc = str_remove_all(headline, "[[:punct:]]"),
         desc = tolower(desc)) %>% 
  unnest_tokens(word, desc) %>% 
  anti_join(stop_words) %>% 
  select(word)

#Downloading UD model trained on English web language and appending it to df
ud_model <- udpipe_download_model(language = "english-ewt")

ud_model <- udpipe_load_model(ud_model$file_model)

#annotating parts of speech in article headlines
x <- udpipe_annotate(ud_model, x = aos_headlines$word)

x <- as.data.frame(x)

headlines_stats <- subset(x, xpos %in% "VB")

headlines_stats <- txt_freq(x = headlines_stats$lemma)


### Creating New Dataset ###


#Joining most common verbs across article text and headlines
headlines<-headlines_stats  %>% 
  mutate(headline = freq) %>%
  select(key, headline) 
  
url_text<-url_textstats  %>% 
  mutate(url_text = freq) %>%
  select(key, url_text) 

#Combining data sets to see frequency of each verb in both headlines and article text in one place
verbs <- full_join(headlines, url_text, by = "key")

write.csv(verbs, file = "verbs_in_aos.csv")