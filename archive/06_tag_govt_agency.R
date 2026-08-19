#### R SCRIPT PURPOSE: 
#### Tags government agencies mentioned in article text
#### Runs (?) How often will this script run?

##Filtering needs
#1. Filter out articles that were screened out by search terms or human coding.
#2. Filter out articles before December 16, 2026
#3. Filter out duplicates using the agreed upon source prioritization, while maintaining the unique ID.

### Logistics for R Script ###


#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse, #to wrangle and organize noisy data
       rebus,
       readxl) #to more easily work with regex expressions

#read in data
df0 <- read_csv("C:/AOS_db/data/06_aos_clean.csv")

#read in spreadsheet with government agencies
gov_agencies <- read_excel("C:/AOS_db/info_tables/Search Terms AOS.xlsx") %>%
  filter(category == "government") %>%
  mutate(search_term_lower = tolower(search_term))

### Tagging Government Agencies in First 1/3 of Article Text ###

#cut URL text down by 1/3
df1  <-  df0 %>%
  mutate(char_length = str_length(url_text), #counting full length of each article by character
         third_length = ceiling(char_length/3) + 1, #create cut off of one third
         use_text = str_sub(url_text, 1, third_length),
         use_text = tolower(use_text)) #cut each article into first third

##Search the truncated url_text for each of the government agencies and return the government agencies as a comma delimited list.
df2 <- data.frame()


for(i in 1:nrow(df1)){
  df1_split <- df1 %>%
    slice(i)
  all_words <- c()
  
  for(j in 1:nrow(gov_agencies)){
    words_to_search <- gov_agencies %>%
      slice(j)
    single_word <- str_extract(df1_split$use_text, words_to_search$search_term_lower)
    single_word_acr <- words_to_search$label[words_to_search$search_term_lower ==single_word]
    if(!is.na(single_word_acr)){
      all_words <- paste(single_word_acr, all_words, sep = ",")
    }
  }
  df1_split <- mutate(df1_split, gov_agency = all_words)
  df2 <- bind_rows(df1_split, df2)
}


remove_duplicates_in_cell <- function(cell_string) {
  items <- unlist(strsplit(cell_string, ","))
  unique_items <- unique(items)
  cleaned_string <- paste(unique_items, collapse = ", ")
  return(cleaned_string)
}

# Apply the function to the 'values' column
df2$gov_agency <- sapply(df2$gov_agency, remove_duplicates_in_cell)

write_csv(df2, "C:/AOS_db/data/06_aos_clean_gov.csv")


#create variables identifying mentions of federal agencies + subelements in first third of article text

##If we want to use this methods, let's make a csv of the government agency names and the important acronyms and what we want to reflect that in the data (generally the acronyms)

#df2  <-  df1 %>%  
#  mutate(White_House = str_detect(use_text, "\\bwhite house\\b|\\bpresident\\b"),
#         DOGE = str_detect(use_text, "\\bdepartment of government efficiency\\b|\\bdoge\\b"),
#         Senate = str_detect(use_text, "\\bsenate\\b|\\bus senate\\b|\\bu.s. senate\\b"),
#         House_of_Representatives = str_detect(use_text, "\\bhouse of representatives\\b|\\bus house of representatives\\b|\\bu.s. house of representatives\\b"),
#         Congress = str_detect(use_text, "\\bcongress\\b"),
#         AirForce = str_detect(use_text, "\\bair force\\b|\\bus air force\\b"), 
#         ACOE = str_detect(use_text, "\\barmy corps of engineers\\b|\\barce\\b|\\bacoe\\b"),
#         Army = str_detect(use_text, "\\bus army\\b|\\bu.s. army\\b|\\barmy\\b"),
#         StratComm = str_detect(use_text, "\\barmy forces strategic command\\b|\\bstratcomm\\b"),
#         DOD = str_detect(use_text, "\\bdepartment of defense\\b|\\bdod\\b|\\bdefense department\\b"),
#         Navy = str_detect(use_text, "\\bus navy\\b|\\bu.s. navy\\b"),
#        USDA = str_detect(use_text, "\\bus department of agriculture\\b|\\bu.s. department of agriculture\\b|\\bdepartment of agriculture\\b|\\bagriculture department\\b|\\busda\\b"),
#        USFS = str_detect(use_text, "\\bus forest service\\b|\\bu.s. forest service\\b|\\bforest service\\b|\\busfs\\b"),
#        NRCS = str_detect(use_text, "\\bnatural resources conservation service\\b|\\bnrcs\\b"),
#        NASS = str_detect(use_text, "\\bnational agricultural statistics service\\b"),
#        DOC = str_detect(use_text,"\\bdepartment of commerce\\b"),
#        NOAA = str_detect(use_text, "\\bnational oceanic and atmospheric administration\\b|\\bnoaa\\b"),
#        NWS = str_detect(use_text, "\\bnational weather service\\b|\\bnws\\b"),
#        NIST = str_detect(use_text, "\\bnational institute of standards and technology\\b|\\bnist\\b"),
#        USCB = str_detect(use_text, "\\bbureau of the census\\b|\\bcensus bureau\\b|\\buscb\\b"),
#        DOJ = str_detect(use_text, "\\bdepartment of justice\\b|\\bjustice department\\b|\\bdoj\\b"),
#        BLS = str_detect(use_text, "\\bbureau of labor statistics\\b"),
#        EBSA = str_detect(use_text, "\\bemployee benefits security administration\\b"),
#        OSHA = str_detect(use_text, "\\boccupational safety and health administration\\b|\\bosha\\b"),
#        DOE = str_detect(use_text, "\\bdepartment of energy\\b|\\benergy department\\b"),
#        NNSA = str_detect(use_text, "\\bnational nuclear security administration\\b|\\bnnsa\\b"),
#        ED = str_detect(use_text, "\\bdepartment of education\\b|\\beducation department\\b"),
#        HHS = str_detect(use_text, "\\bdepartment of health and human services\\b|\\bhhs\\b"),
#        FDA = str_detect(use_text, "\\bfood and drug administration\\b|\\bfda\\b"),
#        NIH = str_detect(use_text, "\\bnational institutes of health\\b|\\bnih\\b"),
#        CDC = str_detect(use_text, "\\bcenters for disease control and prevention\\b|\\bcdc\\b"),
#        DHS = str_detect(use_text, "\\bdepartment of homeland security\\b|\\bdhs\\b"),
#        FEMA = str_detect(use_text, "\\bfederal emergency management agency\\b|\\bfema\\b"),
#        CISA = str_detect(use_text, "\\bcybersecurity and infrastructure security agency\\b|\\bcisa\\b"),
#        HUD = str_detect(use_text, "\\bdepartment of housing and urban development\\b|\\bhud\\b"),
#        DOI = str_detect(use_text, "\\bdepartment of the interior\\b|\\bdepartment of interior\\b|\\binterior department\\b|\\bdoi\\b"),
#        BLM = str_detect(use_text, "\\bbureau of land management\\b|\\bblm\\b"),
#        USBR = str_detect(use_text, "\\bbureau of reclamation\\b|\\busbr\\b"),
#        USGS = str_detect(use_text, "\\bgeological survey\\b|\\busgs\\b"#),
#         NPS = str_detect(use_text, "\\bnational park service\\b|\\bnps\\b"),
#        FWS = str_detect(use_text, "\\bfish and wildlife services\\b|\\bfws\\b"),
#        DOS = str_detect(use_text, "\\bdepartment of state\\b|\\bstate department\\b"),
#        OSTC = str_detect(use_text, "\\boffice of science and technology cooperation\\b|\\bstc\\b"),
#        DOT = str_detect(use_text, "\\bdepartment of transportation\\b|\\bdot\\b"),
#        NHTSA = str_detect(use_text, "\\bnational highway traffic safety administration\\b|\\bnhtsa\\b"),
#        PHMSA = str_detect(use_text, "\\bpipeline and hazardous materials safety administration\\b|\\bphmsa\\b"),
#        TREAS = str_detect(use_text, "\\bdepartment of the treasury\\b|\\btreasury\\b"),
#        IRS = str_detect(use_text, "\\binternal revenue service\\b|\\birs\\b"),
#        VA = str_detect(use_text, "\\bdepartment of veteran affairs\\b|\\bva\\b"),
#        EPA = str_detect(use_text, "\\benvironmental protection agency\\b|\\bepa\\b"),
#        FTC = str_detect(use_text, "\\bfederal trade commission\\b|\\bftc\\b"),
#        Global_Media = str_detect(use_text, "\\bagency for global media\\b"),
#        NSF = str_detect(use_text, "\\bnational science foundation\\b|\\bnsf\\b"),
#        NASA = str_detect(use_text, "\\bnational aeronautics and space administration\\b|\\bnasa\\b"),
#        NRC = str_detect(use_text, "\\bnuclear regulatory commission\\b|\\bnrc\\b"),
#        Wilson_Center = str_detect(use_text, "\\bwoodrow wilson international center for scholars\\b|\\bwoodrow wilson center\\b|\\bwilson center\\b"),
#        SSA = str_detect(use_text, "\\bsocial security administration\\b|\\bssa\\b"),
#        FEC = str_detect(use_text, "\\bfederal election commission\\b|\\bfec\\b"),
#        CPSC = str_detect(use_text, "\\bconsumer product safety commission\\b|\\bcpsc\\b"),
#        Appalachian_Regional_Commission = str_detect(use_text, "\\bappalachian regional commission\\b"),
#        Arctic_Research_Commission = str_detect(use_text, "\\barctic research commission\\b"),
#        CEQ = str_detect(use_text, "\\bcouncil on environmental quality\\b|\\bceq\\b"),
#        EAC = str_detect(use_text, "\\belection assistance commission\\b|\\beac\\b"),
#        OSTP = str_detect(use_text, "\\boffice of science and technology policy\\b|\\bostp\\b"),
#        Tennessee_Valley_Authority = str_detect(use_text, "\\btennessee valley authority\\b"),
#        USAID = str_detect(use_text, "\\bus agency for international aid\\b|\\busaid\\b"),
#        FERC = str_detect(use_text, "\\bfederal energy regulatory commission\\b|\\bferc\\b"),
#        OPM = str_detect(use_text, "\\boffice of personnel management\\b|\\bopm\\b"),
#        OMB = str_detect(use_text, "\\boffice of management and budget\\b|\\bomb\\b"),
#        DOL = str_detect(use_text, "\\bdepartment of labor\\b|\\bdol\\b"#))
         
#validate script and comparison between written agencies and R identified agencies
#df3 <- df2 %>% 
# select(`AGENCIES INVOLVED`, use_text, `LINK`, White_House:DOL)

#write.csv(df2, "ids.csv")