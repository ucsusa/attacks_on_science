#### R SCRIPT PURPOSE: 
#### Creates a workbook with a readme from the combined data from human-driven and automated data collection for Attacks on Science Tracker.
#### Runs 1x/week

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse, 
       scales,
       openxlsx) 

all_aos_data <- read_csv("../data/18_all_the_aos_data.csv")

read_me_workbook <- read.xlsx("../info_tables/DRAFT_data for wireframing.xlsx", sheet = "Read.Me", check.names = TRUE)

wb <- createWorkbook()
addWorksheet(wb, "dataframe")
addWorksheet(wb, "Read.Me")

bold_text <- createStyle(textDecoration = "bold")

writeData(wb, "dataframe", all_aos_data)
writeData(wb, "Read.Me", read_me_workbook, headerStyle = bold_text)

saveWorkbook(wb, "../data/19_data_for_wireframe.xlsx", overwrite = TRUE)
