library(tidyverse)
library(openxlsx)

all_aos_data <- read_csv("C:/AOS_db/data/17_all_the_aos_data.csv")

read_me_workbook <- read.xlsx("C:/AOS_db/info_tables/DRAFT_data for wireframing.xlsx", sheet = "Read.Me", check.names = TRUE)

# Create a new workbook and add a sheet
wb <- createWorkbook()
addWorksheet(wb, "dataframe")
addWorksheet(wb, "Read.Me")

bold_text <- createStyle(textDecoration = "bold")

# Write data to the sheet
writeData(wb, "dataframe", all_aos_data)
writeData(wb, "Read.Me", read_me_workbook, headerStyle = bold_text)

saveWorkbook(wb, "C:/AOS_db/data/18_data_for_wireframe.xlsx", overwrite = TRUE)
