#### R SCRIPT PURPOSE: 
#### Back up all saved AOS data
#### Runs weekly


### Logistics for R Script ###


if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 

p_load(tidyverse) 


### Back Up All AOS Data, Scripts, Processes ###

source_folder <- "C:/AOS_db/data"

destination_folder <- "C:/Users/KristieEllickson/OneDrive - Union of Concerned Scientists/Jules Barbati-Dajches's files - Backup_from_server"

if (!dir.exists(destination_folder)) {
  dir.create(destination_folder, recursive = TRUE)
}

file.copy(from = source_folder, to = destination_folder, recursive = TRUE)

##Copy the r scripts
source_folder <- "C:/AOS_db/r_scripts"

destination_folder <- "C:/Users/KristieEllickson/OneDrive - Union of Concerned Scientists/Jules Barbati-Dajches's files - Backup_from_server/r_scripts"

if (!dir.exists(destination_folder)) {
  dir.create(destination_folder, recursive = TRUE)
}

file.copy(from = source_folder, to = destination_folder, recursive = TRUE)

source_folder <- "C:/AOS_db/newssources_to_attacksonscience.pptx"

destination_folder <- "C:/Users/KristieEllickson/OneDrive - Union of Concerned Scientists/Jules Barbati-Dajches's files - Backup_from_server"

file.copy(from = source_folder, to = destination_folder, recursive = TRUE)
