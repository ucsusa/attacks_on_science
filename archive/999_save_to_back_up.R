#### R SCRIPT PURPOSE: 
#### Back up all saved AOS data
#### Runs (?) How often will this script run?


### Logistics for R Script ###


#Checking if pacman is installed, installing if missing, and loading it
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} #to install, load, and update multiple packages at one once

#Installing (if not already installed) and loading necessary R packages
p_load(tidyverse) #to wrangle and organize noisy data


### Back Up All AOS Data, Scripts, Processes ###


##Copy the data
source_folder <- "C:/AOS_db/data"

#create a destination to save backup data
destination_folder <- "C:/Users/KristieEllickson/OneDrive - Union of Concerned Scientists/Jules Barbati-Dajches's files - Backup_from_server"

#new loop: create a new directory in the destination
if (!dir.exists(destination_folder)) {
  dir.create(destination_folder, recursive = TRUE)
}

#copy all data files to destination
file.copy(from = source_folder, to = destination_folder, recursive = TRUE)

##Copy the r scripts
source_folder <- "C:/AOS_db/r_scripts"

destination_folder <- "C:/Users/KristieEllickson/OneDrive - Union of Concerned Scientists/Jules Barbati-Dajches's files - Backup_from_server/r_scripts"

#new loop: create a new directory in the destination
if (!dir.exists(destination_folder)) {
  dir.create(destination_folder, recursive = TRUE)
}

#copy all r scripts to destination
file.copy(from = source_folder, to = destination_folder, recursive = TRUE)

##Copy the process flow diagram
source_folder <- "C:/AOS_db/newssources_to_attacksonscience.pptx"

destination_folder <- "C:/Users/KristieEllickson/OneDrive - Union of Concerned Scientists/Jules Barbati-Dajches's files - Backup_from_server"

file.copy(from = source_folder, to = destination_folder, recursive = TRUE)
