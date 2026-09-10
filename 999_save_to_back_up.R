##This script saves results and scripts to a backup server on Azure

if (!require("pacman")) { install.packages("pacman"); library(pacman) }
p_load(tidyverse, AzureStor, AzureAuth)

tok  <- get_managed_token("https://storage.azure.com/")
ep   <- storage_endpoint("https://ucsaosdata.blob.core.windows.net", token = tok)

cont <- storage_container(ep, "aos-backup")
backup_dir <- function(cont, local, prefix) {
  if (!dir.exists(local)) { message("skip (missing): ", local); return(invisible()) }
  files <- list.files(local, recursive = TRUE, full.names = FALSE)
  if (!length(files)) { message("skip (empty): ", local); return(invisible()) }
  storage_multiupload(cont, src = file.path(local, files), dest = file.path(prefix, files))
  message("backed up ", length(files), " files from ", local)
}
backup_dir(cont, "../data",      "data")
backup_dir(cont, "../r_scripts", "r_scripts")
pptx <- "../newssources_to_attacksonscience.pptx"
if (file.exists(pptx)) { storage_upload(cont, pptx, basename(pptx)); message("backed up ", basename(pptx)) } else { message("skip (missing): ", pptx) }
