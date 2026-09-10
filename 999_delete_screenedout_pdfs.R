DRY_RUN <- as.logical(Sys.getenv("AOS_PDFCLEAN_DRYRUN", "FALSE"))
if (!require("pacman")) { install.packages("pacman"); library(pacman) }
p_load(tidyverse, readtext, purrr, readxl)
gc()
screened_in_articles <- read_csv("../data/06_aos_raw.csv") %>%
  mutate(clean_title = tolower(title), clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title), clean_title = str_trim(clean_title))
completed_saved_articles <- read_csv("../data/06_screened_read_articles_complete.csv") %>%
  mutate(clean_title = tolower(title), clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title), clean_title = str_trim(clean_title))
screened_out_articles <- completed_saved_articles %>%
  filter(!clean_title %in% screened_in_articles$clean_title)
coded_articles <- read_excel("../data/11_coding_spreadsheet.xlsx") %>%
  filter(`AOS PRESENCE` %in% c(0, 1)) %>%
  mutate(clean_title = tolower(HEADLINE), clean_title = gsub("[[:punct:]]", "", clean_title),
         clean_title = gsub("  ", " ", clean_title), clean_title = str_trim(clean_title)) %>%
  select(HEADLINE, clean_title, `FULL DATE`, LINK, `ARTICLE SOURCE`) %>%
  rename(title = HEADLINE, pub_date = `FULL DATE`, URL = LINK, source = `ARTICLE SOURCE`)
coded_screened_out_articles <- bind_rows(screened_out_articles, coded_articles)
pdf_folder <- "../pdf_articles"
pdf_files  <- list.files(pdf_folder)
pdf_df <- data.frame(files = pdf_files) %>%
  mutate(filename = tolower(files), filename = gsub("[[:punct:]]", "", filename),
         filename = gsub("pdf|stat |ap news", "", filename),
         filename = gsub("  ", " ", filename), filename = str_trim(filename)) %>%
  filter(filename %in% coded_screened_out_articles$clean_title)
to_remove_screened <- file.path(pdf_folder, pdf_df$files)
pdf_df_c_date <- map_dfr(pdf_files, function(i) {
  info <- file.info(file.path(pdf_folder, i)); data.frame(article_names = i, creation_date = info$ctime) }) %>%
  mutate(c_date = as.Date(creation_date), date_diff_num = as.numeric(today() - c_date)) %>%
  filter(date_diff_num > 60)
to_remove_old <- file.path(pdf_folder, pdf_df_c_date$article_names)
to_remove <- unique(c(to_remove_screened, to_remove_old))
if (DRY_RUN) {
  message("DRY RUN -- would remove ", length(to_remove), " of ", length(pdf_files), " files")
  message("  screened/coded matches: ", length(to_remove_screened), " ; >60 days: ", length(to_remove_old))
} else {
  ok <- file.remove(to_remove); message("removed ", sum(ok), " of ", length(to_remove), " targeted files")
}
