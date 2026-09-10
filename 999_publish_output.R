# 999_publish_output.R -- publish the weekly workbook to Azure Blob for Power BI.
# UCS IT, ticket #2312. VM-only: NOT in the AOS GitHub repo.
#
# Why this exists: 19 writes the workbook into C:/AOS_db/data. The backup script
# copies the WHOLE data folder to the aos-backup container (380+ blobs) - that is a
# backup, not somewhere to point a report. Power BI needs ONE file at ONE unchanging
# path, which is why the blob name never carries a date. Dated copies go to archive/.
#
# Freshness guard: a failed mid-chain step does not stop the steps after it, so a
# stale workbook can look current (this is exactly what happened through August when
# 16 was failing). This script REFUSES to publish a workbook older than the limit and
# exits 1, so the problem is loud here instead of silent on the dashboard.
# Override for a deliberate re-publish: set AOS_PUBLISH_MAX_AGE_H.

if (!require("pacman")) { install.packages("pacman"); library(pacman) }
p_load(AzureStor, AzureAuth)

SRC       <- "../data/19_data_for_wireframe.xlsx"
ACCOUNT   <- "https://ucsaosdata.blob.core.windows.net"
CONTAINER <- "aos-output"
BLOB      <- "19_data_for_wireframe.xlsx"
MAX_AGE_H <- as.numeric(Sys.getenv("AOS_PUBLISH_MAX_AGE_H", "26"))

if (!file.exists(SRC)) {
  message("PUBLISH ABORTED - workbook not found: ", SRC)
  quit(status = 1)
}

info  <- file.info(SRC)
age_h <- as.numeric(difftime(Sys.time(), info$mtime, units = "hours"))
message(sprintf("workbook: %s bytes, written %s (%.1f h ago), limit %.0f h",
                info$size, format(info$mtime), age_h, MAX_AGE_H))

if (age_h > MAX_AGE_H) {
  message(sprintf("PUBLISH ABORTED - workbook is %.1f h old. 19 likely did not run; refusing to publish stale data.", age_h))
  quit(status = 1)
}

tok  <- get_managed_token("https://storage.azure.com/")
ep   <- storage_endpoint(ACCOUNT, token = tok)
cont <- storage_container(ep, CONTAINER)

storage_upload(cont, SRC, BLOB)
dated <- paste0("archive/19_data_for_wireframe_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
storage_upload(cont, SRC, dated)

message("PUBLISHED ", BLOB, " and ", dated)
