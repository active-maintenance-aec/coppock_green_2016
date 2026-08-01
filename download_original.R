# coppock_green_2016/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse and verify every
#   file. Run this once before running anything in maintained/. Re-running is free:
#   files already present with the right checksum are not downloaded again.
#
#   The manifest carries two checksums per file because they do not always agree.
#   md5_served is the MD5 of the bytes Dataverse returns for `?format=original`,
#   which is what this code was written against. md5_published is the checksum
#   Dataverse displays for the file. For three of the 24 files here (SMG2009.RData,
#   turnoutrates.RData, votemargins.RData, all of them ingested as tabular data)
#   the published checksum matches neither the original Dataverse serves nor the
#   .tab it derives. Verification therefore runs against md5_served, and the
#   disagreement is reported rather than hidden.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/ALZVAW"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

# Manifest ----
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

dir.create(here::here("original"), showWarnings = FALSE)

# Download what is missing or wrong ----
# format=original asks for the deposited bytes rather than the tabular
# representation Dataverse derives for ingested files.
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify ----
verified <- planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    match = md5_downloaded == md5_served,
    published_agrees = md5_served == md5_published
  ) |>
  select(file, bytes, md5_served, md5_downloaded, match, published_agrees)

print(verified, n = nrow(verified))

if (!all(verified$match)) {
  stop("Checksum mismatch: the downloaded archive does not match what Dataverse served when this code was written.")
}

print(str_glue("All {nrow(verified)} files match. ",
               "{sum(!verified$published_agrees)} of them carry a published checksum ",
               "that verifies neither the original nor the derived tabular file."))
print(str_glue("Archive: {dataset_doi}"))
