# coppock_green_2016/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive,
# then every published table and figure.
# Every script is self-contained and can also be run on its own.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Downstream experiments ----
source(here::here("maintained", "table_1_etov2006.R"))
source(here::here("maintained", "table_2_etov2007.R"))
source(here::here("maintained", "table_3_smg2009.R"))
source(here::here("maintained", "table_a9_retov2007.R"))

# Regression discontinuities ----
# The robustness grid refits every state and window combination and is the slow step.
source(here::here("maintained", "tables_4_5_rd.R"))
source(here::here("maintained", "table_6_rd_persistence.R"))
source(here::here("maintained", "table_7_modeling_caces.R"))
source(here::here("maintained", "tables_a6_a7_robustness.R"))
source(here::here("maintained", "figure_a2_sawtooth.R"))

# Interstate migration ----
source(here::here("maintained", "table_a2_figure_a1_movers.R"))

# Figure timestamps ----
# R's pdf() device stamps a wall-clock /CreationDate and /ModDate into every figure it
# writes, and those two fields are the only reason two runs of this pipeline produce
# differing files. Blanking them lets the determinism check cover every file the
# pipeline writes rather than all but the figures.
source(here::here("maintained", "helpers.R"))
walk(
  list.files(here::here("maintained", "output"), pattern = "\\.pdf$", full.names = TRUE),
  blank_pdf_timestamps
)

# Ground truth and the second instrument ----
# build_ground_truth.R assembles the comparison table and, as its last step, runs
# in_text_claims.R under capture.output as the coverage gate. It is sourced again below
# for the human-readable log, which is the only reason it runs twice.
source(here::here("ground_truth", "build_ground_truth.R"))
source(here::here("maintained", "in_text_claims.R"))

# Deposited archive, again ----
# The check at the top of this file is a precondition: it says original/ was intact
# before anything ran. Nothing above writes to original/, and this second pass is what
# demonstrates it rather than assuming it. Nothing is downloaded; the files are already
# present and are re-checked against the manifest on checksum, byte size and membership.
source(here::here("download_original.R"))
