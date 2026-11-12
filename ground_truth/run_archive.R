# coppock_green_2016/ground_truth/run_archive.R
# Output: one console log per deposited script, written into ARCHIVE_RUN_DIR (default: a
#   scratch directory under tempdir()), plus ground_truth/archive_run_status.csv and
#   ground_truth/archive_overwrites.csv.
# Depends on: original/ (fetched by download_original.R).
# Description: Run the deposit's own ten scripts and keep everything they print, so
#   the archive column of the ground truth is read off the archive rather than typed.
#   Not part of run_all.R: it runs someone else's code, it needs a scratch directory
#   of its own, and its result is a property of the deposit rather than of this
#   repository, so it is run by hand and its parsed output is committed.
#
#   Three things about how the deposit has to be run are not obvious.
#
#   The scripts are never run inside original/ itself. Four of them write .tex files
#   and one writes an Rplots.pdf into the working directory, so an in-place run leaves
#   original/ holding files the manifest does not list. They are run from a copy.
#
#   Each script opens with rm(list = ls()) and expects to be the whole session, so each
#   one gets its own R process rather than being sourced into a shared environment.
#
#   Four of the deposit's tables are built as xtable objects whose print calls are
#   commented out, so the script computes them and shows nothing. Those objects are
#   printed after the script returns. That is reading the archive's own result, not
#   re-implementing its formatting: nothing here rebuilds a table the deposit did not
#   already construct.
#
#   The deposit ships no intermediate objects, so there is nothing to withhold and the run
#   is a single pass, recorded as as_shipped. A stripped pass would run the same files.
#
#   Usage: ARCHIVE_RUN_DIR=/path/to/scratch Rscript ground_truth/run_archive.R

library(here)
library(tidyverse)

here::i_am("ground_truth/run_archive.R")

run_dir <- Sys.getenv("ARCHIVE_RUN_DIR", unset = file.path(tempdir(), "cg2016_archive_run"))
wd  <- file.path(run_dir, "wd")
log_dir <- file.path(run_dir, "logs")

dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
unlink(wd, recursive = TRUE)
dir.create(wd, recursive = TRUE, showWarnings = FALSE)

file.copy(list.files(here::here("original"), full.names = TRUE), wd)

# The movers script reads "Measurement Error/interstatemovers.txt"; the deposit ships
# that file at the top level and no such directory. Without this the script cannot run
# at all, so the scratch copy provides the path the code asks for. The deposit itself
# is untouched, and the report records the mismatch.
dir.create(file.path(wd, "Measurement Error"), showWarnings = FALSE)
file.copy(file.path(wd, "interstatemovers.txt"), file.path(wd, "Measurement Error"))

# Objects each script builds and never prints, printed after it returns ----
extra_prints <- list(
  "CG Habit RD Table 6.R"         = c("RD_xtable_1", "RD_xtable_2"),
  "CG Habit RD Tables A6 and A7.R" = c("no.xtable.0812", "yes.xtable.0812",
                                       "no.xtable.0610", "yes.xtable.0610")
)

scripts <- c(
  "CG Habit DE Table 1.R",
  "CG Habit DE Table 2.R",
  "CG Habit DE Table 3.R",
  "CG Habit DE Table A9.R",
  "CG Habit ME Table A2 and Figure A1.R",
  "CG Habit RD Tables 4 and 5.R",
  "CG Habit RD Table 6.R",
  "CG Habit RD Table 7.R",
  "CG Habit RD Tables A6 and A7.R",
  "CG Habit RD Figure A2.R"
)

runner <- file.path(run_dir, "runner.R")
# Everything the runner needs lives inside a function, because the deposit's first line
# is rm(list = ls()) and anything left in the global environment does not survive it.
# source() still evaluates in the global environment, which is what the scripts expect.
write_lines(c(
  '# The Figure A2 script calls system("say ..."). Shadowing system() on the search',
  '# path survives the script\'s own rm(list = ls()), which clears only the global env.',
  'attach(list(system = function(...) invisible(0L)), name = "shadow", warn.conflicts = FALSE)',
  'run_one <- function() {',
  '  a <- commandArgs(trailingOnly = TRUE)',
  '  status <- tryCatch({',
  '    source(a[1], echo = FALSE, print.eval = TRUE)',
  '    "ok"',
  '  }, error = function(e) paste("error:", conditionMessage(e)))',
  '  for (obj in a[-1]) {',
  '    cat("#### OBJECT:", obj, "\\n")',
  '    print(tryCatch(get(obj, envir = globalenv()), error = function(e) conditionMessage(e)))',
  '  }',
  '  cat("#### STATUS:", status, "\\n")',
  '  status',
  '}',
  '# A script that stops exits non-zero, so the exit status records the failure as well as',
  '# the marker does.',
  'if (run_one() != "ok") quit(status = 1)'
), runner)

log_path <- function(script) {
  file.path(log_dir, paste0(str_replace_all(str_remove(script, "\\.R$"), " ", "_"), ".log"))
}

# The deposit's scripts load their data by bare file name, so they run with the scratch
# copy as the working directory. Every path this script uses is absolute.
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(wd)

# Each deposited file's checksum and modification time before the run, so a file the run
# writes over can be told from one it leaves alone.
deposited <- list.files(here::here("original"))
before <- tibble(
  file = deposited,
  md5_before = unname(tools::md5sum(deposited)),
  mtime_before = file.mtime(deposited)
)

runs <- map(scripts, function(script) {
  args <- c(runner, script, extra_prints[[script]])
  exit_status <- system2(
    file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", shQuote(args)),
    stdout = log_path(script), stderr = log_path(script),
    env = paste0("R_LIBS=", shQuote(paste(.libPaths(), collapse = .Platform$path.sep)))
  )
  # The marker is not always at the start of a line: a deposited print call can leave
  # the cursor mid-line, so the status is matched anywhere and read off the match.
  line <- str_subset(read_lines(log_path(script)), "#### STATUS: ")
  status <- if (length(line) == 0) {
    "no status recorded"
  } else {
    str_trim(str_remove(line[1], "^.*#### STATUS: "))
  }
  tibble(script = script, exit_status = as.integer(exit_status), status = status)
}) |>
  list_rbind()

after <- tibble(
  file = deposited,
  md5_after = unname(tools::md5sum(deposited)),
  mtime_after = file.mtime(deposited)
)

setwd(old_wd)

# Files a run of the deposit leaves behind, which is the reason it never runs in place
strays <- setdiff(
  list.files(wd, recursive = TRUE, all.files = TRUE, no.. = TRUE),
  c(deposited, file.path("Measurement Error", "interstatemovers.txt"))
)

# The two archive tables, in the canonical long form ----
# Every deposited script produces published output, so each is an analysis script; the
# deposit's helper file is only ever sourced by them and has no row of its own.
run_status <- runs |>
  transmute(
    pass = "as_shipped",
    script,
    role = "analysis",
    exit_status,
    outcome = if_else(status == "ok", "clean", "error"),
    stopped_at = if_else(status == "ok", NA_character_, str_remove(status, "^error: "))
  )

overwrites <- bind_rows(
  tibble(pass = "as_shipped", file = strays, effect = "added", matches_deposit = NA),
  before |>
    inner_join(after, by = "file") |>
    filter(mtime_after != mtime_before | md5_after != md5_before) |>
    transmute(pass = "as_shipped", file, effect = "overwritten",
              matches_deposit = md5_after == md5_before)
)

stopifnot(nrow(run_status) == length(scripts),
          all(run_status$outcome == "clean" | run_status$exit_status != 0))

write_csv(run_status, here::here("ground_truth", "archive_run_status.csv"))
write_csv(overwrites, here::here("ground_truth", "archive_overwrites.csv"))

print(run_status, n = Inf, width = 200)
print(overwrites, n = Inf)
