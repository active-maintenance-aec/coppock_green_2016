# coppock_green_2016/ground_truth/extract_archive_values.R
# Output: ground_truth/archive_values.csv
# Depends on: the console logs and working directory that ground_truth/run_archive.R
#   leaves in ARCHIVE_RUN_DIR.
# Description: Recover every number the deposit's own scripts produce, so the archive
#   column of the ground truth is read off the archive rather than transcribed. Six of
#   the ten scripts print a LaTeX table to the console and one writes four .tex files
#   into its working directory; both are parsed here. Nothing is re-implemented: where
#   a script builds a table and never prints it, run_archive.R prints the object the
#   script itself constructed.
#
#   Two scripts yield nothing. The Figure A2 script indexes one past the end of its own
#   loop and stops before it reaches the frame it plots, so neither the figure nor the
#   60-day primary analysis beneath it is ever reached. Tables A4, A5 and A8 have no
#   deposited code at all.
#
#   Usage: ARCHIVE_RUN_DIR=/path/to/scratch Rscript ground_truth/extract_archive_values.R
#   after running ground_truth/run_archive.R against the same directory.

library(here)
library(tidyverse)

here::i_am("ground_truth/extract_archive_values.R")

run_dir <- Sys.getenv("ARCHIVE_RUN_DIR", unset = file.path(tempdir(), "cg2016_archive_run"))
stopifnot(dir.exists(file.path(run_dir, "logs")))

log_lines <- function(script) {
  read_lines(file.path(run_dir, "logs", paste0(script, ".log")))
}

# xtable prints one row per line, cells separated by & and the line ended by \\ ----
xtable_cells <- function(line) {
  line |>
    str_remove("\\\\\\\\\\s*$") |>
    str_split_1("&") |>
    str_trim() |>
    str_remove_all("[()\\\\]")
}

# A row of an xtable body, found by its own label and returned as its cells ----
# xtable glues a \cmidrule rule onto the front of the row that follows it, which hides
# the meta-analysis row from an anchored pattern.
xtable_row <- function(lines, label) {
  lines <- str_remove(lines, "^\\s*\\\\cmidrule\\(r\\)\\{[0-9-]+\\}")
  hit <- str_subset(lines, paste0("^\\s*", label, " *&"))
  stopifnot(length(hit) == 1)
  xtable_cells(hit)[-1]
}

as_value <- function(x) if_else(x %in% c("", "NA"), NA_character_, x)

# A table whose estimates and standard errors sit on alternating lines ------------------
stacked_table <- function(lines, float, panel, rows, columns) {
  imap(rows, function(spec, i) {
    cells <- as_value(xtable_row(lines, spec$pattern))
    stopifnot(length(cells) == length(columns))
    tibble(float = float, panel = panel, row_label = spec$label,
           column_label = columns, stat = spec$stat, value_script = cells)
  }) |>
    list_rbind() |>
    filter(!is.na(value_script))
}

# A table whose estimates and standard errors alternate across the row ------------------
interleaved_table <- function(lines, float, panel, row_labels, columns, published_labels) {
  imap(row_labels, function(label, i) {
    cells <- as_value(xtable_row(lines, str_replace_all(label, "'", "'")))
    stopifnot(length(cells) == 2 * length(columns))
    tibble(float = float, panel = panel, row_label = published_labels[i],
           column_label = rep(columns, each = 2),
           stat = rep(c("est", "se"), length(columns)),
           value_script = cells)
  }) |>
    list_rbind() |>
    filter(!is.na(value_script))
}

state_rows <- c("Arkansas", "Colorado", "Connecticut", "Iowa", "Illinois", "Florida",
                "Florida '05", "Kentucky", "Michigan", "Missouri", "Missouri '05",
                "Montana", "New Jersey", "Nevada", "New York", "Oklahoma", "Oregon",
                "Pennsylvania", "Rhode Island", "Meta Analysis")
state_published <- str_replace_all(state_rows, c("'05" = "2005", "Meta Analysis" = "Meta-Analysis"))

# Table 1 ---------------------------------------------------------------------------------
t1_cols <- c("Aug. 2006", "Nov. 2006", "Jan. 2008", "Aug. 2008", "Nov. 2008",
             "Aug. 2010", "Nov. 2010", "Feb. 2012", "Aug. 2012", "Nov. 2012")
t1_log <- log_lines("CG_Habit_DE_Table_1")

table_1 <- stacked_table(
  t1_log, "table_1", "main",
  list(list(pattern = "Civic Duty", label = "Civic Duty", stat = "est"),
       list(pattern = "SE1", label = "Civic Duty", stat = "se"),
       list(pattern = "Hawthorne", label = "Hawthorne", stat = "est"),
       list(pattern = "SE2", label = "Hawthorne", stat = "se"),
       list(pattern = "Self", label = "Self", stat = "est"),
       list(pattern = "SE3", label = "Self", stat = "se"),
       list(pattern = "Neighbors", label = "Neighbors", stat = "est"),
       list(pattern = "SE4", label = "Neighbors", stat = "se"),
       list(pattern = "All Instruments", label = "All Instruments", stat = "est"),
       list(pattern = "SE5", label = "All Instruments", stat = "se")),
  t1_cols
)

# The compliers table is a single unlabelled row of bracketed ranges.
complier_ranges <- function(lines, float, columns) {
  hit <- str_subset(lines, "^1 & \\[")
  stopifnot(length(hit) == 1)
  cells <- xtable_cells(hit)[-1]
  parts <- str_match(cells, "^\\[(-?[0-9.]+),(-?[0-9.]+)\\]$")
  stopifnot(!any(is.na(parts[, 1])), nrow(parts) == length(columns))
  tibble(float = float, panel = "main", row_label = "Untreated Compliers",
         column_label = rep(columns, each = 2),
         stat = rep(c("lo", "hi"), length(columns)),
         value_script = as.vector(t(parts[, 2:3])))
}

table_1 <- bind_rows(table_1, complier_ranges(t1_log, "table_1", t1_cols[2:10]))

# Table 2 ---------------------------------------------------------------------------------
t2_cols <- c("Nov. 2007", "Jan. 2008", "Aug. 2008", "Nov. 2008", "Aug. 2010",
             "Nov. 2010", "Feb. 2012", "Aug. 2012", "Nov. 2012")
t2_log <- log_lines("CG_Habit_DE_Table_2")

table_2 <- bind_rows(
  stacked_table(
    t2_log, "table_2", "main",
    list(list(pattern = "Civic Duty", label = "Civic Duty", stat = "est"),
         list(pattern = "SE1", label = "Civic Duty", stat = "se"),
         list(pattern = "Shown 05 Vote", label = "Shown 2005 Vote", stat = "est"),
         list(pattern = "SE2", label = "Shown 2005 Vote", stat = "se"),
         list(pattern = "Shown 06 Vote", label = "Shown 2006 Vote", stat = "est"),
         list(pattern = "SE3", label = "Shown 2006 Vote", stat = "se"),
         list(pattern = "All Together", label = "All Instruments", stat = "est"),
         list(pattern = "SE4", label = "All Instruments", stat = "se")),
    t2_cols
  ),
  complier_ranges(t2_log, "table_2", t2_cols[2:9])
)

# Table 3 ---------------------------------------------------------------------------------
t3_cols <- c("Apr. 2009", "Feb. 2010", "Nov. 2010", "Feb. 2011", "Apr. 2011",
             "Mar. 2012", "Nov. 2012")
t3_log <- log_lines("CG_Habit_DE_Table_3")

table_3 <- stacked_table(
  t3_log, "table_3", "main",
  list(list(pattern = "Household Size = 1", label = "Household Size = 1", stat = "est"),
       list(pattern = "n = 16638", label = "Household Size = 1", stat = "se"),
       list(pattern = "Untreated Compliers HH1", label = "Untreated Compliers HH1", stat = "est"),
       list(pattern = "Household Size = 2", label = "Household Size = 2", stat = "est"),
       list(pattern = "n = 16915", label = "Household Size = 2", stat = "se"),
       list(pattern = "Untreated Compliers HH2", label = "Untreated Compliers HH2", stat = "est"),
       list(pattern = "Household Size = 3", label = "Household Size = 3", stat = "est"),
       list(pattern = "n = 5086", label = "Household Size = 3", stat = "se"),
       list(pattern = "Untreated Compliers HH3", label = "Untreated Compliers HH3", stat = "est"),
       list(pattern = "Meta-analytic Estimate", label = "Pooled Estimate", stat = "est"),
       list(pattern = "n = 38639", label = "Pooled Estimate", stat = "se")),
  t3_cols
)

# The deposit prints each stratum's count in its own row label rather than as a cell.
table_3 <- bind_rows(
  table_3,
  tibble(float = "table_3", panel = "main",
         row_label = c("Household Size = 1", "Household Size = 2", "Household Size = 3",
                       "Pooled Estimate"),
         column_label = "n", stat = "n",
         value_script = str_match(
           map_chr(c("n = 16638", "n = 16915", "n = 5086", "n = 38639"),
                   \(p) str_subset(t3_log, paste0("^\\s*", p, " *&"))),
           "n = ([0-9]+)")[, 2])
)

# Tables 4 and 5 ----------------------------------------------------------------------------
# Four .tex files, written into the working directory rather than printed.
rd_tex <- function(file) read_lines(file.path(run_dir, "wd", file))

table_4 <- bind_rows(
  interleaved_table(rd_tex("rdtable2.tex"), "table_4", "Presidential on Midterm",
                    state_rows, c("1992-94", "1996-98", "2000-02", "2004-06", "2008-10"),
                    state_published),
  interleaved_table(rd_tex("rdtable3.tex"), "table_4", "Midterm on Presidential",
                    state_rows, c("1994-96", "1998-2000", "2002-04", "2006-08", "2010-12"),
                    state_published)
)

table_5 <- bind_rows(
  interleaved_table(rd_tex("rdtable1.tex"), "table_5", "Presidential on Presidential",
                    state_rows, c("1992-96", "1996-2000", "2000-04", "2004-08", "2008-12"),
                    state_published),
  interleaved_table(rd_tex("rdtable4.tex"), "table_5", "Midterm on Midterm",
                    state_rows, c("1994-98", "1998-2002", "2002-06", "2006-10"),
                    state_published)
)

# Table 6 -------------------------------------------------------------------------------------
# Built as two xtable objects whose print calls are commented out in the deposit.
t6_log <- log_lines("CG_Habit_RD_Table_6")
t6_rows <- state_rows[!state_rows %in% c("Colorado", "Florida '05", "Michigan", "Missouri '05")]
t6_published <- str_replace_all(t6_rows, c("Meta Analysis" = "Meta-Analysis"))

object_block <- function(lines, object) {
  start <- which(str_detect(lines, paste0("^#### OBJECT: ", str_replace_all(object, "\\.", "\\\\."))))
  stopifnot(length(start) == 1)
  ends <- which(str_detect(lines, "^#### OBJECT: |^#### STATUS: "))
  finish <- min(ends[ends > start])
  lines[start:(finish - 1)]
}

table_6 <- bind_rows(
  interleaved_table(object_block(t6_log, "RD_xtable_1"), "table_6", "upper", t6_rows,
                    c("1992-2012", "1994-2012", "1996-2012", "1998-2012", "2000-12"),
                    t6_published),
  interleaved_table(object_block(t6_log, "RD_xtable_2"), "table_6", "lower", t6_rows,
                    c("2002-12", "2004-12", "2006-12", "2008-12", "2010-12"),
                    t6_published)
)

# Table 7 ----------------------------------------------------------------------------------------
# stargazer prints the estimate row and the standard error row separately, the second
# unlabelled, and it prints the state fixed-effects indicator, N and R-squared as text rows.
t7_log <- log_lines("CG_Habit_RD_Table_7")
t7_cols <- paste0("(", 1:5, ")")

t7_terms <- c("Years between upstream and downstream", "Youth turnout in upstream election",
              "Presidential battleground", "Presidential or midterm battleground",
              "Presidential upstream", "Presidential downstream", "Constant")

stargazer_row <- function(label) {
  hit <- which(str_detect(t7_log, paste0("^\\s*", str_escape(label), " *&")))
  stopifnot(length(hit) == 1)
  list(est = as_value(xtable_cells(t7_log[hit])[-1]),
       se  = as_value(xtable_cells(t7_log[hit + 1])[-1]))
}

table_7 <- imap(t7_terms, function(label, i) {
  cells <- stargazer_row(label)
  stopifnot(length(cells$est) == 5, length(cells$se) == 5)
  tibble(float = "table_7", panel = "main", row_label = label,
         column_label = rep(t7_cols, 2), stat = rep(c("est", "se"), each = 5),
         value_script = c(cells$est, cells$se))
}) |>
  list_rbind() |>
  filter(!is.na(value_script))

gof_row <- function(pattern, label) {
  hit <- str_subset(t7_log, pattern)
  stopifnot(length(hit) == 1)
  cells <- xtable_cells(hit)[-1] |> str_remove_all("multicolumn\\{1\\}\\{c\\}|[{}$^]|R\\{2\\}")
  cells <- str_trim(cells)
  stopifnot(length(cells) == 5)
  tibble(float = "table_7", panel = "main", row_label = label, column_label = t7_cols,
         stat = "est", value_script = cells)
}

table_7 <- bind_rows(
  table_7,
  gof_row("^\\s*State F\\.E\\. &", "State fixed effects"),
  gof_row("^N &", "N"),
  gof_row("^R\\$", "R2")
)

# Table A2 -------------------------------------------------------------------------------------
a2_log <- log_lines("CG_Habit_ME_Table_A2_and_Figure_A1")
a2_cols <- c("Net Migrants", "Nc 2008", "CACE 08-12", "Bias Estimate", "Corrected CACE")

table_a2 <- str_subset(a2_log, "^\\s*[A-Z]{2} & -?[0-9]") |>
  map(function(line) {
    cells <- xtable_cells(line)
    stopifnot(length(cells) == 6)
    tibble(float = "table_a2", panel = "main", row_label = cells[1],
           column_label = a2_cols, stat = "est", value_script = cells[-1])
  }) |>
  list_rbind()

# Tables A6 and A7 ---------------------------------------------------------------------------------
# Four xtable objects the deposit builds and never prints.
a67_log <- log_lines("CG_Habit_RD_Tables_A6_and_A7")
a67_cols <- paste(c(90, 180, 270, 365, 455, 545, 635, 730), "Days")
a67_rows <- c("Difference-in-Means", "First-order Polynomial",
              "Second-order Polynomial", "Third-order Polynomial")

robustness_object <- function(object, float, panel) {
  block <- object_block(a67_log, object)
  body <- str_subset(block, "&.*\\\\\\\\")
  body <- body[!str_detect(body, "^\\s*&\\s*X90")]
  imap(a67_rows, function(label, i) {
    hit <- which(str_detect(body, paste0("^\\s*[0-9]* *& *", label, " *&")))
    stopifnot(length(hit) == 1)
    est <- as_value(xtable_cells(body[hit])[-(1:2)])
    se  <- as_value(xtable_cells(body[hit + 1])[-(1:2)])
    stopifnot(length(est) == 8, length(se) == 8)
    tibble(float = float, panel = panel, row_label = label,
           column_label = rep(a67_cols, 2), stat = rep(c("est", "se"), each = 8),
           value_script = c(est, se))
  }) |>
    list_rbind()
}

table_a6 <- bind_rows(
  robustness_object("no.xtable.0812", "table_a6", "No additional controls"),
  robustness_object("yes.xtable.0812", "table_a6", "Controls for lagged vote totals")
)
table_a7 <- bind_rows(
  robustness_object("no.xtable.0610", "table_a7", "No additional controls"),
  robustness_object("yes.xtable.0610", "table_a7", "Controls for lagged vote totals")
)

# Table A9 -----------------------------------------------------------------------------------------
a9_log <- log_lines("CG_Habit_DE_Table_A9")
a9_cols <- c("Nov 2008", "Aug 2010", "Nov 2010", "Feb 2012", "Aug 2012", "Nov 2012")

table_a9 <- stacked_table(
  a9_log, "table_a9", "main",
  list(list(pattern = "ests\\\\_recontact", label = "Shown Vote + Recontact", stat = "est"),
       list(pattern = "ses\\\\_recontact", label = "Shown Vote + Recontact", stat = "se"),
       list(pattern = "ests\\\\_norecontact", label = "Shown Vote", stat = "est"),
       list(pattern = "ses\\\\_norecontact", label = "Shown Vote", stat = "se")),
  a9_cols
)

archive_values <- bind_rows(table_1, table_2, table_3, table_4, table_5, table_6,
                            table_7, table_a2, table_a6, table_a7, table_a9) |>
  mutate(value_script = str_replace(str_trim(value_script), "^(-?)\\.", "\\10"))

stopifnot(
  !any(duplicated(archive_values[c("float", "panel", "row_label", "column_label", "stat")])),
  !any(is.na(archive_values$value_script))
)

write_csv(archive_values, here::here("ground_truth", "archive_values.csv"))

print(archive_values |> count(float, panel), n = Inf)
