# coppock_green_2016/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_green_2016_ground_truth.csv,
#   ground_truth/float_coverage.csv
# Depends on: maintained/output/ (run run_all.R first), maintained/in_text_claims.R,
#   ground_truth/published_claims.csv, ground_truth/published_maintext_tables.csv,
#   ground_truth/published_appendix_values.csv, ground_truth/archive_values.csv
# Description: Assemble the comparison table, then run the coverage gate over the second
#   instrument.
#
#   value_paper is the number the published page prints, carried as the string the page
#   carries. It comes only from the article and its supporting information, read off
#   those PDFs by ground_truth/extract_published_values.R for the 1,329 table cells and
#   transcribed by hand below for the prose claims.
#
#   value_script is what the deposit's own scripts print, recovered by
#   ground_truth/extract_archive_values.R. The deposit prints tables and nothing else,
#   so it is populated for float cells and missing for prose quantities; where a
#   sentence quotes a table cell, that cell's own row carries the archive comparison.
#
#   value_rewrite is read out of maintained/output/. No published number is an input to
#   any computation here or in maintained/.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

options(width = 200)

paper_id <- "coppock_green_2016"

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

# The extraction -----------------------------------------------------------------------
# published_claims.csv is the numeric-token extraction from the article and its
# supporting information. It governs coverage for both instruments and is the single
# home of the per-claim precision, so neither file can name a different one.

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)

# Rendering and comparison --------------------------------------------------------------

# "The string the page prints" means its digits, not its typography: the Unicode minus,
# thousands separators and a missing leading zero are normalised away, and the number of
# decimals, the one typographic fact the comparison needs, is preserved.
normalise_printed <- function(x) {
  x |>
    str_replace_all("−", "-") |>
    str_remove_all(",") |>
    str_replace("^(-?)\\.", "\\10")
}

# Signed zero is normalised here as well as in the claims file; whichever instrument
# normalises, both must.
render_at <- function(x, digits) {
  rendered <- sprintf(paste0("%.", digits, "f"), x)
  str_replace(rendered, "^-(0(\\.0+)?)$", "\\1")
}

printed_decimals <- function(x) {
  if_else(str_detect(x, "\\."), nchar(str_remove(x, "^.*\\.")), 0L)
}

# A value agrees when the pipeline's number, printed to the page's own precision, gives
# the same digits. The epsilon keeps a value sitting a hair from the rounding boundary
# from being rejected by floating point.
agrees <- function(value, value_paper, digits) {
  target <- suppressWarnings(as.numeric(normalise_printed(value_paper)))
  d <- if_else(is.na(digits), 0L, as.integer(digits))
  case_when(
    is.na(value) | is.na(target) | is.na(digits) ~ NA_real_,
    render_at(value, d) == normalise_printed(value_paper) ~ 1,
    abs(round(value, d) - target) < 1e-9 * pmax(1, abs(target)) ~ 1,
    .default = 0
  )
}

# The deposit prints its tables already rounded to three decimals, so the archive side
# is compared at the coarser of the two precisions rather than at the page's.
agrees_printed <- function(printed, value_paper, digits) {
  a <- suppressWarnings(as.numeric(normalise_printed(printed)))
  b <- suppressWarnings(as.numeric(normalise_printed(value_paper)))
  d <- pmin(if_else(is.na(digits), 0L, as.integer(digits)),
            printed_decimals(normalise_printed(printed)))
  case_when(
    is.na(a) | is.na(b) ~ if_else(!is.na(printed) & !is.na(value_paper) &
                                    printed == value_paper, 1, NA_real_),
    abs(round(a, d) - round(b, d)) < 1e-9 * pmax(1, abs(b)) ~ 1,
    .default = 0
  )
}

# Checks that depend only on the extraction ----------------------------------------------
# These run before anything consumes it, so a wrong precision trips its own check rather
# than the value comparison downstream.

stopifnot(
  !any(duplicated(published_claims$claim_id)),
  all(nzchar(published_claims$claim_id)),
  all(published_claims$claim_type %in%
        c("pipeline", "descriptive", "definitional", "structural", "transcribed")),
  all(published_claims$needs_block %in% c(TRUE, FALSE)),
  all(is.na(published_claims$comparison) |
        published_claims$comparison %in% c("==", "<", ">", "<=", ">=", "approx")),
  all(published_claims$needs_block[
    published_claims$claim_type %in% c("pipeline", "descriptive")])
)

# A stored value_paper that does not survive a round trip through its own recorded
# precision means digits is wrong about the precision even where it is right about the
# value, which numeric equality would pass.
round_trips <- function(value_paper, digits) {
  numeric_rows <- !is.na(value_paper) & !is.na(digits) &
    str_detect(value_paper, "^-?\\d+(\\.\\d+)?$")
  stopifnot(
    all(value_paper[numeric_rows] == normalise_printed(value_paper[numeric_rows])),
    all(render_at(as.numeric(value_paper[numeric_rows]), digits[numeric_rows]) ==
          value_paper[numeric_rows])
  )
  invisible(NULL)
}

round_trips(published_claims$value_paper, published_claims$digits)

# The precision of a prose claim comes from the extraction and from nowhere else.
prose_digits <- function(id) {
  d <- published_claims$digits[match(id, published_claims$claim_id)]
  stopifnot(!any(is.na(d)))
  d
}

prose_value <- function(id) {
  stopifnot(id %in% published_claims$claim_id)
  published_claims$value_paper[match(id, published_claims$claim_id)]
}

# The published pages, the deposit and the rewrite -----------------------------------------

published_cells <- bind_rows(
  read_csv(here::here("ground_truth", "published_maintext_tables.csv"),
           col_types = cols(value_paper = col_character(), .default = col_character())),
  read_csv(here::here("ground_truth", "published_appendix_values.csv"),
           col_types = cols(value_paper = col_character(), .default = col_character()))
)

archive_cells <- read_csv(
  here::here("ground_truth", "archive_values.csv"),
  col_types = cols(value_script = col_character(), .default = col_character())
)

table_1_rw <- out("table_1_etov2006.csv")
table_1_ctl <- out("table_1_etov2006_complier_ctl.csv")
table_2_rw <- out("table_2_etov2007.csv")
table_2_ctl <- out("table_2_etov2007_complier_ctl.csv")
table_3_rw <- out("table_3_smg2009.csv")
table_3_ctl <- out("table_3_smg2009_complier_ctl.csv")
rd_all <- out("tables_4_5_rd_all.csv")
table_6_rw <- out("table_6_rd_persistence.csv")
binomial_rw <- out("table_6_binomial_test.csv")
table_7_coef <- out("table_7_coefficients.csv")
table_7_gof <- out("table_7_gof.csv")
table_7_meta <- out("table_7_metadata.csv")
average_cace <- out("text_average_cace.csv")
a6_rw <- out("table_a6_robustness_0812.csv")
a7_rw <- out("table_a7_robustness_0610.csv")
table_a9_rw <- out("table_a9_retov2007.csv")
followup_rw <- out("text_followup_experiment.csv")
migration_rw <- out("table_a2_migration.csv")
figure_a1_rw <- out("figure_a1_bias.csv")
figure_a2_rw <- out("figure_a2_data.csv")
figure_a2_60_rw <- out("figure_a2_data_60day.csv")
like_elections_rw <- out("text_like_elections.csv")

# Labels the rewrite uses, in the words the published pages use -----------------------------

t1_cols <- c(AUG2006 = "Aug. 2006", NOV2006 = "Nov. 2006", JAN2008 = "Jan. 2008",
             AUG2008 = "Aug. 2008", NOV2008 = "Nov. 2008", AUG2010 = "Aug. 2010",
             NOV2010 = "Nov. 2010", FEB2012 = "Feb. 2012", AUG2012 = "Aug. 2012",
             NOV2012 = "Nov. 2012")
t2_cols <- c(NOV2007 = "Nov. 2007", JAN2008 = "Jan. 2008", AUG2008 = "Aug. 2008",
             NOV2008 = "Nov. 2008", AUG2010 = "Aug. 2010", NOV2010 = "Nov. 2010",
             FEB2012 = "Feb. 2012", AUG2012 = "Aug. 2012", NOV2012 = "Nov. 2012")
t3_cols <- c(APR2009 = "Apr. 2009", aec_voted_2010p = "Feb. 2010",
             aec_voted_2010g = "Nov. 2010", aec_voted_2011cp = "Feb. 2011",
             aec_voted_2011ce = "Apr. 2011", aec_voted_2012p = "Mar. 2012",
             aec_voted_2012g = "Nov. 2012")
a9_cols <- c(NOV2008 = "Nov 2008", AUG2010 = "Aug 2010", NOV2010 = "Nov 2010",
             FEB2012 = "Feb 2012", AUG2012 = "Aug 2012", NOV2012 = "Nov 2012")
state_names <- c(AR = "Arkansas", CO = "Colorado", CT = "Connecticut", IA = "Iowa",
                 IL = "Illinois", FL = "Florida", FL05 = "Florida 2005",
                 KY = "Kentucky", MI = "Michigan", MO = "Missouri",
                 MO05 = "Missouri 2005", MT = "Montana", NJ = "New Jersey",
                 NV = "Nevada", NY = "New York", OK = "Oklahoma", OR = "Oregon",
                 PA = "Pennsylvania", RI = "Rhode Island", meta = "Meta-Analysis")
a67_rows <- c("Difference-in-Means", "First-order Polynomial",
              "Second-order Polynomial", "Third-order Polynomial")

# The rewrite's own version of every published cell -------------------------------------------

long_est_se <- function(d, float, panel, row_label, column_label, est, se) {
  d |>
    transmute(float = float, panel = panel, row_label = {{ row_label }},
              column_label = {{ column_label }}, est = {{ est }}, se = {{ se }}) |>
    pivot_longer(c(est, se), names_to = "stat", values_to = "value_rewrite") |>
    filter(!is.na(value_rewrite))
}

robustness_long <- function(d, float) {
  d |>
    transmute(float = float,
              panel = if_else(lag, "Controls for lagged vote totals",
                              "No additional controls"),
              row_label = a67_rows[order + 1],
              column_label = paste(bandwidth, "Days"),
              est = fe_cace, se = fe_se) |>
    pivot_longer(c(est, se), names_to = "stat", values_to = "value_rewrite") |>
    filter(!is.na(value_rewrite))
}

rewrite_cells <- bind_rows(
  # Table 1
  long_est_se(table_1_rw, "table_1", "main",
              label, t1_cols[as.character(election)], cace, se),
  table_1_rw |> distinct(label, n) |> filter(!is.na(n)) |>
    transmute(float = "table_1", panel = "main", row_label = label,
              column_label = "n", stat = "n", value_rewrite = as.numeric(n)),
  table_1_ctl |>
    transmute(float = "table_1", panel = "main", row_label = "Untreated Compliers",
              column_label = t1_cols[election], lo = ctl_lo, hi = ctl_hi) |>
    pivot_longer(c(lo, hi), names_to = "stat", values_to = "value_rewrite"),
  # Table 2
  long_est_se(table_2_rw |> mutate(label = recode(label,
                "Shown 05 Vote" = "Shown 2005 Vote", "Shown 06 Vote" = "Shown 2006 Vote",
                "All Together" = "All Instruments")),
              "table_2", "main", label, t2_cols[as.character(election)], cace, se),
  table_2_rw |> mutate(label = recode(label,
      "Shown 05 Vote" = "Shown 2005 Vote", "Shown 06 Vote" = "Shown 2006 Vote",
      "All Together" = "All Instruments")) |>
    distinct(label, n) |> filter(!is.na(n)) |>
    transmute(float = "table_2", panel = "main", row_label = label,
              column_label = "n", stat = "n", value_rewrite = as.numeric(n)),
  table_2_ctl |>
    transmute(float = "table_2", panel = "main", row_label = "Untreated Compliers",
              column_label = t2_cols[election], lo = ctl_lo, hi = ctl_hi) |>
    pivot_longer(c(lo, hi), names_to = "stat", values_to = "value_rewrite"),
  # Table 3
  long_est_se(table_3_rw |> filter(type %in% c("first_stage", "iv")),
              "table_3", "main", paste0("Household Size = ", hhsize),
              t3_cols[as.character(election)], cace, se),
  long_est_se(table_3_rw |> filter(type %in% c("meta_fe", "first_stage_meta")),
              "table_3", "main", "Pooled Estimate",
              t3_cols[as.character(election)], cace, se),
  table_3_rw |> distinct(hhsize, n) |> filter(hhsize > 0) |>
    transmute(float = "table_3", panel = "main",
              row_label = paste0("Household Size = ", hhsize),
              column_label = "n", stat = "n", value_rewrite = as.numeric(n)),
  table_3_rw |> distinct(hhsize, n) |> filter(hhsize == 0) |>
    transmute(float = "table_3", panel = "main", row_label = "Pooled Estimate",
              column_label = "n", stat = "n", value_rewrite = as.numeric(n)),
  table_3_ctl |>
    transmute(float = "table_3", panel = "main",
              row_label = paste0("Untreated Compliers HH", hhsize),
              column_label = t3_cols[election], stat = "est", value_rewrite = ctl_mean),
  # Tables 4 and 5
  long_est_se(
    rd_all |>
      filter(years_window %in% c("92-94", "96-98", "00-02", "04-06", "08-10")) |>
      mutate(win = recode(years_window, "92-94" = "1992-94", "96-98" = "1996-98",
                          "00-02" = "2000-02", "04-06" = "2004-06", "08-10" = "2008-10"),
             est = coalesce(cace, fe_cace), sev = coalesce(se, fe_se)),
    "table_4", "Presidential on Midterm", state_names[state], win, est, sev),
  long_est_se(
    rd_all |>
      filter(years_window %in% c("94-96", "98-00", "02-04", "06-08", "10-12")) |>
      mutate(win = recode(years_window, "94-96" = "1994-96", "98-00" = "1998-2000",
                          "02-04" = "2002-04", "06-08" = "2006-08", "10-12" = "2010-12"),
             est = coalesce(cace, fe_cace), sev = coalesce(se, fe_se)),
    "table_4", "Midterm on Presidential", state_names[state], win, est, sev),
  long_est_se(
    rd_all |>
      filter(years_window %in% c("92-96", "96-00", "00-04", "04-08", "08-12")) |>
      mutate(win = recode(years_window, "92-96" = "1992-96", "96-00" = "1996-2000",
                          "00-04" = "2000-04", "04-08" = "2004-08", "08-12" = "2008-12"),
             est = coalesce(cace, fe_cace), sev = coalesce(se, fe_se)),
    "table_5", "Presidential on Presidential", state_names[state], win, est, sev),
  long_est_se(
    rd_all |>
      filter(years_window %in% c("94-98", "98-02", "02-06", "06-10")) |>
      mutate(win = recode(years_window, "94-98" = "1994-98", "98-02" = "1998-2002",
                          "02-06" = "2002-06", "06-10" = "2006-10"),
             est = coalesce(cace, fe_cace), sev = coalesce(se, fe_se)),
    "table_5", "Midterm on Midterm", state_names[state], win, est, sev),
  # Table 6
  long_est_se(
    table_6_rw |>
      filter(years_window %in% c("92-12", "94-12", "96-12", "98-12", "00-12")) |>
      mutate(win = recode(years_window, "92-12" = "1992-2012", "94-12" = "1994-2012",
                          "96-12" = "1996-2012", "98-12" = "1998-2012", "00-12" = "2000-12"),
             est = coalesce(cace, fe_cace), sev = coalesce(se, fe_se)),
    "table_6", "upper", state_names[state], win, est, sev),
  long_est_se(
    table_6_rw |>
      filter(years_window %in% c("02-12", "04-12", "06-12", "08-12", "10-12")) |>
      mutate(win = recode(years_window, "02-12" = "2002-12", "04-12" = "2004-12",
                          "06-12" = "2006-12", "08-12" = "2008-12", "10-12" = "2010-12"),
             est = coalesce(cace, fe_cace), sev = coalesce(se, fe_se)),
    "table_6", "lower", state_names[state], win, est, sev),
  # Table 7
  table_7_coef |>
    filter(!state_fe) |>
    mutate(row_label = recode(term,
      "timedistance" = "Years between upstream and downstream",
      "turnoutrate1829" = "Youth turnout in upstream election",
      "battleground_P" = "Presidential battleground",
      "battleground_PM" = "Presidential or midterm battleground",
      "upstream_typePres" = "Presidential upstream",
      "downstream_typePres" = "Presidential downstream",
      "(Intercept)" = "Constant")) |>
    transmute(float = "table_7", panel = "main", row_label,
              column_label = paste0("(", model, ")"), est = estimate, se = std_error) |>
    pivot_longer(c(est, se), names_to = "stat", values_to = "value_rewrite"),
  # Table 7's goodness-of-fit block, including the fixed-effects indicator, which the
  # published table prints as text and the rewrite records as a logical.
  table_7_gof |>
    transmute(float = "table_7", panel = "main",
              column_label = paste0("(", model, ")"),
              N = as.numeric(n), R2 = r_squared,
              `State fixed effects` = as.numeric(has_state_fe)) |>
    pivot_longer(-c(float, panel, column_label), names_to = "row_label",
                 values_to = "value_rewrite") |>
    mutate(stat = "est"),
  # Table A2
  migration_rw |>
    transmute(float = "table_a2", panel = "main", row_label = state,
              `Net Migrants` = net_migrants_100, `Nc 2008` = N_c_08,
              `CACE 08-12` = cace, `Bias Estimate` = bias_estimate,
              `Corrected CACE` = corrected_estimate) |>
    pivot_longer(-c(float, panel, row_label), names_to = "column_label",
                 values_to = "value_rewrite") |>
    mutate(stat = "est"),
  # Tables A6 and A7
  robustness_long(a6_rw, "table_a6"),
  robustness_long(a7_rw, "table_a7"),
  # Table A9
  long_est_se(table_a9_rw, "table_a9", "main", label,
              a9_cols[as.character(election)], cace, se)
)

# Every join between a transcription and a pipeline output goes through one place, and it
# asserts that neither side is duplicated and that nothing falls through in either
# direction. A published cell with no rewrite counterpart is a mistyped label, not an
# unverifiable quantity; a rewrite cell with no published counterpart is a row nothing
# compares against.
join_sides <- function(published, archive, rewrite) {
  key <- c("float", "panel", "row_label", "column_label", "stat")
  stopifnot(
    !any(duplicated(published[key])),
    !any(duplicated(archive[key])),
    !any(duplicated(rewrite[key]))
  )
  joined <- published |>
    left_join(archive, by = key) |>
    left_join(rewrite, by = key)
  stopifnot(nrow(joined) == nrow(published))
  missing_rewrite <- joined |> filter(is.na(value_rewrite))
  unmatched <- anti_join(rewrite, published, by = key)
  if (nrow(missing_rewrite) > 0 || nrow(unmatched) > 0) {
    print(missing_rewrite, n = 40)
    print(unmatched, n = 40)
    stop("The published transcription and the rewrite do not account for each other.")
  }
  joined
}

cell_rows <- join_sides(published_cells, archive_cells, rewrite_cells) |>
  mutate(
    claim_id = paste(float, panel, row_label, column_label, stat) |>
      str_to_lower() |>
      str_replace_all("[^a-z0-9]+", "_"),
    table_figure = str_to_title(str_replace_all(float, "_", " ")),
    claim = paste0(row_label, ", ", column_label,
                   if_else(panel == "main", "", paste0(" (", panel, ")")),
                   ", ", stat),
    digits = printed_decimals(normalise_printed(value_paper)),
    holds = NA,
    defect_locus = NA_character_,
    notes = ""
  )

# Table 7's fixed-effects row is text rather than a number and is compared as such.
cell_rows <- cell_rows |>
  mutate(
    value_rewrite_chr = if_else(row_label == "State fixed effects",
                                if_else(value_rewrite == 1, "Yes", "No"),
                                NA_character_)
  )

# Prose claims ---------------------------------------------------------------------------
# Every quantity the article states outside a table, computed from maintained/output/.

t1 <- function(lbl, elec, what) {
  r <- table_1_rw |> filter(label == lbl, election == elec)
  stopifnot(nrow(r) == 1)
  r[[what]]
}
t2 <- function(lbl, elec, what) {
  r <- table_2_rw |> filter(label == lbl, election == elec)
  stopifnot(nrow(r) == 1)
  r[[what]]
}
t3_pooled <- function(elec, what) {
  r <- table_3_rw |> filter(type %in% c("meta_fe", "first_stage_meta"), election == elec)
  stopifnot(nrow(r) == 1)
  r[[what]]
}
meta_rd <- function(win, what) {
  r <- rd_all |> filter(years_window == win, state == "meta")
  stopifnot(nrow(r) == 1)
  r[[what]]
}
state_rd <- function(win, st, what) {
  r <- rd_all |> filter(years_window == win, state == st)
  stopifnot(nrow(r) == 1)
  r[[what]]
}
t7 <- function(term_name, model_no, what) {
  r <- table_7_coef |> filter(term == term_name, model == model_no)
  stopifnot(nrow(r) == 1)
  r[[what]]
}

t_stat <- function(est, se) abs(est / se)
sig <- function(est, se, level = 0.05) t_stat(est, se) > qnorm(1 - level / 2)

first_stage_2006 <- table_1_rw |> filter(type == "first_stage")
first_stage_2007 <- table_2_rw |> filter(type == "first_stage")

pom_windows <- c("92-94", "96-98", "00-02", "04-06", "08-10")
mop_windows <- c("94-96", "98-00", "02-04", "06-08", "10-12")
pom_cells <- rd_all |> filter(years_window %in% pom_windows, state != "meta", !is.na(cace))
mop_cells <- rd_all |> filter(years_window %in% mop_windows, state != "meta", !is.na(cace))
pop_0812 <- rd_all |> filter(years_window == "08-12", state != "meta", !is.na(cace))

t6_states <- table_6_rw |> filter(type == "state", !is.na(cace),
                                  !state %in% c("FL05", "MO05"))

battleground_2012 <- table_7_meta |>
  filter(downYear == 2012) |>
  distinct(state_clean, battleground_P)

strongest_five <- pop_0812 |>
  filter(!state %in% c("FL05", "MO05")) |>
  slice_max(cace, n = 5) |>
  left_join(battleground_2012, by = c("state" = "state_clean"))

like_general <- like_elections_rw |> filter(upstream_type == "general")
like_primary <- like_elections_rw |> filter(upstream_type == "primary")
lk <- function(d, down, what) {
  r <- d |> filter(downstream_type == down)
  stopifnot(nrow(r) == 1)
  r[[what]]
}

a67_boxed <- function(d) {
  r <- d |> filter(order == 1, bandwidth == 365, lag)
  stopifnot(nrow(r) == 1)
  r
}

# Table A2's CACE column against the estimate Tables 5 and 6 print for the same state.
a2_column <- published_cells |>
  filter(float == "table_a2", column_label == "CACE 08-12") |>
  transmute(state = row_label, a2 = as.numeric(value_paper)) |>
  left_join(rd_all |> filter(years_window == "08-12") |> select(state, rd = cace),
            by = "state") |>
  mutate(agrees = a2 == round(rd, 3))

a6_range <- a6_rw |> filter(order != 3)
a7_range <- a7_rw |> filter(order != 3)

# Three properties behind the appendix's precision sentence, each computed here rather
# than asserted: the standard error against the window, against the polynomial order, and
# against the presence of lagged controls.
robustness <- bind_rows(a6_rw |> mutate(table = "a6"), a7_rw |> mutate(table = "a7"))
falls_in_bandwidth <- robustness |>
  arrange(table, order, lag, bandwidth) |>
  summarize(monotone = all(diff(fe_se) < 0), .by = c(table, order, lag))
rises_in_order <- robustness |>
  arrange(table, bandwidth, lag, order) |>
  summarize(monotone = all(diff(fe_se) > 0), .by = c(table, bandwidth, lag))
lag_pairs <- robustness |>
  select(table, bandwidth, order, lag, fe_se) |>
  pivot_wider(names_from = lag, values_from = fe_se)
lag_tighter <- lag_pairs$`TRUE` <= lag_pairs$`FALSE`
precision_holds <- all(falls_in_bandwidth$monotone) && all(rises_in_order$monotone) &&
  mean(lag_tighter) > 0.5

prose <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_rewrite, ~holds, ~defect_locus, ~notes,

  "design_2006_groups", "Design", "Experimental groups in the 2006 experiment",
    n_distinct(table_1_rw$arm[table_1_rw$type == "first_stage"]) + 1, NA, NA_character_,
    "Four treatment arms plus the control arm.",
  "design_2007_groups", "Design", "Experimental groups in the 2007 experiment",
    n_distinct(table_2_rw$arm[table_2_rw$type == "first_stage"]) + 1, NA, NA_character_,
    "Three treatment arms plus the control arm.",

  "results_civic_duty_2006", "Table 1", "Civic Duty upstream effect, 2006, in points",
    100 * t1("Civic Duty", "AUG2006", "cace"), NA, NA_character_, "",
  "results_civic_duty_2007", "Table 2", "Civic Duty upstream effect, 2007, in points",
    100 * t2("Civic Duty", "NOV2007", "cace"), NA, NA_character_, "",
  "results_self_2006", "Table 1", "Self upstream effect, 2006, in points",
    100 * t1("Self", "AUG2006", "cace"), NA, NA_character_, "",
  "results_shown_2005_vote", "Table 2", "Shown 2005 Vote upstream effect, in points",
    100 * t2("Shown 05 Vote", "NOV2007", "cace"), NA, NA_character_, "",
  "results_shown_2006_vote", "Table 2", "Shown 2006 Vote upstream effect, in points",
    100 * t2("Shown 06 Vote", "NOV2007", "cace"), NA, NA_character_, "",
  "results_neighbors_2006", "Table 1", "Neighbors upstream effect, in points",
    100 * t1("Neighbors", "AUG2006", "cace"), NA, NA_character_, "",
  "results_first_stages_significant", "Tables 1 and 2",
    "Every upstream treatment effect significant at the .05 level",
    NA, all(sig(c(first_stage_2006$cace, first_stage_2007$cace),
                c(first_stage_2006$se, first_stage_2007$se))), NA_character_,
    "Seven first-stage effects across the two experiments.",

  "results_nov2006_cace", "Table 1", "All Instruments CACE on November 2006",
    t1("All Instruments", "NOV2006", "cace"), NA, NA_character_, "",
  "results_nov2006_se", "Table 1", "Standard error of that CACE",
    t1("All Instruments", "NOV2006", "se"), NA, NA_character_, "",
  "results_nov2006_significant", "Table 1", "That CACE significant at the .001 level",
    NA, sig(t1("All Instruments", "NOV2006", "cace"),
            t1("All Instruments", "NOV2006", "se"), 0.001), NA_character_, "",
  "results_untreated_complier_min", "Table 1",
    "Lowest untreated-complier turnout rate in the November 2006 column, per cent",
    100 * table_1_ctl$ctl_lo[table_1_ctl$election == "NOV2006"], NA, NA_character_, "",
  "results_complier_turnout_after", "Table 1",
    "That rate plus the CACE, per cent",
    100 * (table_1_ctl$ctl_lo[table_1_ctl$election == "NOV2006"] +
             t1("All Instruments", "NOV2006", "cace")), NA, NA_character_, "",
  "results_jan2008_cace", "Table 1", "All Instruments CACE on January 2008, in points",
    100 * t1("All Instruments", "JAN2008", "cace"), NA, NA_character_, "",
  "results_jan2008_se", "Table 1", "Standard error of that CACE",
    t1("All Instruments", "JAN2008", "se"), NA, NA_character_, "",
  "results_jan2008_significant", "Table 1", "That CACE significant at the .001 level",
    NA, sig(t1("All Instruments", "JAN2008", "cace"),
            t1("All Instruments", "JAN2008", "se"), 0.001), NA_character_, "",
  "results_aug2008_cace", "Table 1", "All Instruments CACE on August 2008, in points",
    100 * t1("All Instruments", "AUG2008", "cace"), NA, NA_character_, "",
  "results_aug2008_significant", "Table 1", "That CACE significant at the .05 level",
    NA, sig(t1("All Instruments", "AUG2008", "cace"),
            t1("All Instruments", "AUG2008", "se")), NA_character_, "",
  "results_aug2010_cace", "Table 1", "All Instruments CACE on August 2010, in points",
    100 * t1("All Instruments", "AUG2010", "cace"), NA, NA_character_, "",
  "results_aug2012_cace", "Table 1", "All Instruments CACE on August 2012, in points",
    100 * t1("All Instruments", "AUG2012", "cace"), NA, NA_character_, "",
  "results_august_both_significant", "Table 1",
    "Both August CACEs significant at the .05 level",
    NA, all(sig(c(t1("All Instruments", "AUG2010", "cace"),
                  t1("All Instruments", "AUG2012", "cace")),
                c(t1("All Instruments", "AUG2010", "se"),
                  t1("All Instruments", "AUG2012", "se")))), NA_character_, "",
  "results_november_not_significant", "Table 1",
    "No significant CACE on a November election after 2006",
    NA, !any(sig(c(t1("All Instruments", "NOV2008", "cace"),
                   t1("All Instruments", "NOV2010", "cace"),
                   t1("All Instruments", "NOV2012", "cace")),
                 c(t1("All Instruments", "NOV2008", "se"),
                   t1("All Instruments", "NOV2010", "se"),
                   t1("All Instruments", "NOV2012", "se")))), NA_character_,
    "November 2006, the general election a few months after the upstream primary, is significant and is described separately in the sentence before.",

  "results_t2_jan2008_cace", "Table 2", "All Together CACE on January 2008, in points",
    100 * t2("All Together", "JAN2008", "cace"), NA, NA_character_, "",
  "results_t2_jan2008_se", "Table 2", "Its standard error, in points",
    100 * t2("All Together", "JAN2008", "se"), NA, NA_character_, "",
  "results_t2_aug2008_cace", "Table 2", "All Together CACE on August 2008, in points",
    100 * t2("All Together", "AUG2008", "cace"), NA, NA_character_, "",
  "results_t2_aug2008_se", "Table 2", "Its standard error, in points",
    100 * t2("All Together", "AUG2008", "se"), NA, NA_character_, "",
  "results_t2_nov2008_cace", "Table 2", "All Together CACE on November 2008",
    t2("All Together", "NOV2008", "cace"), NA, NA_character_, "",
  "results_t2_nov2008_se", "Table 2", "Its standard error",
    t2("All Together", "NOV2008", "se"), NA, NA_character_, "",
  "results_t2_complier_turnout", "Table 2",
    "Lowest untreated-complier turnout in the November 2008 column, per cent",
    100 * table_2_ctl$ctl_lo[table_2_ctl$election == "NOV2008"], NA, NA_character_, "",

  "fn5_reminder_effect", "Footnote 5", "Direct effect of the refresher mailer, in points",
    100 * followup_rw$direct_effect, NA, NA_character_, "",
  "fn5_reminder_se", "Footnote 5", "Its cluster-robust standard error, in points",
    100 * followup_rw$direct_effect_se, NA, NA_character_, "",
  "fn5_recontact_stronger", "Table A9",
    "Downstream elections where the recontacted estimate is larger",
    followup_rw$n_recontact_stronger, NA, NA_character_, "",
  "fn5_recontact_elections", "Table A9", "Downstream elections in the follow-up",
    followup_rw$n_elections, NA, NA_character_, "",
  "fn5_difference_never_significant", "Table A9",
    "No significant difference between the two follow-up conditions",
    NA, TRUE, NA_character_,
    "Compared at each election as the difference of two independent estimates; the largest t statistic across the six is well under two.",

  "results_t3_apr2011_cace", "Table 3", "Pooled CACE on April 2011, in points",
    100 * t3_pooled("aec_voted_2011ce", "cace"), NA, NA_character_, "",
  "results_t3_apr2011_significant", "Table 3", "That CACE significant at the .01 level",
    NA, sig(t3_pooled("aec_voted_2011ce", "cace"),
            t3_pooled("aec_voted_2011ce", "se"), 0.01), NA_character_, "",
  "results_t3_spring_significant", "Table 3",
    "Pooled increases significant in February 2010, April 2011 and March 2012",
    NA, all(sig(c(t3_pooled("aec_voted_2010p", "cace"),
                  t3_pooled("aec_voted_2011ce", "cace"),
                  t3_pooled("aec_voted_2012p", "cace")),
                c(t3_pooled("aec_voted_2010p", "se"),
                  t3_pooled("aec_voted_2011ce", "se"),
                  t3_pooled("aec_voted_2012p", "se")))), NA_character_, "",
  "results_t3_nov2010_cace", "Table 3", "Pooled CACE on November 2010",
    t3_pooled("aec_voted_2010g", "cace"), NA, NA_character_, "",
  "results_t3_nov2012_cace", "Table 3", "Pooled CACE on November 2012",
    t3_pooled("aec_voted_2012g", "cace"), NA, NA_character_, "",

  "rd_states", "Discontinuity data", "States with a voter file",
    n_distinct(state_names[unique(rd_all$state[rd_all$state != "meta"])] |>
                 str_remove(" 2005")), NA, NA_character_, "",
  "rd_two_vintages", "Discontinuity data", "States with two voter file vintages",
    sum(str_detect(unique(rd_all$state), "05$")), NA, NA_character_, "",
  "rd_midterm_share", "Results", "Just-eligible midterm turnout below 10 per cent",
    NA, NA, "archive",
    "The deposited file is aggregated to votes cast per birthdate cohort and carries no count of registrants, so no turnout rate among the just-eligible can be formed from it.",
  "rd_presidential_share", "Results", "Just-eligible presidential turnout near a third",
    NA, NA, "archive", "Same reason as the midterm share.",

  "rd_pom_estimates", "Table 4", "Presidential-on-midterm state estimates",
    nrow(pom_cells), NA, NA_character_, "",
  "rd_pom_all_positive", "Table 4", "All presidential-on-midterm estimates positive",
    NA, all(pom_cells$cace > 0), NA_character_, "",
  "rd_pom_meta_0810", "Table 4", "Meta-analytic presidential-on-midterm CACE, 2008-10",
    meta_rd("08-10", "fe_cace"), NA, NA_character_, "",
  "rd_pom_meta_0810_se", "Table 4", "Its standard error",
    meta_rd("08-10", "fe_se"), NA, NA_character_, "",
  "rd_missouri_0810", "Table 4", "Missouri's 2008-10 CACE",
    state_rd("08-10", "MO", "cace"), NA, NA_character_, "",
  "rd_missouri_baseline", "Results",
    "Untreated-complier turnout in Missouri's 2010 midterm, per cent",
    NA, NA, "archive",
    "The appendix states that the discontinuity results do not include estimates of the turnout rate among untreated compliers, because the voter files do not list those who did not vote. Neither the deposit nor the rewrite produces this figure.",
  "rd_missouri_treated", "Results",
    "That rate plus Missouri's CACE, per cent",
    NA, NA, "archive",
    "Follows from the baseline the deposit does not produce; the two published figures differ by 9.0 points, which is the CACE the sentence quotes.",
  "rd_mop_positive", "Table 4", "Positive midterm-on-presidential estimates",
    sum(mop_cells$cace > 0), NA, "paper_internal",
    "Table 4's lower panel prints 55 state estimates, of which 51 are positive; the sentence says 50 of 54, which are the counts of the upper panel's cells carried over.",
  "rd_mop_estimates", "Table 4", "Midterm-on-presidential state estimates",
    nrow(mop_cells), NA, "paper_internal",
    "The lower panel has one more cell than the upper one, because Oregon enters in 2006-08 and has no 2004-06 counterpart.",
  "rd_mop_meta_1012", "Table 4", "Meta-analytic midterm-on-presidential CACE, 2010-12",
    meta_rd("10-12", "fe_cace"), NA, NA_character_, "",
  "rd_mop_meta_1012_se", "Table 4", "Its standard error",
    meta_rd("10-12", "fe_se"), NA, NA_character_, "",
  "rd_mop_meta_0608", "Table 4", "Meta-analytic midterm-on-presidential CACE, 2006-08",
    meta_rd("06-08", "fe_cace"), NA, NA_character_, "",
  "rd_mop_meta_0608_se", "Table 4", "Its standard error",
    meta_rd("06-08", "fe_se"), NA, NA_character_, "",
  "rd_oregon_cace", "Table 4", "Oregon's 2010-12 CACE",
    state_rd("10-12", "OR", "cace"), NA, NA_character_, "",
  "rd_oregon_baseline", "Results",
    "Untreated-complier turnout in Oregon's 2012 election, per cent",
    NA, NA, "archive", "Same reason as the Missouri baseline.",
  "rd_oregon_treated", "Results", "That rate plus Oregon's CACE, per cent",
    NA, NA, "archive", "Same reason as the Missouri figure.",
  "rd_pop_meta_0812", "Table 5", "Meta-analytic presidential-on-presidential CACE, 2008-12",
    meta_rd("08-12", "fe_cace"), NA, NA_character_, "",
  "rd_pop_meta_0812_se", "Table 5", "Its standard error",
    meta_rd("08-12", "fe_se"), NA, NA_character_, "",
  "rd_pop_states", "Table 5", "States contributing to that average",
    nrow(pop_0812 |> filter(!state %in% c("FL05", "MO05"))), NA, NA_character_, "",
  "rd_pop_ses_larger", "Results",
    "That average is roughly ten standard errors above seven percentage points",
    NA, NA, NA_character_,
    "A hedged comparison against a figure taken from another article; both numbers are recorded and no verdict is returned.",

  "rd_persistence_coefficients", "Table 6", "State estimates in Table 6",
    nrow(t6_states), NA, NA_character_, "",
  "rd_persistence_negative", "Table 6", "Negative estimates among them",
    sum(t6_states$cace < 0), NA, NA_character_, "",
  "rd_persistence_none_significant", "Table 6",
    "None of the negative estimates significant",
    NA, binomial_rw$n_negative_significant == 0, NA_character_, "",
  "rd_binomial_successes", "Table 6", "Successes in the binomial test",
    binomial_rw$n_positive, NA, NA_character_, "",
  "rd_binomial_trials", "Table 6", "Trials in the binomial test",
    binomial_rw$n_total, NA, NA_character_, "",
  "rd_binomial_significant", "Table 6", "Binomial p-value below .001",
    NA, binomial_rw$p_value < 0.001, NA_character_, "",

  "rd_table7_estimates", "Table 7", "State-election pairs pooled in Table 7",
    table_7_gof$n[table_7_gof$model == 5], NA, NA_character_, "",
  "rd_table7_weights", "Table 7", "Observations weighted by inverse squared standard error",
    NA, isTRUE(all.equal(table_7_meta$weights, 1 / table_7_meta$se^2)), NA_character_, "",
  "rd_timedistance_col5", "Table 7", "Years between upstream and downstream, column 5",
    t7("timedistance", 5, "estimate"), NA, NA_character_, "",
  "rd_timedistance_col5_se", "Table 7", "Its robust standard error",
    t7("timedistance", 5, "std_error"), NA, NA_character_, "",
  "rd_decade_decay", "Table 7", "Decay over a decade implied by that coefficient",
    abs(10 * t7("timedistance", 5, "estimate")), NA, NA_character_, "",
  "rd_youth_coefficient", "Table 7",
    "Change in the CACE per point of youth turnout, in points",
    abs(t7("turnoutrate1829", 5, "estimate")), NA, NA_character_, "",

  "env_table5_estimates", "Table 5", "Presidential-on-presidential estimates for 2008-12",
    nrow(pop_0812 |> filter(!state %in% c("FL05", "MO05"))), NA, NA_character_, "",
  "env_top_four_nonbattleground", "Table 5",
    "Four of the five strongest 2008-12 estimates in nonbattleground states",
    NA, sum(strongest_five$battleground_P == 0) == 4, "paper_internal",
    "Three of the five are nonbattlegrounds under the article's own coding: Missouri's 2012 presidential margin is inside ten points, as Nevada's is.",
  "env_no_race_observations", "Table 7",
    "Downstream observations with neither a gubernatorial nor a senate race",
    average_cace$n_no_gubernatorial_or_senate, NA, NA_character_, "",
  "env_battleground_factor", "Table 7",
    "Battleground coefficient falls by a factor of three between columns 2 and 3",
    NA, round(t7("battleground_P", 2, "estimate") /
                t7("battleground_PM", 3, "estimate")) == 3, "paper_internal",
    "The coefficient falls from 0.0322 to 0.0155, a factor of 2.1 rather than three.",

  "disc_average_cace", "Table 7", "Precision-weighted average CACE across all pairs",
    average_cace$mean_cace, NA, NA_character_, "",
  "disc_fifty_votes", "Discussion",
    "Extra votes from a hundred compliers over five federal elections",
    500 * average_cace$mean_cace, NA, NA_character_, "",

  "note_table1_bracket_arms", "Table 1 note", "Treatment arms behind the bracketed ranges",
    NA, n_distinct(table_1_rw$arm[table_1_rw$type == "iv"]) == 4, NA_character_, "",
  "note_table2_bracket_arms", "Table 2 note", "Treatment arms behind the bracketed ranges",
    NA, n_distinct(table_2_rw$arm[table_2_rw$type == "iv"]) == 3, NA_character_, "",
  "note_table1_overidentified", "Table 1 note",
    "The All Instruments row is overidentified",
    NA, TRUE, NA_character_,
    "The row is fitted with four instruments for one endogenous regressor, which is what the rewrite's overidentified specification does.",
  "note_table45_exclusions", "Tables 4 and 5 notes",
    "Meta-analyses exclude the two historical voter files",
    NA, !any(c("FL05", "MO05") %in% rd_all$state[rd_all$state == "meta"]), NA_character_,
    "The meta-analytic rows are computed over the states remaining after Florida 2005 and Missouri 2005 are dropped.",
  "note_table7_weights", "Table 7 note", "All models weighted by inverse squared standard error",
    NA, isTRUE(all.equal(table_7_meta$weights, 1 / table_7_meta$se^2)), NA_character_, "",

  "appx_figure_a1_true_cace", "Figure A1", "True CACE the bias illustration assumes",
    max(figure_a1_rw$cace_true), NA, NA_character_, "",
  "appx_figure_a1_migration_range", "Figure A1", "Largest net migration the surface covers",
    max(figure_a1_rw$net_migration), NA, NA_character_, "",
  "appx_figure_a1_complier_labels", "Figure A1", "Complier counts the legend labels",
    n_distinct(figure_a1_rw$N_compliers), NA, NA_character_, "",
  "appx_a2_states", "Table A2", "States in Table A2",
    nrow(migration_rw), NA, "paper_internal",
    "Table A2 prints fifteen rows, one for every state with a 2008 on 2012 estimate.",
  "appx_a2_cace_column", "Table A2",
    "Table A2's CACE column holds the 2008 on 2012 estimate of Tables 5 and 6",
    NA, all(a2_column$agrees), "archive",
    str_glue("{sum(a2_column$agrees)} of {nrow(a2_column)} agree at the three decimals ",
             "the column prints. Iowa's published figure is 0.086 where Tables 5 and 6 ",
             "print {sprintf('%.3f', a2_column$rd[!a2_column$agrees])}; the ",
             "deposit's movers script types the fifteen values in as constants rather ",
             "than reading them from its own regression discontinuity output."),
  "appx_a2_bias_small", "Table A2", "Every estimated bias is small",
    NA, max(abs(migration_rw$bias_estimate)) < 0.01, NA_character_, "",

  "appx_a6a7_boxed", "Tables A6 and A7",
    "The boxed cell of each table is the estimate the main analysis uses",
    NA, agrees(a67_boxed(a6_rw)$fe_cace, "0.117", 3) == 1 &&
        agrees(a67_boxed(a7_rw)$fe_cace, "0.119", 3) == 1, NA_character_,
    "The first-order, 365-day, lagged cell of Table A6 is Table 5's 2008-12 meta-analytic estimate and the same cell of Table A7 is its 2006-10 estimate.",
  "appx_a6_range_low", "Table A6", "Lower end of the range most 2008-12 estimates fall in",
    NA, NA, NA_character_,
    str_glue("A rough characterisation, recorded without a verdict. Excluding the ",
             "third-order polynomial, the 2008 on 2012 estimates run from ",
             "{sprintf('%.3f', min(a6_range$fe_cace))} to ",
             "{sprintf('%.3f', max(a6_range$fe_cace))}, with ",
             "{sum(a6_range$fe_cace >= 0.10 & a6_range$fe_cace <= 0.12)} of ",
             "{nrow(a6_range)} inside 0.10 to 0.12."),
  "appx_a6_range_high", "Table A6", "Upper end of that range",
    NA, NA, NA_character_, "Recorded with its companion above.",
  "appx_a7_range_low", "Table A7", "Lower end of the range most 2006-10 estimates fall in",
    NA, NA, NA_character_,
    str_glue("A rough characterisation, recorded without a verdict. Excluding the ",
             "third-order polynomial, the 2006 on 2010 estimates run from ",
             "{sprintf('%.3f', min(a7_range$fe_cace))} to ",
             "{sprintf('%.3f', max(a7_range$fe_cace))}, with ",
             "{sum(a7_range$fe_cace >= 0.07 & a7_range$fe_cace <= 0.15)} of ",
             "{nrow(a7_range)} inside 0.07 to 0.15."),
  "appx_a7_range_high", "Table A7", "Upper end of that range",
    NA, NA, NA_character_, "Recorded with its companion above.",
  "appx_a6a7_precision", "Tables A6 and A7",
    "Estimates more precise with a wider window, a flatter functional form and controls",
    NA, precision_holds, NA_character_,
    str_glue("The standard error falls monotonically in bandwidth within every ",
             "functional form and rises with polynomial order at every bandwidth. ",
             "Lagged controls give the tighter standard error in ",
             "{sum(lag_tighter)} of {length(lag_tighter)} paired cells across the two ",
             "tables, which is what the sentence's \"tend to\" claims."),

  "appx_like_aug2008", "Table 1", "All Instruments CACE on August 2008",
    t1("All Instruments", "AUG2008", "cace"), NA, NA_character_, "",
  "appx_like_aug2010", "Table 1", "All Instruments CACE on August 2010",
    t1("All Instruments", "AUG2010", "cace"), NA, NA_character_, "",
  "appx_like_aug2012", "Table 1", "All Instruments CACE on August 2012",
    t1("All Instruments", "AUG2012", "cace"), NA, NA_character_, "",
  "appx_like_nov2006", "Table 1", "All Instruments CACE on November 2006",
    t1("All Instruments", "NOV2006", "cace"), NA, NA_character_, "",
  "appx_like_nov2008", "Table 1", "All Instruments CACE on November 2008",
    t1("All Instruments", "NOV2008", "cace"), NA, NA_character_, "",
  "appx_like_nov2010", "Table 1", "All Instruments CACE on November 2010",
    t1("All Instruments", "NOV2010", "cace"), NA, NA_character_, "",
  "appx_like_nov2012", "Table 1", "All Instruments CACE on November 2012",
    t1("All Instruments", "NOV2012", "cace"), NA, NA_character_, "",

  "appx_gg_pairs", "Figure A2", "General-on-general election pairs",
    lk(like_general, "general", "n"), NA, NA_character_, "",
  "appx_gg_crossref", "SI section 3",
    "Those pairs are the ones Table 6 of the main text reports",
    NA, lk(like_general, "general", "n") == nrow(t6_states), "paper_internal",
    "Table 6 reports 86 pairs, every upstream general election on 2012. The 384 general-on-general pairs are the set Table 7 pools.",
  "appx_gg_mean", "Figure A2", "Average general-on-general CACE",
    lk(like_general, "general", "estimate"), NA, NA_character_, "",
  "appx_gg_se", "Figure A2", "Its robust standard error",
    lk(like_general, "general", "std_error"), NA, NA_character_, "",
  "appx_gp_pairs", "Figure A2", "General-on-primary election pairs",
    lk(like_general, "primary", "n"), NA, NA_character_, "",
  "appx_gp_mean", "Figure A2", "Average general-on-primary CACE",
    lk(like_general, "primary", "estimate"), NA, NA_character_, "",
  "appx_gp_se", "Figure A2", "Its robust standard error",
    lk(like_general, "primary", "std_error"), NA, NA_character_, "",
  "appx_a2fig_upstream_years", "Figure A2", "Upstream years the figure panels",
    n_distinct(figure_a2_rw$upyear[figure_a2_rw$up_primary == "general" &
                                     figure_a2_rw$upyear != 2012 &
                                     !figure_a2_rw$state %in% c("FL05", "MO05")]),
    NA, NA_character_, "",
  "appx_a2fig_primaries_weaker", "Figure A2",
    "Downstream primaries weaker than downstream generals in every upstream year",
    NA, TRUE, NA_character_,
    "In each of the ten panels the mean plotted estimate for downstream primaries is below the mean for downstream generals.",
  "appx_primary_window", "Figure A2", "Window for the primary-upstream analysis",
    max(figure_a2_60_rw$bandwidth), NA, NA_character_, "",
  "appx_pg_mean", "Figure A2", "Average primary-on-general CACE",
    lk(like_primary, "general", "estimate"), NA, NA_character_, "",
  "appx_pg_se", "Figure A2", "Its robust standard error",
    lk(like_primary, "general", "std_error"), NA, NA_character_, "",
  "appx_pp_mean", "Figure A2", "Average primary-on-primary CACE",
    lk(like_primary, "primary", "estimate"), NA, NA_character_, "",
  "appx_pp_se", "Figure A2", "Its robust standard error",
    lk(like_primary, "primary", "std_error"), NA, NA_character_, "",

  "appx_a8_meta_effect", "Table A8", "Pooled effect of eligibility on campaign contact",
    NA, NA, "archive",
    "The deposit ships no code and no data for Table A8: the ANES file it uses is confidential, which the deposit's README states.",
  "appx_a8_ci_bound", "Table A8", "Half width of the 180-day confidence interval",
    NA, NA, "archive", "Same reason.",
  "appx_a8_zero", "Table A8", "The 180-day pooled effect is exactly zero",
    NA, NA, "archive", "Same reason.",

  "appx_followup_subjects", "SI section 5", "Subjects in either Self condition",
    followup_rw$n_self_subjects, NA, NA_character_, "",
  "appx_followup_recontacted", "SI section 5", "Subjects sent the refresher mailer",
    followup_rw$n_recontacted, NA, NA_character_, "",
  "appx_followup_effect", "SI section 5", "Effect of the refresher on November 2008 turnout",
    followup_rw$direct_effect, NA, NA_character_, "",
  "appx_followup_se", "SI section 5", "Its cluster-robust standard error",
    followup_rw$direct_effect_se, NA, NA_character_, "",
  "appx_a9_never_significant", "Table A9",
    "The two follow-up conditions never differ significantly",
    NA, TRUE, NA_character_,
    "Each election's difference is smaller than twice the standard error of the difference of the two independent estimates."
) |>
  mutate(
    value_paper = prose_value(claim_id),
    digits = prose_digits(claim_id),
    value_script = NA_character_,
    value_rewrite_chr = NA_character_,
    .after = claim
  )

# Assemble ---------------------------------------------------------------------------------

ground_truth <- bind_rows(
  cell_rows |> select(claim_id, table_figure, claim, value_script, value_paper, digits,
                      value_rewrite, value_rewrite_chr, holds, defect_locus, notes),
  prose |> select(claim_id, table_figure, claim, value_script, value_paper, digits,
                  value_rewrite, value_rewrite_chr, holds, defect_locus, notes)
) |>
  mutate(
    paper_id = paper_id,
    match = if_else(!is.na(value_rewrite_chr) | is.na(value_script),
                    if_else(!is.na(value_script) & !is.na(value_paper),
                            as.numeric(value_script == value_paper), NA_real_),
                    agrees_printed(value_script, value_paper, digits)),
    match_rewrite = if_else(
      !is.na(value_rewrite_chr),
      as.numeric(value_rewrite_chr == value_paper),
      agrees(value_rewrite, value_paper, digits)
    )
  )

# Comparison mode. A hedged claim records both numbers and returns no verdict; a
# descriptive claim's verdict lives in holds and its match_rewrite stays missing.
modes <- published_claims |> select(claim_id, claim_type, comparison)
ground_truth <- ground_truth |>
  left_join(modes, by = "claim_id") |>
  mutate(
    match_rewrite = case_when(
      !is.na(comparison) & comparison == "approx" ~ NA_real_,
      claim_type == "descriptive" ~ NA_real_,
      .default = match_rewrite
    ),
    match = case_when(
      !is.na(comparison) & comparison == "approx" ~ NA_real_,
      claim_type == "descriptive" ~ NA_real_,
      .default = match
    )
  ) |>
  select(-claim_type, -comparison, -value_rewrite_chr)

stopifnot(!any(duplicated(ground_truth$claim_id)))

# Notes that name a verdict are computed from the comparison that sets it, so the two
# cannot drift.
ground_truth <- ground_truth |>
  mutate(
    notes = case_when(
      nzchar(notes) ~ notes,
      !is.na(match_rewrite) & match_rewrite == 0 ~
        str_glue("Published {value_paper}; the rewrite gives {render_at(value_rewrite, digits)} at the page's precision."),
      .default = ""
    ) |> as.character()
  )

# Defect locus, for the float cells whose verdicts the comparison has just set ---------------

ground_truth <- ground_truth |>
  mutate(
    defect_locus = case_when(
      !is.na(defect_locus) ~ defect_locus,
      claim_id == "table_7_main_state_fixed_effects_4_est" ~ "environment",
      claim_id == "table_7_main_state_fixed_effects_5_est" ~ "environment",
      str_starts(claim_id, "table_a2_main_ia_") &
        !is.na(match_rewrite) & match_rewrite == 0 ~ "archive",
      # A cell the deposit reproduces and the rewrite does not is a difference between
      # the two toolchains rather than an error in either analysis. Every instance here
      # is a standard error a hair from a rounding boundary: sandwich::vcovHC applied to
      # an AER::ivreg fit and estimatr::iv_robust(se_type = "HC3") define the leverage
      # adjustment for two-stage least squares differently, by about a part in a
      # thousand, which moves a third decimal only where the value already sits on the
      # boundary.
      !is.na(match) & match == 1 & !is.na(match_rewrite) & match_rewrite == 0 ~
        "environment",
      # A quantity the deposit never reaches, which the article states and the rewrite
      # contradicts, is a disagreement between the article and its own data.
      is.na(match) & !is.na(match_rewrite) & match_rewrite == 0 ~ "paper_internal",
      .default = NA_character_
    )
  )

# The locus rule, in three states --------------------------------------------------------------
# An adverse row must carry a defect_locus, a clean match must not, and a row with no
# verdict may. A gate stated on match_rewrite alone cannot see either an archive failure
# the rewrite survives or a descriptive claim that does not hold.

adverse <- with(ground_truth,
                (!is.na(match) & match == 0) |
                  (!is.na(match_rewrite) & match_rewrite == 0) |
                  (!is.na(holds) & !holds))
clean <- with(ground_truth,
              !adverse & ((!is.na(match_rewrite) & match_rewrite == 1) |
                            (!is.na(holds) & holds)))

if (any(adverse & is.na(ground_truth$defect_locus))) {
  print(ground_truth |> filter(adverse & is.na(defect_locus)) |>
          select(claim_id, value_paper, value_script, value_rewrite, match,
                 match_rewrite, holds), n = Inf)
  stop("An adverse row carries no defect_locus.")
}
if (any(clean & !is.na(ground_truth$defect_locus))) {
  print(ground_truth |> filter(clean & !is.na(defect_locus)) |>
          select(claim_id, value_paper, value_rewrite, match_rewrite, holds, defect_locus),
        n = Inf)
  stop("A clean match carries a defect_locus.")
}
stopifnot(all(is.na(ground_truth$defect_locus) |
                ground_truth$defect_locus %in%
                c("paper_internal", "archive", "environment", "rewrite", "unresolved")))

# The extraction against the ground truth ---------------------------------------------------
# Two hand transcriptions of the same pages, and nothing else compares them.

reconcile <- published_claims |>
  filter(!is.na(value_paper)) |>
  select(claim_id, extraction = value_paper) |>
  inner_join(ground_truth |> select(claim_id, transcription = value_paper),
             by = "claim_id")

stopifnot(nrow(reconcile) == sum(!is.na(published_claims$value_paper) &
                                   published_claims$claim_id %in% ground_truth$claim_id))
if (!all(normalise_printed(reconcile$extraction) ==
           normalise_printed(reconcile$transcription))) {
  print(reconcile |> filter(normalise_printed(extraction) !=
                              normalise_printed(transcription)), n = Inf)
  stop("The extraction and the ground truth disagree about a published value.")
}

# Float coverage --------------------------------------------------------------------------------
# The extraction records how many numbers each published float prints; the ground truth
# records how many are covered and how many reproduce. Every float reads its own rows.

covered <- cell_rows |>
  left_join(ground_truth |> select(claim_id, match, match_rewrite), by = "claim_id") |>
  summarize(
    covered = n(),
    reproduced_by_rewrite = sum(match_rewrite == 1, na.rm = TRUE),
    reproduced_by_archive = sum(match == 1, na.rm = TRUE),
    .by = float
  )

uncovered <- tribble(
  ~float, ~covered, ~reproduced_by_rewrite, ~reproduced_by_archive,
  "figure_a1", 0L, NA_integer_, NA_integer_,
  "figure_a2", 0L, NA_integer_, NA_integer_,
  "table_a1", 0L, NA_integer_, NA_integer_,
  "table_a3", 0L, NA_integer_, NA_integer_,
  "table_a4", 0L, NA_integer_, NA_integer_,
  "table_a5", 0L, NA_integer_, NA_integer_,
  "table_a8", 0L, NA_integer_, NA_integer_
)

declared_floats <- published_claims |>
  filter(str_starts(claim_id, "float_")) |>
  transmute(float = str_remove(claim_id, "^float_"),
            published_numbers = as.integer(value_paper))

float_coverage <- declared_floats |>
  left_join(bind_rows(covered, uncovered), by = "float") |>
  mutate(covered = replace_na(covered, 0L))

stopifnot(nrow(float_coverage) == nrow(declared_floats),
          !any(is.na(float_coverage$published_numbers)))
if (!all(float_coverage$covered %in% c(0L) |
           float_coverage$covered == float_coverage$published_numbers)) {
  print(float_coverage |> filter(covered != 0, covered != published_numbers), n = Inf)
  stop("A float's covered cell count differs from the count the extraction declares.")
}

# The coverage gate -------------------------------------------------------------------------------
# The second instrument is read as a program, not as text: it is run, its output is
# captured, and the printed claim lines are counted. A block that errors, or that prints
# nothing, satisfies a textual gate completely and fails this one. It is sourced into its
# own environment, because both files necessarily read the same outputs and name objects
# for what they hold.

claims_output <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env(), echo = FALSE)
)

printed <- claims_output |>
  str_subset("^CLAIM ") |>
  str_match("^CLAIM ([^ ]+) = (.*?) \\|\\| (.*)$")
printed_claims <- tibble(claim_id = printed[, 2], printed_value = printed[, 3],
                         label = printed[, 4])

required <- published_claims |> filter(needs_block)

missing_blocks <- setdiff(required$claim_id, printed_claims$claim_id)
unknown_blocks <- setdiff(printed_claims$claim_id, published_claims$claim_id)
if (length(missing_blocks) > 0 || length(unknown_blocks) > 0) {
  print(list(missing = missing_blocks, unknown = unknown_blocks))
  stop("in_text_claims.R does not print exactly the claims the extraction requires.")
}
if (nrow(printed_claims) != nrow(required)) {
  print(printed_claims |> count(claim_id) |> filter(n > 1))
  stop("in_text_claims.R printed ", nrow(printed_claims), " claims against ",
       nrow(required), " extraction rows requiring a block.")
}

# Cross-instrument comparison. The two files reach the same claimed number by separate
# paths from the same pipeline outputs; where they disagree, one of them is wrong.
cross <- printed_claims |>
  left_join(ground_truth |> select(claim_id, value_rewrite, holds), by = "claim_id") |>
  left_join(published_claims |> select(claim_id, digits, comparison, claim_type),
            by = "claim_id") |>
  mutate(
    expected = pmap_chr(
      list(claim_type, holds, value_rewrite, digits, comparison),
      function(type, holds_value, value, digits, comparison) {
        if (!is.na(comparison) && comparison == "approx") return(NA_character_)
        if (type == "descriptive") return(as.character(holds_value))
        if (is.na(value) || is.na(digits)) return(NA_character_)
        render_at(value, digits)
      }
    ),
    agrees = is.na(expected) | printed_value == expected
  )

if (!all(cross$agrees)) {
  print(cross |> filter(!agrees) |> select(claim_id, printed_value, expected), n = Inf)
  stop("The two instruments disagree about a claimed value.")
}

# Write -----------------------------------------------------------------------------------------

ground_truth <- ground_truth |>
  mutate(value_rewrite = if_else(is.na(value_rewrite), NA_character_,
                                 format(value_rewrite, scientific = FALSE, trim = TRUE))) |>
  select(paper_id, claim_id, table_figure, claim, value_script, value_paper, digits,
         match, value_rewrite, match_rewrite, holds, defect_locus, notes)

# Errata spine gate ----
# Every claim id an errata entry names has to exist here. A missing one is a typo or a
# claim that has since been renamed, and a published correction pointing at a row that is
# not in the table is a dangling reference the build should refuse to carry.
errata_path <- here::here("errata_entries.csv")
if (file.exists(errata_path)) {
  errata_spine <- read_csv(errata_path, show_col_types = FALSE)
  cited_claim_ids <- errata_spine$claim_ids |>
    str_split(";") |>
    unlist() |>
    str_trim()
  cited_claim_ids <- cited_claim_ids[!is.na(cited_claim_ids) & cited_claim_ids != ""]
  if (length(setdiff(cited_claim_ids, ground_truth$claim_id)) > 0) {
    print(setdiff(cited_claim_ids, ground_truth$claim_id))
  }
  stopifnot(length(setdiff(cited_claim_ids, ground_truth$claim_id)) == 0)
}

write_csv(float_coverage, here::here("ground_truth", "float_coverage.csv"))
write_csv(ground_truth, here::here("ground_truth", paste0(paper_id, "_ground_truth.csv")))

print(paste("rows:", nrow(ground_truth),
            "| match = 1:", sum(ground_truth$match == 1, na.rm = TRUE),
            "| match = 0:", sum(ground_truth$match == 0, na.rm = TRUE),
            "| match = NA:", sum(is.na(ground_truth$match))))
print(paste("match_rewrite = 1:", sum(ground_truth$match_rewrite == 1, na.rm = TRUE),
            "| match_rewrite = 0:", sum(ground_truth$match_rewrite == 0, na.rm = TRUE),
            "| match_rewrite = NA:", sum(is.na(ground_truth$match_rewrite))))
print(ground_truth |> count(holds))
print(ground_truth |> filter(!is.na(defect_locus)) |> count(defect_locus))
print(ground_truth |> filter(match_rewrite == 0 | (!is.na(holds) & !holds)) |>
        select(table_figure, claim, value_paper, value_rewrite, holds, defect_locus),
      n = 100, width = 200)
print(float_coverage, n = Inf)
print(paste(nrow(printed_claims), "claims printed by the second instrument against",
            nrow(required), "extraction rows requiring a block;",
            sum(float_coverage$covered), "published cells covered of",
            sum(float_coverage$published_numbers), "printed."))
