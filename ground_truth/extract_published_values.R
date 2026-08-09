# coppock_green_2016/ground_truth/extract_published_values.R
# Output: ground_truth/published_maintext_tables.csv,
#   ground_truth/published_appendix_values.csv
# Depends on: the published article and its supporting information, and pdftotext.
#   Both PDFs live in the catalog rather than in this repository, so their directory
#   comes from PUBLISHED_PDF_DIR.
# Description: Transcribe every cell of every published table this repository can
#   compare against, so value_paper is a reading of the page rather than a typed
#   number. Not part of run_all.R: it reads documents the repository does not carry,
#   and its output is committed.
#
#   Two ways of reading a page are used, and which one applies depends on whether the
#   table has holes in it. Tables 1, 2, 3, A2, A6, A7 and A9 print a value in every
#   cell of every row, so their rows are read in order out of pdftotext's layout text
#   and the few rows that start in the second column say so. Tables 4, 5, 6 and 7 are
#   mostly empty: a state appears only in the years its voter file covers, so reading
#   in order would slide every value in a sparse row to the left. Those are parsed
#   positionally from pdftotext -bbox-layout, with each number assigned to whichever
#   column of the table's one complete row it sits nearest.
#
#   Artifacts of this document, checked against rendered pages: the minus sign is
#   U+2212 rather than a hyphen, and thousands separators in the main text's n rows
#   come back as "36, 903" with a space after the comma.
#
#   Usage: PUBLISHED_PDF_DIR=/path/to/original_materials Rscript \
#     ground_truth/extract_published_values.R

library(here)
library(tidyverse)

here::i_am("ground_truth/extract_published_values.R")

pdf_dir <- Sys.getenv(
  "PUBLISHED_PDF_DIR",
  unset = path.expand("~/Dropbox/works/catalog/coppock_green_2016/original_materials")
)
stopifnot(dir.exists(pdf_dir))

paper_pdf <- file.path(pdf_dir, "coppock_green_2016.pdf")
appendix_pdf <- file.path(pdf_dir, "coppock_green_2016_appendix.pdf")
stopifnot(file.exists(paper_pdf), file.exists(appendix_pdf))

scratch <- file.path(tempdir(), "cg2016_published")
dir.create(scratch, showWarnings = FALSE)

# Reading the pages ------------------------------------------------------------------

# A form feed is glued to the first character of each page's first line, which defeats
# every anchored pattern without changing anything visible, so it comes off here.
layout_lines <- function(pdf, tag) {
  out <- file.path(scratch, paste0(tag, ".txt"))
  system2("pdftotext", c("-layout", shQuote(pdf), shQuote(out)))
  lines <- read_lines(out)
  tibble(page = cumsum(str_detect(lines, "\f")) + 1L,
         line = str_remove_all(lines, "\f"))
}

on_page <- function(layout, pages) {
  layout$line[layout$page %in% pages]
}

# One row per word, with the coordinates pdftotext assigns it. Words are grouped into
# visual rows by their vertical centre, which is what makes a sparse table readable.
bbox_words <- function(pdf, tag) {
  out <- file.path(scratch, paste0(tag, ".xml"))
  system2("pdftotext", c("-bbox-layout", shQuote(pdf), shQuote(out)))
  lines <- read_lines(out)
  page <- cumsum(str_detect(lines, "<page "))
  is_word <- str_detect(lines, "<word ")
  m <- str_match(
    lines[is_word],
    '<word xMin="([0-9.]+)" yMin="([0-9.]+)" xMax="([0-9.]+)" yMax="([0-9.]+)">(.*)</word>'
  )
  tibble(
    page = page[is_word],
    x = (as.numeric(m[, 2]) + as.numeric(m[, 4])) / 2,
    y = (as.numeric(m[, 3]) + as.numeric(m[, 5])) / 2,
    text = m[, 6]
  ) |>
    arrange(page, y, x) |>
    mutate(row = cumsum(c(1, (diff(y) > 3) | (diff(page) != 0))), .by = NULL)
}

paper_layout <- layout_lines(paper_pdf, "paper")
p6  <- on_page(paper_layout, 6)
p7  <- on_page(paper_layout, 7)
p9  <- on_page(paper_layout, 9)
p15 <- on_page(paper_layout, 15)
appendix_layout <- layout_lines(appendix_pdf, "appendix")
ap11 <- on_page(appendix_layout, 11)
ap18 <- on_page(appendix_layout, 18)
ap19 <- on_page(appendix_layout, 19)
ap25 <- on_page(appendix_layout, 25)
paper_words <- bbox_words(paper_pdf, "paper")
appendix_words <- bbox_words(appendix_pdf, "appendix")

# The digits the page prints, with its typography normalised away ----
normalise <- function(x) {
  x |>
    str_replace_all("\u2212", "-") |>
    str_remove_all("[,\\[\\]()]") |>
    str_trim() |>
    str_replace("^(-?)\\.", "\\10")
}

numbers_in <- function(line) {
  line |>
    str_remove_all("(?<=[0-9]),\\s(?=[0-9])") |>
    str_extract_all("[-\u2212]?[0-9]+\\.?[0-9]*%?") |>
    unlist() |>
    normalise()
}

# Dense tables: rows read in order ------------------------------------------------------
# A row that starts in a later column names the column it starts in, so nothing slides.

dense_row <- function(lines, pattern, float, row_label, columns, stat, first_column = 1) {
  line <- str_subset(lines, pattern)
  stopifnot(length(line) == 1)
  values <- numbers_in(str_remove(line, pattern))
  cols <- columns[seq(first_column, length.out = length(values))]
  stopifnot(length(values) == length(cols), !any(is.na(cols)))
  tibble(float = float, row_label = row_label, column_label = cols,
         stat = stat, value_paper = values)
}

# Sparse tables: numbers assigned to the nearest column of a complete row -----------------

table_words <- function(words, page, y_min, y_max) {
  words |> filter(page == !!page, y >= y_min, y <= y_max)
}

row_text <- function(w) {
  w |> summarize(text = paste(text, collapse = " "), y = first(y), .by = row)
}

# The anchors are the x positions of one row that prints every column, taken separately
# for the estimate and for the standard error beneath it in the same cell.
anchors_from <- function(w, anchor_row) {
  vals <- w |>
    filter(row == anchor_row, str_detect(text, "^[-\u2212(]?[0-9.]")) |>
    arrange(x)
  list(
    est = vals |> filter(!str_detect(text, "^\\(")) |> pull(x),
    se  = vals |> filter(str_detect(text, "^\\(")) |> pull(x)
  )
}

sparse_rows <- function(w, columns, anchors, float, skip_rows = integer(),
                        carry_label = FALSE) {
  # A row label can itself end in a number, as "Florida 2005" does, so a word counts as
  # a value only if it also sits to the right of the row-label column.
  label_cut <- min(c(anchors$est, anchors$se)) - 20
  w <- w |> mutate(is_value = str_detect(text, "^[-\u2212(]?[0-9.]") & x >= label_cut)
  out <- list()
  last_label <- ""
  for (r in setdiff(unique(w$row), skip_rows)) {
    rw <- w |> filter(row == r)
    label <- rw |>
      filter(!is_value) |>
      arrange(x) |>
      pull(text) |>
      paste(collapse = " ")
    if (str_detect(label, "^(TABLE|Notes)")) next
    if (!nzchar(label)) {
      # Table 7 puts the standard errors on an unlabelled line beneath the estimates.
      if (!carry_label || !nzchar(last_label)) next
      label <- last_label
    } else {
      last_label <- label
    }
    vals <- rw |> filter(is_value)
    if (nrow(vals) == 0) next
    for (i in seq_len(nrow(vals))) {
      is_se <- str_detect(vals$text[i], "^\\(")
      centres <- if (is_se) anchors$se else anchors$est
      j <- which.min(abs(centres - vals$x[i]))
      out[[length(out) + 1]] <- tibble(
        float = float, row_label = label, column_label = columns[j],
        stat = if (is_se) "se" else "est",
        value_paper = normalise(vals$text[i])
      )
    }
  }
  res <- list_rbind(out)
  stopifnot(!any(duplicated(res[c("row_label", "column_label", "stat")])))
  res
}

# Table 1 --------------------------------------------------------------------------------
t1_cols <- c("Aug. 2006", "Nov. 2006", "Jan. 2008", "Aug. 2008", "Nov. 2008",
             "Aug. 2010", "Nov. 2010", "Feb. 2012", "Aug. 2012", "Nov. 2012")

table_1 <- bind_rows(
  dense_row(p6, "^Civic Duty ", "table_1", "Civic Duty", t1_cols, "est"),
  dense_row(p6, "^n = 36, 903 ", "table_1", "Civic Duty", t1_cols, "se"),
  dense_row(p6, "^Hawthorne ", "table_1", "Hawthorne", t1_cols, "est"),
  dense_row(p6, "^n = 37, 005 ", "table_1", "Hawthorne", t1_cols, "se"),
  dense_row(p6, "^Self ", "table_1", "Self", t1_cols, "est"),
  dense_row(p6, "^n = 37, 011 ", "table_1", "Self", t1_cols, "se"),
  dense_row(p6, "^Neighbors ", "table_1", "Neighbors", t1_cols, "est"),
  dense_row(p6, "^n = 36, 893 ", "table_1", "Neighbors", t1_cols, "se"),
  dense_row(p6, "^All Instruments ", "table_1", "All Instruments", t1_cols,
            "est", first_column = 2),
  dense_row(p6, "^(?= +\\(0\\.021\\))", "table_1", "All Instruments", t1_cols,
            "se", first_column = 2),
  dense_row(p6, "^Control  ", "table_1", "Control", t1_cols, "est"),
  dense_row(p6, "^n = 184, 749 ", "table_1", "Control", t1_cols, "se"),
  tibble(float = "table_1",
         row_label = c("Civic Duty", "Hawthorne", "Self", "Neighbors", "Control"),
         column_label = "n", stat = "n",
         value_paper = c("36903", "37005", "37011", "36893", "184749"))
)

t1_compliers <- str_subset(p6, "^Untreated Compliers +\\[0\\.845")
stopifnot(length(t1_compliers) == 1)
t1_bracket <- str_match_all(t1_compliers, "\\[([-\u2212]?[0-9.]+), ([-\u2212]?[0-9.]+)\\]")[[1]]
stopifnot(nrow(t1_bracket) == 9)

table_1 <- bind_rows(
  table_1,
  tibble(float = "table_1", row_label = "Untreated Compliers",
         column_label = rep(t1_cols[2:10], each = 2),
         stat = rep(c("lo", "hi"), 9),
         value_paper = normalise(as.vector(t(t1_bracket[, 2:3]))))
)

# Table 2 --------------------------------------------------------------------------------
t2_cols <- c("Nov. 2007", "Jan. 2008", "Aug. 2008", "Nov. 2008", "Aug. 2010",
             "Nov. 2010", "Feb. 2012", "Aug. 2012", "Nov. 2012")

table_2 <- bind_rows(
  dense_row(p7, "^Civic Duty  ", "table_2", "Civic Duty", t2_cols, "est"),
  dense_row(p7, "^n = 6, 815 ", "table_2", "Civic Duty", t2_cols, "se"),
  dense_row(p7, "^Shown 2005 Vote ", "table_2", "Shown 2005 Vote", t2_cols, "est"),
  dense_row(p7, "^n = 13, 592 ", "table_2", "Shown 2005 Vote", t2_cols, "se"),
  dense_row(p7, "^Shown 2006 Vote ", "table_2", "Shown 2006 Vote", t2_cols, "est"),
  dense_row(p7, "^n = 13, 546 ", "table_2", "Shown 2006 Vote", t2_cols, "se"),
  dense_row(p7, "^All Instruments  ", "table_2", "All Instruments", t2_cols,
            "est", first_column = 2),
  dense_row(p7, "^(?= +\\(0\\.067\\))", "table_2", "All Instruments", t2_cols,
            "se", first_column = 2),
  dense_row(p7, "^Control   ", "table_2", "Control", t2_cols, "est"),
  dense_row(p7, "^n = 759, 964 ", "table_2", "Control", t2_cols, "se"),
  tibble(float = "table_2",
         row_label = c("Civic Duty", "Shown 2005 Vote", "Shown 2006 Vote", "Control"),
         column_label = "n", stat = "n",
         value_paper = c("6815", "13592", "13546", "759964"))
)

t2_compliers <- str_subset(p7, "^Untreated Compliers +\\[0\\.191")
stopifnot(length(t2_compliers) == 1)
t2_bracket <- str_match_all(t2_compliers, "\\[([-\u2212]?[0-9.]+), ([-\u2212]?[0-9.]+)\\]")[[1]]
stopifnot(nrow(t2_bracket) == 8)

table_2 <- bind_rows(
  table_2,
  tibble(float = "table_2", row_label = "Untreated Compliers",
         column_label = rep(t2_cols[2:9], each = 2),
         stat = rep(c("lo", "hi"), 8),
         value_paper = normalise(as.vector(t(t2_bracket[, 2:3]))))
)

# Table 3 --------------------------------------------------------------------------------
t3_cols <- c("Apr. 2009", "Feb. 2010", "Nov. 2010", "Feb. 2011", "Apr. 2011",
             "Mar. 2012", "Nov. 2012")

t3_compliers <- str_subset(p9, "^Untreated Compliers ")
stopifnot(length(t3_compliers) == 3)

table_3 <- bind_rows(
  dense_row(p9, "^Household Size = 1 ", "table_3", "Household Size = 1",
            t3_cols, "est"),
  dense_row(p9, "^n = 16,638 ", "table_3", "Household Size = 1", t3_cols, "se"),
  dense_row(p9, "^Household Size = 2 ", "table_3", "Household Size = 2",
            t3_cols, "est"),
  dense_row(p9, "^n = 16,915 ", "table_3", "Household Size = 2", t3_cols, "se"),
  dense_row(p9, "^Household Size = 3 ", "table_3", "Household Size = 3",
            t3_cols, "est"),
  dense_row(p9, "^n = 5,086 ", "table_3", "Household Size = 3", t3_cols, "se"),
  dense_row(p9, "^Pooled Estimate ", "table_3", "Pooled Estimate", t3_cols, "est"),
  dense_row(p9, "^n = 38,639 ", "table_3", "Pooled Estimate", t3_cols, "se"),
  imap(t3_compliers, function(line, i) {
    values <- numbers_in(str_remove(line, "^Untreated Compliers "))
    stopifnot(length(values) == 6)
    tibble(float = "table_3", row_label = paste0("Untreated Compliers HH", i),
           column_label = t3_cols[2:7], stat = "est", value_paper = values)
  }) |> list_rbind(),
  tibble(float = "table_3",
         row_label = c("Household Size = 1", "Household Size = 2", "Household Size = 3",
                       "Pooled Estimate"),
         column_label = "n", stat = "n",
         value_paper = c("16638", "16915", "5086", "38639"))
)

# Tables 4, 5 and 6 -----------------------------------------------------------------------
# Each is two panels of state rows with a Meta-Analysis row that prints every column.

sparse_panel <- function(words, page, y_min, y_max, columns, float) {
  w <- table_words(words, page, y_min, y_max)
  anchor <- w |> row_text() |> filter(str_detect(text, "^Meta.?Analysis")) |> pull(row)
  stopifnot(length(anchor) == 1)
  anchors <- anchors_from(w, anchor)
  stopifnot(length(anchors$est) == length(columns),
            length(anchors$se) == length(columns))
  sparse_rows(w, columns, anchors, float)
}

table_4 <- bind_rows(
  sparse_panel(paper_words, 11, 100, 390,
               c("1992-94", "1996-98", "2000-02", "2004-06", "2008-10"),
               "table_4") |>
    mutate(panel = "Presidential on Midterm"),
  sparse_panel(paper_words, 11, 395, 700,
               c("1994-96", "1998-2000", "2002-04", "2006-08", "2010-12"),
               "table_4") |>
    mutate(panel = "Midterm on Presidential")
)

table_5 <- bind_rows(
  sparse_panel(paper_words, 12, 100, 390,
               c("1992-96", "1996-2000", "2000-04", "2004-08", "2008-12"),
               "table_5") |>
    mutate(panel = "Presidential on Presidential"),
  sparse_panel(paper_words, 12, 395, 700,
               c("1994-98", "1998-2002", "2002-06", "2006-10"),
               "table_5") |>
    mutate(panel = "Midterm on Midterm")
)

table_6 <- bind_rows(
  sparse_panel(paper_words, 14, 100, 310,
               c("1992-2012", "1994-2012", "1996-2012", "1998-2012", "2000-12"),
               "table_6") |>
    mutate(panel = "upper"),
  sparse_panel(paper_words, 14, 312, 545,
               c("2002-12", "2004-12", "2006-12", "2008-12", "2010-12"),
               "table_6") |>
    mutate(panel = "lower")
)

# Table 7 -----------------------------------------------------------------------------------
# The Constant row prints all five columns, so it anchors the coefficient rows; N and
# R2 print all five as well and are read straight off their own lines.

t7_cols <- paste0("(", 1:5, ")")
t7_w <- table_words(paper_words, 15, 100, 312)
t7_anchor <- t7_w |> row_text() |> filter(str_detect(text, "^Constant")) |> pull(row)
stopifnot(length(t7_anchor) == 1)
# Table 7 prints the standard error on its own line rather than beside the estimate, so
# the two sets of anchors come from the Constant row and the line beneath it.
t7_se_row <- min(t7_w$row[t7_w$row > t7_anchor])
t7_anchors <- list(
  est = anchors_from(t7_w, t7_anchor)$est,
  se  = anchors_from(t7_w, t7_se_row)$se
)
stopifnot(length(t7_anchors$est) == 5, length(t7_anchors$se) == 5)

t7_skip <- t7_w |>
  row_text() |>
  filter(str_detect(text, "^(N |R2|R 2|State fixed|Dependent|\\(1\\)|TABLE|Notes)")) |>
  pull(row)

table_7 <- sparse_rows(t7_w, t7_cols, t7_anchors, "table_7", skip_rows = t7_skip,
                       carry_label = TRUE) |>
  bind_rows(
    dense_row(p15, "^N(?= +384)", "table_7", "N", t7_cols, "est"),
    dense_row(p15, "^R2(?= +0\\.0851)", "table_7", "R2", t7_cols, "est"),
    tibble(float = "table_7", row_label = "State fixed effects",
           column_label = t7_cols, stat = "est",
           value_paper = c("No", "No", "No", "Yes", "Yes"))
  )

maintext <- bind_rows(table_1, table_2, table_3, table_4, table_5, table_6, table_7) |>
  select(float, panel = any_of("panel"), row_label, column_label, stat, value_paper) |>
  mutate(panel = if_else(is.na(panel) | panel == "", "main", panel))

# Appendix Table A2 ---------------------------------------------------------------------
a2_cols <- c("Net Migrants", "Nc 2008", "CACE 08-12", "Bias Estimate", "Corrected CACE")
a2_lines <- str_subset(ap11, "^ +[A-Z]{2} +-?[0-9]+ +[0-9]+ +[0-9.]+ ")
stopifnot(length(a2_lines) == 15)

table_a2 <- imap(a2_lines, function(line, i) {
  parts <- str_split_1(str_trim(line), " +")
  stopifnot(length(parts) == 6)
  tibble(float = "table_a2", panel = "main", row_label = parts[1], column_label = a2_cols,
         stat = "est", value_paper = normalise(parts[-1]))
}) |> list_rbind()

# Appendix Tables A6 and A7 ---------------------------------------------------------------
a67_cols <- paste(c(90, 180, 270, 365, 455, 545, 635, 730), "Days")
a67_rows <- c("Difference-in-Means", "First-order Polynomial",
              "Second-order Polynomial", "Third-order Polynomial")

# Each page holds two panels, marked by their own headings; every one of the four
# functional-form rows appears once per panel with its standard errors on the next line.
robustness_panel <- function(lines, float) {
  starts <- which(str_detect(lines, "No additional controls|Controls for lagged vote totals"))
  panels <- str_trim(lines[starts])
  stopifnot(length(starts) == 2)
  bounds <- c(starts, length(lines) + 1L)

  imap(panels, function(panel, p) {
    seg <- lines[bounds[p]:(bounds[p + 1] - 1)]
    imap(a67_rows, function(row_label, i) {
      hit <- which(str_detect(seg, paste0("^ *", row_label, " ")))
      stopifnot(length(hit) == 1)
      est <- numbers_in(str_remove(seg[hit], paste0("^ *", row_label, " ")))
      se <- numbers_in(seg[hit + 1])
      stopifnot(length(est) == 8, length(se) == 8)
      tibble(float = float, panel = panel, row_label = row_label,
             column_label = rep(a67_cols, 2),
             stat = rep(c("est", "se"), each = 8),
             value_paper = c(est, se))
    }) |> list_rbind()
  }) |> list_rbind()
}

table_a6 <- bind_rows(
  robustness_panel(ap18, "table_a6")
)
table_a7 <- bind_rows(
  robustness_panel(ap19, "table_a7")
)

# Appendix Table A9 -------------------------------------------------------------------------
a9_cols <- c("Nov 2008", "Aug 2010", "Nov 2010", "Feb 2012", "Aug 2012", "Nov 2012")

# Each row prints its estimates on one line and its standard errors on the next.
a9_pair <- function(pattern, label) {
  hit <- which(str_detect(ap25, pattern))
  stopifnot(length(hit) == 1)
  est <- numbers_in(str_remove(ap25[hit], "^ *[A-Za-z+ ]+"))
  se <- numbers_in(ap25[hit + 1])
  stopifnot(length(est) == 6, length(se) == 6)
  tibble(float = "table_a9", panel = "main", row_label = label,
         column_label = rep(a9_cols, 2), stat = rep(c("est", "se"), each = 6),
         value_paper = c(est, se))
}

table_a9 <- bind_rows(
  a9_pair("^ *Shown Vote \\+ Recontact +[-\u2212 0-9]", "Shown Vote + Recontact"),
  a9_pair("^ *Shown Vote +[-\u2212 0-9]", "Shown Vote")
)

appendix <- bind_rows(table_a2, table_a6, table_a7, table_a9)

stopifnot(
  !any(duplicated(maintext[c("float", "panel", "row_label", "column_label", "stat")])),
  !any(duplicated(appendix[c("float", "panel", "row_label", "column_label", "stat")])),
  !any(is.na(maintext$value_paper)), !any(is.na(appendix$value_paper))
)

write_csv(maintext, here::here("ground_truth", "published_maintext_tables.csv"))
write_csv(appendix, here::here("ground_truth", "published_appendix_values.csv"))

print(maintext |> count(float, panel), n = Inf)
print(appendix |> count(float, panel), n = Inf)
