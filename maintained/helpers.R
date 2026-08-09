# coppock_green_2016/maintained/helpers.R
# Shared helpers for the coppock_green_2016 maintained rewrite

library(here)
library(tidyverse)
library(estimatr)
library(metafor)

here::i_am("maintained/helpers.R")

# Meta-analysis via metafor: returns both fixed-effects and random-effects estimates ----
# FE: inverse-variance pooling (method = "FE"), matching original rmeta::meta.summaries
# RE: REML estimation of between-study variance (method = "REML")
# Returns list with fe_est, fe_se, re_est, re_se (all NA if < 2 valid studies)
run_meta <- function(caces, ses) {
  keep <- !is.na(caces) & !is.na(ses) & ses > 0
  na_out <- list(fe_est = NA_real_, fe_se = NA_real_,
                 re_est = NA_real_, re_se = NA_real_)
  if (sum(keep) < 2) return(na_out)
  d <- caces[keep]; v <- ses[keep]

  fe <- tryCatch(
    rma(yi = d, sei = v, method = "FE"),
    error = function(e) NULL
  )
  re <- tryCatch(
    suppressWarnings(rma(yi = d, sei = v, method = "REML")),
    error = function(e) NULL
  )

  list(
    fe_est = if (is.null(fe)) NA_real_ else as.numeric(coef(fe)),
    fe_se  = if (is.null(fe)) NA_real_ else as.numeric(fe$se),
    re_est = if (is.null(re)) NA_real_ else as.numeric(coef(re)),
    re_se  = if (is.null(re)) NA_real_ else as.numeric(re$se)
  )
}

# Complier control mean ----
complier_control_mean <- function(Y, D, Z) {
  pr_c  <- mean(D[Z == 1]) - mean(D[Z == 0])
  pr_at <- mean(D[Z == 0])
  pr_nt <- 1 - pr_c - pr_at
  Y0_at <- mean(Y[D == 1 & Z == 0])
  Y0_nt <- mean(Y[D == 0 & Z == 1])
  Y0    <- mean(Y[Z == 0])
  (Y0 - pr_at * Y0_at - pr_nt * Y0_nt) / pr_c
}

# Bias from interstate migration ----
bias_finder <- function(net_migrants, cace = 0.2, N_c = 500) {
  itt    <- cace * N_c
  ittd   <- N_c - net_migrants
  biased <- itt / ittd
  biased - cace
}

# Run a single RD 2SLS (het-robust HC3 SEs, matching original vcovHC default) ----
# Returns list(cace, se) or NULL if too little data or model fails.
run_rd_iv <- function(dat, upstream, downstream, bandwidth,
                      order = 1, lag = TRUE, state_abbrev = NULL) {
  if (!is.null(state_abbrev)) dat <- dat[dat$state %in% state_abbrev, ]

  days           <- dat[[paste0("days_",   upstream)]]
  treat          <- dat[[paste0("treat_",  upstream)]]
  voted_up       <- dat[[paste0("voted",   upstream)]]
  voted_down     <- dat[[paste0("voted",   downstream)]]
  voted_down_lag <- dat[[paste0("voted",   downstream, "_lag")]]

  if (sum(voted_up,   na.rm = TRUE) < 500 ||
      sum(voted_down, na.rm = TRUE) < 500) return(NULL)

  df <- tibble(days, treat, voted_up, voted_down, voted_down_lag) |>
    filter(abs(days) < bandwidth)

  if (order >= 2) df$days_2 <- df$days^2
  if (order >= 3) df$days_3 <- df$days^3

  key <- paste0("o", order, "_", lag)
  fmla_list <- list(
    "o0_FALSE" = voted_down ~ voted_up | treat,
    "o1_FALSE" = voted_down ~ voted_up + days + voted_up:days |
                   treat + days + treat:days,
    "o2_FALSE" = voted_down ~ voted_up + days + voted_up:days + days_2 + voted_up:days_2 |
                   treat + days + treat:days + days_2 + treat:days_2,
    "o3_FALSE" = voted_down ~ voted_up + days + voted_up:days + days_2 + voted_up:days_2 +
                   days_3 + voted_up:days_3 |
                   treat + days + treat:days + days_2 + treat:days_2 +
                   days_3 + treat:days_3,
    "o0_TRUE"  = voted_down ~ voted_up + voted_down_lag | treat + voted_down_lag,
    "o1_TRUE"  = voted_down ~ voted_up + days + voted_up:days + voted_down_lag |
                   treat + days + treat:days + voted_down_lag,
    "o2_TRUE"  = voted_down ~ voted_up + days + voted_up:days + days_2 + voted_up:days_2 +
                   voted_down_lag |
                   treat + days + treat:days + days_2 + treat:days_2 + voted_down_lag,
    "o3_TRUE"  = voted_down ~ voted_up + days + voted_up:days + days_2 + voted_up:days_2 +
                   days_3 + voted_up:days_3 + voted_down_lag |
                   treat + days + treat:days + days_2 + treat:days_2 +
                   days_3 + treat:days_3 + voted_down_lag
  )

  fmla <- fmla_list[[key]]
  fit  <- tryCatch(
    iv_robust(fmla, data = df, se_type = "HC3"),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  list(cace = coef(fit)[["voted_up"]], se = fit$std.error[["voted_up"]])
}

# Blank a figure PDF's embedded timestamps ----
# R's pdf() device stamps /CreationDate and /ModDate with the wall clock, so an
# otherwise deterministic pipeline writes a different file on every run. The epoch
# string is the same width as what it replaces, which keeps the cross-reference byte
# offsets valid, and a file with no timestamp is left alone.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("D:[0-9]{14}", raw_pdf, all = TRUE)
  if (length(hits) == 0) return(invisible(path))
  for (h in hits) raw_pdf[h:(h + length(epoch) - 1L)] <- epoch
  writeBin(raw_pdf, path)
  invisible(path)
}
