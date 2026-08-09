# coppock_green_2016/maintained/table_7_modeling_caces.R
# Table 7: Weighted meta-regression of all state × pair CACEs
# All upstream × all downstream pairs (triangular grid), then merge covariates

source(here::here("maintained", "helpers.R"))
load(here::here("original", "USA.habit.rdata"))
load(here::here("original", "turnoutrates.RData"))
load(here::here("original", "votemargins.RData"))
library(modelsummary)

states <- c(
  "AR", "CO", "CT", "IA", "IL", "FL", "FL05",
  "KY", "MI", "MO", "MO05", "MT", "NJ", "NV",
  "NY", "OK", "OR", "PA", "RI"
)

years <- c("92", "94", "96", "98", "00", "02", "04", "06", "08", "10", "12")

# Build full triangular grid of upstream × downstream pairs ----
# (all pairs where downstream > upstream; exclude last two per original: remove 10-12 handled
#  separately; original removes downstream_year indices 56-57 from the list)
all_pairs <- crossing(
  upstream_year   = years,
  downstream_year = years
) |>
  filter(
    match(downstream_year, years) > match(upstream_year, years)
  ) |>
  # The deposit builds the same 55 pairs by a loop whose last iteration indexes
  # years[12:11], appending an NA and a duplicate "12"; its downstream_year[-c(56, 57)]
  # drops exactly those two. The triangular filter above is that loop's intent.
  mutate(years_window = paste0(upstream_year, "-", downstream_year))

# Run RD IV for every state × pair ----
grid <- crossing(all_pairs, state = states)

grid_fits <- grid |>
  mutate(
    result = pmap(
      list(upstream_year, downstream_year, state),
      \(u, d, s) tryCatch(
        run_rd_iv(USA, u, d, bandwidth = 365, state_abbrev = s),
        error = function(e) NULL
      )
    ),
    cace = map_dbl(result, ~ if (is.null(.x)) NA_real_ else .x$cace),
    se   = map_dbl(result, ~ if (is.null(.x)) NA_real_ else .x$se)
  ) |>
  select(upstream_year, downstream_year, years_window, state, cace, se) |>
  filter(!is.na(cace))

# Build metadata frame matching original structure ----
metadata <- grid_fits |>
  mutate(
    up_num    = as.integer(upstream_year),
    down_num  = as.integer(downstream_year),
    upYear    = if_else(up_num < 50, up_num + 2000L, up_num + 1900L),
    downYear  = if_else(down_num < 50, down_num + 2000L, down_num + 1900L),
    year      = downYear - mean(downYear),
    timedistance   = downYear - upYear,
    weights        = 1 / se^2,
    upstream_type  = if_else(upstream_year %in%
                               c("92", "96", "00", "04", "08", "12"), "Pres", "Mid"),
    downstream_type = if_else(downstream_year %in%
                                c("92", "96", "00", "04", "08", "12"), "Pres", "Mid"),
    voterfile_year = if_else(state %in% c("FL05", "MO05"), 2005L, 2013L),
    state_clean    = if_else(state == "FL05", "FL",
                      if_else(state == "MO05", "MO", state)),
    state_upyear   = paste0(state_clean, "_", upYear),
    state_downyear = paste0(state_clean, "_", downYear)
  ) |>
  filter(voterfile_year == 2013)

# Merge covariates ----
metadata <- metadata |>
  left_join(turnoutrates, by = "state_upyear")  |>
  left_join(votemargins,  by = "state_downyear")

# Weighted meta-regression models (Table 7) ----
fit_1 <- lm(cace ~ timedistance + turnoutrate1829,
             data = metadata, weights = weights)
fit_2 <- lm(cace ~ timedistance + turnoutrate1829 + battleground_P,
             data = metadata, weights = weights)
fit_3 <- lm(cace ~ timedistance + turnoutrate1829 + battleground_PM,
             data = metadata, weights = weights)
fit_4 <- lm(cace ~ timedistance + turnoutrate1829 + state_clean,
             data = metadata, weights = weights)
fit_5 <- lm(cace ~ timedistance + turnoutrate1829 + state_clean +
              upstream_type + downstream_type,
             data = metadata, weights = weights)

# Robust SEs ----
robust_vcov <- map(
  list(fit_1, fit_2, fit_3, fit_4, fit_5),
  \(f) sandwich::vcovHC(f)
)
robust_ses <- map(robust_vcov, \(v) sqrt(diag(v)))

# Every cell of Table 7, unrounded ----
# The typeset table rounds to four decimals; a ground truth reading a value back out
# of it would round twice. The state dummies are absorbed in the printed table and
# are kept here, flagged, so the file is the fit rather than the display.
fits <- list(fit_1, fit_2, fit_3, fit_4, fit_5)

table_7_coefficients <- imap(fits, function(f, i) {
  tibble(
    model     = i,
    term      = names(coef(f)),
    estimate  = as.numeric(coef(f)),
    std_error = as.numeric(robust_ses[[i]][names(coef(f))]),
    state_fe  = str_starts(names(coef(f)), "state_clean")
  )
}) |>
  list_rbind()

table_7_gof <- imap(fits, function(f, i) {
  tibble(model = i, n = nobs(f), r_squared = summary(f)$r.squared,
         has_state_fe = any(str_starts(names(coef(f)), "state_clean")))
}) |>
  list_rbind()

write_csv(table_7_coefficients,
          here::here("maintained", "output", "table_7_coefficients.csv"))
write_csv(table_7_gof, here::here("maintained", "output", "table_7_gof.csv"))

# The precision-weighted average across every state-election pair ----
# The Discussion quotes it, and the deposit computes it in this script.
write_csv(
  tibble(
    n_pairs   = nrow(metadata),
    mean_cace = weighted.mean(metadata$cace, metadata$weights),
    # downstream_margin is missing exactly where a midterm year had neither a
    # gubernatorial nor a senate race; downstream_margin_b is the filled version the
    # battleground indicators are built from.
    n_no_gubernatorial_or_senate = sum(is.na(metadata$downstream_margin)),
    n_battleground_p  = sum(metadata$battleground_P),
    n_battleground_pm = sum(metadata$battleground_PM)
  ),
  here::here("maintained", "output", "text_average_cace.csv")
)

# Key coefficients ----
tibble(
  check     = "Table 7, column 5",
  term      = c("timedistance", "turnoutrate1829", "r_squared"),
  estimate  = c(round(coef(fit_5)[["timedistance"]], 4),
                round(coef(fit_5)[["turnoutrate1829"]], 4),
                round(summary(fit_5)$r.squared, 4)),
  std_error = c(round(robust_ses[[5]][["timedistance"]], 4),
                round(robust_ses[[5]][["turnoutrate1829"]], 4),
                NA_real_)
) |>
  print()

# Display table ----
# modelsummary replaces stargazer, which has not been updated since 2022 and
# required sink() to reach a file. Robust SEs come from the same vcovHC list
# used above, so the table and the printed check cannot disagree.
coef_labels <- c(
  "timedistance"        = "Years between upstream and downstream",
  "turnoutrate1829"     = "Youth turnout in upstream election",
  "battleground_P"      = "Presidential battleground",
  "battleground_PM"     = "Presidential or midterm battleground",
  "upstream_typePres"   = "Presidential upstream",
  "downstream_typePres" = "Presidential downstream",
  "(Intercept)"         = "Constant"
)

# The state dummies are absorbed rather than shown, so the table says which
# models carry them. Seven coefficients occupy fourteen body rows, so the
# indicator sits at row fifteen: last coefficient above it, goodness of fit below.
state_fe_row <- tibble(
  term = "State fixed effects",
  `(1)` = "No", `(2)` = "No", `(3)` = "No", `(4)` = "Yes", `(5)` = "Yes"
)
attr(state_fe_row, "position") <- 2 * length(coef_labels) + 1

# siunitx wrapping would put every cell in \\num{}, which needs the package at
# compile time for no gain here.
options(modelsummary_format_numeric_latex = "plain")

modelsummary(
  list(fit_1, fit_2, fit_3, fit_4, fit_5),
  vcov      = robust_vcov,
  coef_map  = coef_labels,
  gof_map   = tibble(
    raw   = c("nobs", "r.squared"),
    clean = c("N", "R2"),
    fmt   = c(0, 4)
  ),
  add_rows  = state_fe_row,
  stars     = FALSE,
  fmt       = 4,
  title     = "Modeling downstream CACEs",
  notes     = c(
    "Youth are defined as 18 to 29 year-olds.",
    "Battleground status: two-party vote share difference less than 10 points.",
    "All models weighted by the inverse of the squared standard error of the CACE estimate.",
    "Robust standard errors in parentheses."
  ),
  output    = here::here("maintained", "output", "table_7_modeling_caces.tex")
)

write_csv(metadata, here::here("maintained", "output", "table_7_metadata.csv"))
