# coppock_green_2016/maintained/table_7_modeling_caces.R
# Table 7: Weighted meta-regression of all state × pair CACEs
# All upstream × all downstream pairs (triangular grid), then merge covariates

source(here::here("maintained", "helpers.R"))
load(here::here("original", "USA.habit.rdata"))
load(here::here("original", "turnoutrates.RData"))
load(here::here("original", "votemargins.RData"))
library(stargazer)

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
  # Original script removes downstream_year at positions 56-57 of the original loop
  # which corresponds to the pair (10, 12) and (12, ?). Checking: the original generates
  # all i < j pairs in the triangular grid, giving 11*10/2 = 55 pairs, then removes indices
  # 56,57 which don't exist in a 55-element vector... so effectively no removal beyond bounds.
  # The original downstream_year vector has 55 elements and removes [56,57] which is a no-op.
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
robust_ses <- map(
  list(fit_1, fit_2, fit_3, fit_4, fit_5),
  \(f) sqrt(diag(sandwich::vcovHC(f)))
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

# Stargazer output ----
sink(here::here("maintained", "output", "table_7_modeling_caces.tex"))
stargazer(
  fit_1, fit_2, fit_3, fit_4, fit_5,
  se          = robust_ses,
  style       = "apsr",
  column.sep.width = "0pt",
  star.cutoffs = c(NA, NA, NA),
  digits      = 4,
  omit        = "state_clean",
  omit.labels = "State F.E.",
  omit.stat   = c("adj.rsq", "f", "ser"),
  dep.var.labels = "Dependent Variable: CACE Estimate in State-Election Pair",
  covariate.labels = c(
    "Years between upstream and downstream",
    "Youth turnout in upstream election",
    "Presidential battleground",
    "Presidential or midterm battleground",
    "Presidential upstream",
    "Presidential downstream",
    "Constant"
  ),
  title  = "Modeling Downstream CACEs",
  align  = TRUE,
  notes  = c(
    "Youth are defined as 18-29 year-olds.",
    "Battleground status: two-party vote share difference less than 10 points.",
    "All models weighted by inverse of squared SE of CACE estimate.",
    "Robust standard errors in parentheses."
  ),
  notes.append = FALSE,
  font.size = "small",
  label   = "tab:modelingcaces",
  float   = FALSE
)
sink()

write_csv(metadata, here::here("maintained", "output", "table_7_metadata.csv"))
