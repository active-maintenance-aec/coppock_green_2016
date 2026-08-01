# coppock_green_2016/maintained/tables_4_5_rd.R
# RD Tables 4 and 5: meta-analytic CACEs for 19 states × 19 year-window pairs
# Architecture: crossing(state, pair_idx) → pmap → meta_iv per window

source(here::here("maintained", "helpers.R"))
load(here::here("original", "USA.habit.rdata"))

states <- c(
  "AR", "CO", "CT", "IA", "IL", "FL", "FL05",
  "KY", "MI", "MO", "MO05", "MT", "NJ", "NV",
  "NY", "OK", "OR", "PA", "RI"
)

# 19 year-window pairs (same ordering as original) ----
pairs <- tibble(
  years_window = c(
    "92-96", "96-00", "00-04", "04-08", "08-12",   # Pres-on-Pres
    "92-94", "96-98", "00-02", "04-06", "08-10",   # Pres-on-Mid
    "94-96", "98-00", "02-04", "06-08", "10-12",   # Mid-on-Pres
    "94-98", "98-02", "02-06", "06-10"             # Mid-on-Mid
  ),
  upstream_year = c(
    "92", "96", "00", "04", "08",
    "92", "96", "00", "04", "08",
    "94", "98", "02", "06", "10",
    "94", "98", "02", "06"
  ),
  downstream_year = c(
    "96", "00", "04", "08", "12",
    "94", "98", "02", "06", "10",
    "96", "00", "04", "08", "12",
    "98", "02", "06", "10"
  )
)

# All state × pair combinations ----
grid <- crossing(pairs, state = states)

# Run RD IV for every combination (with try wrapper for missing data) ----
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
  select(years_window, upstream_year, downstream_year, state, cace, se)

# Meta-analysis across states (exclude FL05 and MO05 per original) ----
meta_results <- grid_fits |>
  filter(!state %in% c("FL05", "MO05")) |>
  group_by(years_window, upstream_year, downstream_year) |>
  summarise(
    m       = list(run_meta(cace, se)),
    fe_cace = m[[1]]$fe_est,
    fe_se   = m[[1]]$fe_se,
    re_cace = m[[1]]$re_est,
    re_se   = m[[1]]$re_se,
    state   = "meta",
    .groups = "drop"
  ) |>
  select(years_window, upstream_year, downstream_year, state, fe_cace, fe_se, re_cace, re_se)

all_results <- bind_rows(grid_fits, meta_results)

# Output ----
write_csv(all_results,  here::here("maintained", "output", "tables_4_5_rd_all.csv"))
write_csv(meta_results, here::here("maintained", "output", "tables_4_5_rd_meta.csv"))

# Quick inspection: key meta-analytic estimates (FE and RE) ----
meta_results |>
  filter(years_window %in% c("92-94", "08-10", "92-96", "08-12", "06-10")) |>
  mutate(across(c(fe_cace, fe_se, re_cace, re_se), \(x) round(x, 3))) |>
  select(years_window, fe_cace, fe_se, re_cace, re_se) |>
  print()
