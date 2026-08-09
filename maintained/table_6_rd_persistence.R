# coppock_green_2016/maintained/table_6_rd_persistence.R
# RD Table 6: all upstream elections × 2012 downstream; binom.test

source(here::here("maintained", "helpers.R"))
load(here::here("original", "USA.habit.rdata"))

states <- c(
  "AR", "CO", "CT", "IA", "IL", "FL", "FL05",
  "KY", "MI", "MO", "MO05", "MT", "NJ", "NV",
  "NY", "OK", "OR", "PA", "RI"
)

years <- c("92", "94", "96", "98", "00", "02", "04", "06", "08", "10")

# All upstream × 2012 downstream ----
pairs <- tibble(
  upstream_year   = years,
  downstream_year = "12",
  years_window    = paste0(years, "-12")
)

grid <- crossing(pairs, state = states)

grid_fits <- grid |>
  mutate(
    result = pmap(
      list(upstream_year, state),
      \(u, s) tryCatch(
        run_rd_iv(USA, u, "12", bandwidth = 365, state_abbrev = s),
        error = function(e) NULL
      )
    ),
    cace = map_dbl(result, ~ if (is.null(.x)) NA_real_ else .x$cace),
    se   = map_dbl(result, ~ if (is.null(.x)) NA_real_ else .x$se)
  ) |>
  select(years_window, upstream_year, state, cace, se)

# Meta-analysis across states (exclude FL05, MO05) ----
meta_results <- grid_fits |>
  filter(!state %in% c("FL05", "MO05")) |>
  group_by(years_window, upstream_year) |>
  summarise(
    m       = list(run_meta(cace, se)),
    fe_cace = m[[1]]$fe_est,
    fe_se   = m[[1]]$fe_se,
    re_cace = m[[1]]$re_est,
    re_se   = m[[1]]$re_se,
    state   = "meta",
    .groups = "drop"
  ) |>
  select(years_window, upstream_year, state, fe_cace, fe_se, re_cace, re_se)

# Binomial test: how many of the 86 state-window estimates are positive? ----
# The two historical voter files are excluded, as they are from every meta-analysis
# here. Colorado and Michigan have no 2012 downstream data at all, so they drop out
# through the missing-value filter rather than by name.
positive_count <- grid_fits |>
  filter(
    !state %in% c("FL05", "MO05"),
    !is.na(cace)
  ) |>
  summarise(
    n_positive = sum(cace > 0),
    n_total    = n()
  )

binom_result <- binom.test(
  positive_count$n_positive,
  positive_count$n_total
)

print(binom_result)

# Output ----
all_results <- bind_rows(
  grid_fits    |> mutate(type = "state"),
  meta_results |> mutate(type = "meta")
)

write_csv(all_results, here::here("maintained", "output", "table_6_rd_persistence.csv"))

# The nonparametric test the text reports, written out rather than only printed ----
binom_out <- tibble(
  n_positive = positive_count$n_positive,
  n_negative = positive_count$n_total - positive_count$n_positive,
  n_total    = positive_count$n_total,
  p_value    = binom_result$p.value,
  n_negative_significant = sum(
    with(grid_fits, !is.na(cace) & !state %in% c("FL05", "MO05") &
           cace < 0 & abs(cace / se) > qnorm(0.975))
  )
)

write_csv(binom_out, here::here("maintained", "output", "table_6_binomial_test.csv"))

# Quick inspection (FE and RE) ----
meta_results |>
  filter(years_window %in% c("10-12", "08-12")) |>
  mutate(across(c(fe_cace, fe_se, re_cace, re_se), \(x) round(x, 3))) |>
  select(years_window, fe_cace, fe_se, re_cace, re_se) |>
  print()
