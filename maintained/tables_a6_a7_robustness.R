# coppock_green_2016/maintained/tables_a6_a7_robustness.R
# Appendix Tables A6 and A7: robustness of 2008→2012 and 2006→2010 meta estimates
# Triple crossing: bandwidth × polynomial order × lag inclusion

source(here::here("maintained", "helpers.R"))
load(here::here("original", "USA.habit.rdata"))

# States used in robustness check (FL05 and MO05 excluded by construction) ----
states <- c(
  "AR", "CT", "IA", "IL", "FL", "KY", "MO",
  "MT", "NJ", "NV", "NY", "OK", "OR", "PA", "RI"
)

bandwidths <- c(90, 180, 270, 365, 455, 545, 635, 730)
orders     <- 0:3
lags       <- c(TRUE, FALSE)

# Helper: run full robustness grid for one upstream → downstream pair ----
run_robustness_grid <- function(upstream_yr, downstream_yr) {
  specs <- crossing(
    bandwidth = bandwidths,
    order     = orders,
    lag       = lags,
    state     = states
  )

  results <- specs |>
    mutate(
      result = pmap(
        list(bandwidth, order, lag, state),
        \(bw, o, l, s) tryCatch(
          run_rd_iv(USA, upstream_yr, downstream_yr, bw, o, l, s),
          error = function(e) NULL
        )
      ),
      cace = map_dbl(result, ~ if (is.null(.x)) NA_real_ else .x$cace),
      se   = map_dbl(result, ~ if (is.null(.x)) NA_real_ else .x$se)
    ) |>
    select(bandwidth, order, lag, state, cace, se)

  # Meta-analysis (FE and RE) for each bandwidth × order × lag ----
  results |>
    group_by(bandwidth, order, lag) |>
    summarise(
      m       = list(run_meta(cace, se)),
      fe_cace = m[[1]]$fe_est,
      fe_se   = m[[1]]$fe_se,
      re_cace = m[[1]]$re_est,
      re_se   = m[[1]]$re_se,
      .groups = "drop"
    ) |>
    select(bandwidth, order, lag, fe_cace, fe_se, re_cace, re_se)
}

# Run both analyses ----
meta_0812 <- run_robustness_grid("08", "12")

meta_0610 <- run_robustness_grid("06", "10")

# Output ----
write_csv(meta_0812, here::here("maintained", "output", "table_a6_robustness_0812.csv"))
write_csv(meta_0610, here::here("maintained", "output", "table_a7_robustness_0610.csv"))

# Quick inspection: boxed FE and RE estimates (order=1, bandwidth=365, lag=TRUE) ----
meta_0812 |>
  filter(order == 1, bandwidth == 365, lag == TRUE) |>
  mutate(check = "Table A6 boxed estimate, 2008 to 2012, first-order polynomial, 365 days, lagged",
         across(c(fe_cace, fe_se, re_cace, re_se), \(x) round(x, 3))) |>
  print()

meta_0610 |>
  filter(order == 1, bandwidth == 365, lag == TRUE) |>
  mutate(check = "Table A7 boxed estimate, 2006 to 2010, first-order polynomial, 365 days, lagged",
         across(c(fe_cace, fe_se, re_cace, re_se), \(x) round(x, 3))) |>
  print()
