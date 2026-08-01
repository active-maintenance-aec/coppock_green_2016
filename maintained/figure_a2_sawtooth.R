# coppock_green_2016/maintained/figure_a2_sawtooth.R
# Appendix Figure A2: sawtooth plot of CACEs across all upstream × downstream pairs
# including primary elections
# Replaces: plyr::adply, system("say ..."), three nested for loops

source(here::here("maintained", "helpers.R"))
load(here::here("original", "USA.habit.rdata"))

states <- c(
  "AR", "CO", "CT", "IA", "IL", "FL", "FL05",
  "KY", "MI", "MO", "MO05", "MT", "NJ", "NV",
  "NY", "OK", "OR", "PA", "RI"
)

# Election labels: alternating primary and general ----
year_labels <- c(
  "presprimary92", "92", "primary94",   "94",
  "presprimary96", "96", "primary98",   "98",
  "presprimary00", "00", "primary02",   "02",
  "presprimary04", "04", "primary06",   "06",
  "presprimary08", "08", "primary10",   "10",
  "presprimary12", "12"
)

days_cols   <- paste0("days_",   year_labels)
treat_cols  <- paste0("treat_",  year_labels)
voted_cols  <- c(
  "pres_pri_voted92", "voted92", "pri_voted94",   "voted94",
  "pres_pri_voted96", "voted96", "pri_voted98",   "voted98",
  "pres_pri_voted00", "voted00", "pri_voted02",   "voted02",
  "pres_pri_voted04", "voted04", "pri_voted06",   "voted06",
  "pres_pri_voted08", "voted08", "pri_voted10",   "voted10",
  "pres_pri_voted12", "voted12"
)
lag_cols <- paste0(voted_cols, "_lag")

# Build grid of all upstream × downstream × state combinations ----
# Only downstream > upstream (triangular)
n_years  <- length(year_labels)
up_idx   <- rep(seq_len(n_years - 1), (n_years - 1):1)
down_idx <- unlist(map(seq_len(n_years - 1), \(i) (i + 1):n_years))

pairs <- tibble(
  up_label   = year_labels[up_idx],
  down_label = year_labels[down_idx],
  up_days    = days_cols[up_idx],
  up_treat   = treat_cols[up_idx],
  up_voted   = voted_cols[up_idx],
  down_voted = voted_cols[down_idx],
  down_lag   = lag_cols[down_idx]
)

grid <- crossing(pairs, state = states)

# run_rd_iv_primary: like run_rd_iv but with explicit column name args ----
run_rd_primary <- function(dat, up_days, up_treat, up_voted,
                           down_voted, down_lag, bandwidth,
                           state_abbrev = NULL) {
  if (!is.null(state_abbrev)) dat <- dat[dat$state %in% state_abbrev, ]

  days           <- dat[[up_days]]
  treat          <- dat[[up_treat]]
  voted_up       <- dat[[up_voted]]
  voted_down     <- dat[[down_voted]]
  voted_down_lag <- dat[[down_lag]]

  if (sum(voted_up,   na.rm = TRUE) < 500 ||
      sum(voted_down, na.rm = TRUE) < 500) return(NULL)

  df <- tibble(days, treat, voted_up, voted_down, voted_down_lag) |>
    filter(abs(days) < bandwidth)

  fit <- tryCatch(
    iv_robust(
      voted_down ~ voted_up + days + voted_up:days + voted_down_lag |
        treat + days + treat:days + voted_down_lag,
      data = df, se_type = "HC3"
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  list(cace = coef(fit)[["voted_up"]], se = fit$std.error[["voted_up"]])
}

# Run all combinations (bandwidth = 365) ----
grid_fits <- grid |>
  mutate(
    result = pmap(
      list(up_days, up_treat, up_voted, down_voted, down_lag, state),
      \(ud, ut, uv, dv, dl, s) tryCatch(
        run_rd_primary(USA, ud, ut, uv, dv, dl, 365, s),
        error = function(e) NULL
      )
    ),
    cace = map_dbl(result, ~ if (is.null(.x)) NA_real_ else .x$cace),
    se   = map_dbl(result, ~ if (is.null(.x)) NA_real_ else .x$se)
  ) |>
  select(up_label, down_label, state, cace, se)

# Build plotting data frame ----
plot_dat <- grid_fits |>
  filter(!is.na(cace)) |>
  mutate(
    up_primary   = if_else(str_detect(up_label, "primary"), "primary", "general"),
    down_primary = if_else(str_detect(down_label, "primary"), "primary", "general"),
    upyear_char  = str_remove_all(up_label, "(primary)|(presprimary)"),
    downyear_char = str_remove_all(down_label, "(primary)|(presprimary)"),
    upyear   = as.integer(upyear_char),
    downyear = as.integer(downyear_char),
    upyear   = if_else(upyear   > 50, upyear   + 1900L, upyear   + 2000L),
    downyear = if_else(downyear > 50, downyear + 1900L, downyear + 2000L),
    downyear = if_else(down_primary == "primary", downyear - 0.5, as.double(downyear)),
    weights  = 1 / se^2
  )

# Figure A2: sawtooth of CACEs (general-election upstream, excluding 2012 and FL05/MO05) ----
gg_df <- plot_dat |>
  filter(
    up_primary == "general",
    upyear     != 2012,
    !state %in%  c("FL05", "MO05")
  )

g <- ggplot(gg_df, aes(downyear, cace, colour = down_primary, group = upyear)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1992, 2012, by = 2)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  facet_grid(upyear ~ .) +
  theme_bw() +
  theme(
    legend.position   = c(0.13, 0.045),
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.direction  = "horizontal"
  ) +
  guides(colour = guide_legend(title = "", reverse = TRUE)) +
  labs(y = "Estimated CACEs", x = "Downstream Election Year")

ggsave(
  here::here("maintained", "output", "figure_a2_sawtooth.pdf"),
  g, width = 10, height = 7
)
ggsave(
  here::here("maintained", "output", "figure_a2_sawtooth.png"),
  g, width = 10, height = 7, dpi = 150
)

write_csv(plot_dat, here::here("maintained", "output", "figure_a2_data.csv"))


