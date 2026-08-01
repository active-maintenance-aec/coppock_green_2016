# coppock_green_2016/maintained/table_a2_figure_a1_movers.R
# Table A2 and Figure A1: measurement error from interstate migration
# Replaces: reshape2::dcast, melt, %>% pipes, system("say...")

source(here::here("maintained", "helpers.R"))
load(here::here("original", "USA.habit.rdata"))

# Complier counts per state (from RD voter file data) ----
n_compliers <- USA |>
  group_by(state) |>
  summarise(
    N_c_08 = sum(voted08[days_08 < 0 & days_08 > -365], na.rm = TRUE),
    N_c_06 = sum(voted06[days_06 < 0 & days_06 > -365], na.rm = TRUE),
    N_c_04 = sum(voted04[days_04 < 0 & days_04 > -365], na.rm = TRUE),
    N_c_02 = sum(voted02[days_02 < 0 & days_02 > -365], na.rm = TRUE)
  )

# Interstate movers data ----
interstate <- read_table(
  here::here("original", "interstatemovers.txt"),
  show_col_types = FALSE
)

# State migration matrix (21–23 year-olds, as in original) ----
state_matrix_long <- interstate |>
  filter(ca_age >= 21, ca_age < 24) |>
  group_by(state08, state12) |>
  summarise(corrected_count = sum(corrected_count), .groups = "drop")

state_matrix_wide <- state_matrix_long |>
  pivot_wider(
    names_from  = state12,
    values_from = corrected_count,
    values_fill = 0
  ) |>
  arrange(state08)

state_codes <- state_matrix_wide$state08
mat <- as.matrix(state_matrix_wide[, -1])
rownames(mat) <- colnames(mat) <- state_codes

# Compute migration flows per state ----
state_df <- tibble(state = state_codes) |>
  mutate(
    totals_08             = rowSums(mat),
    totals_12             = colSums(mat)[state],
    residentially_stable  = diag(mat)[state],
    out_migrants          = totals_08 - residentially_stable,
    in_migrants           = totals_12 - residentially_stable,
    net_migrants          = in_migrants - out_migrants,
    net_migrants_100      = net_migrants * 100,
    pr_residentially_stable = residentially_stable / totals_12,
    pr_in_migrant         = in_migrants / totals_12,
    pr_out_migrant        = out_migrants / totals_12
  )

# Hard-coded CACE estimates from RD analysis (2008→2012, same as original) ----
cace_ests <- tibble(
  state = c("AR", "CT", "IA", "IL", "FL", "KY", "MO", "MT",
            "NJ", "NV", "NY", "OK", "OR", "PA", "RI"),
  cace  = c(0.200, 0.161, 0.0861, 0.080, 0.105, 0.075, 0.155, 0.111,
            0.155, 0.174, 0.068, 0.138, 0.108, 0.121, 0.113)
)

analysis_states <- cace_ests$state

# Merge and compute bias estimates ----
migration_flows <- state_df |>
  filter(state %in% analysis_states) |>
  left_join(n_compliers |> select(state, N_c_08), by = "state") |>
  left_join(cace_ests, by = "state") |>
  mutate(
    bias_estimate     = bias_finder(net_migrants_100, cace, N_c_08),
    corrected_estimate = cace - bias_estimate
  ) |>
  select(state, net_migrants_100, N_c_08, cace, bias_estimate, corrected_estimate) |>
  arrange(net_migrants_100)

# Output Table A2 ----
write_csv(migration_flows, here::here("maintained", "output", "table_a2_migration.csv"))
print(migration_flows)

# Figure A1: bias as a function of net migration × N compliers ----
net_migration_seq <- seq(-1000, 1000, length.out = 100)
n_compliers_seq   <- seq(2000, 100000, length.out = 5000)

gg_df <- crossing(
  net_migration = net_migration_seq,
  N_compliers   = n_compliers_seq
) |>
  mutate(bias = bias_finder(net_migration, cace = 0.117, N_c = N_compliers))

g_a1 <- ggplot(gg_df, aes(net_migration, bias, group = N_compliers, colour = N_compliers)) +
  geom_line(alpha = 0.5) +
  ylim(-0.05, 0.15) +
  scale_colour_gradient2(
    low      = "red",
    mid      = "white",
    high     = "blue",
    midpoint = 25000,
    breaks   = seq(0, 100000, by = 10000),
    labels   = scales::comma(seq(0, 100000, by = 10000)),
    guide    = guide_colorbar(
      reverse   = TRUE,
      barheight = 25,
      nbin      = 100,
      ticks     = FALSE,
      title     = "Number of Compliers"
    )
  ) +
  labs(x = "Net Migration", y = "Bias") +
  theme_bw()

ggsave(
  here::here("maintained", "output", "figure_a1_bias.pdf"),
  g_a1, width = 8, height = 6
)
ggsave(
  here::here("maintained", "output", "figure_a1_bias.png"),
  g_a1, width = 8, height = 6, dpi = 150
)


