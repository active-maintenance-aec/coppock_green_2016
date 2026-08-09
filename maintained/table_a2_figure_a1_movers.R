# coppock_green_2016/maintained/table_a2_figure_a1_movers.R
# Table A2 and Figure A1: measurement error from interstate migration
# Replaces: reshape2::dcast, melt, %>% pipes, system("say...")
#
# Runs after tables_4_5_rd.R, whose 2008 on 2012 estimates are the CACE column of
# Table A2. The deposit types those fifteen numbers in as constants; here they are
# read back from the pipeline, so the table cannot carry a value the rest of the
# analysis does not produce.

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

# State migration matrix (21 to 23 year-olds, as in original) ----
# The rows and the columns of this matrix have to carry the same states in the same
# order, because the diagonal is the residentially stable count and the column sums
# are the 2012 totals. pivot_wider orders new columns by first appearance unless it
# is told otherwise, so names_sort is load-bearing rather than cosmetic and the
# assertion below is what proves the two margins line up.
state_matrix_long <- interstate |>
  filter(ca_age >= 21, ca_age < 24) |>
  group_by(state08, state12) |>
  summarise(corrected_count = sum(corrected_count), .groups = "drop")

state_matrix_wide <- state_matrix_long |>
  pivot_wider(
    names_from  = state12,
    values_from = corrected_count,
    values_fill = 0,
    names_sort  = TRUE
  ) |>
  arrange(state08)

state_codes <- state_matrix_wide$state08
mat <- as.matrix(state_matrix_wide[, -1])
rownames(mat) <- state_codes
stopifnot(identical(colnames(mat), state_codes), nrow(mat) == ncol(mat))

# Compute migration flows per state ----
state_df <- tibble(state = state_codes) |>
  mutate(
    totals_08             = rowSums(mat),
    totals_12             = colSums(mat),
    residentially_stable  = diag(mat),
    out_migrants          = totals_08 - residentially_stable,
    in_migrants           = totals_12 - residentially_stable,
    net_migrants          = in_migrants - out_migrants,
    net_migrants_100      = net_migrants * 100,
    pr_residentially_stable = residentially_stable / totals_12,
    pr_in_migrant         = in_migrants / totals_12,
    pr_out_migrant        = out_migrants / totals_12
  )

# The 2008 on 2012 CACE per state, read back from the RD pipeline ----
# Every state with an estimate at the 365-day window, excluding the two historical
# voter files, which is the fifteen states Table A2 lists.
cace_ests <- read_csv(
  here::here("maintained", "output", "tables_4_5_rd_all.csv"),
  show_col_types = FALSE
) |>
  filter(years_window == "08-12", !state %in% c("meta", "FL05", "MO05"), !is.na(cace)) |>
  # Table A2 prints the CACE at three decimals and its Corrected CACE column is that
  # printed figure less the printed bias, so the bias formula takes the estimate at the
  # precision the table states rather than at full precision.
  transmute(state, cace = round(cace, 3))

analysis_states <- cace_ests$state

# Merge and compute bias estimates ----
migration_flows <- state_df |>
  filter(state %in% analysis_states) |>
  left_join(n_compliers |> select(state, N_c_08), by = "state") |>
  left_join(cace_ests, by = "state") |>
  mutate(
    bias_estimate      = bias_finder(net_migrants_100, cace, N_c_08),
    corrected_estimate = cace - bias_estimate
  ) |>
  select(state, net_migrants_100, N_c_08, cace, bias_estimate, corrected_estimate) |>
  arrange(net_migrants_100)

stopifnot(nrow(migration_flows) == length(analysis_states),
          !any(is.na(migration_flows$cace)))

# Output Table A2 ----
write_csv(migration_flows, here::here("maintained", "output", "table_a2_migration.csv"))
print(migration_flows)

# Figure A1: bias as a function of net migration x N compliers ----
# The true CACE the illustration assumes is the 2008 on 2012 meta-analytic estimate,
# read back from the pipeline at the three decimals the appendix states it to rather
# than typed in as a constant.
cace_true <- round(
  read_csv(here::here("maintained", "output", "tables_4_5_rd_meta.csv"),
           show_col_types = FALSE) |>
    filter(years_window == "08-12") |>
    pull(fe_cace),
  3
)
stopifnot(length(cace_true) == 1)

net_migration_seq <- seq(-1000, 1000, length.out = 100)
n_compliers_seq   <- seq(2000, 100000, length.out = 5000)

gg_df <- crossing(
  net_migration = net_migration_seq,
  N_compliers   = n_compliers_seq
) |>
  mutate(bias = bias_finder(net_migration, cace = cace_true, N_c = N_compliers))

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

# The surface the figure draws, at the complier counts its own legend names ----
# The drawn surface is 5,000 lines over the 100-point net-migration grid, half a
# million points, which is far too many to commit and none of which the page labels.
# What the page does label is ten complier counts, so the committed file is the same
# surface evaluated at those ten, which is what a reader can actually read off it.
figure_a1_data <- crossing(
  net_migration = net_migration_seq,
  N_compliers   = seq(10000, 100000, by = 10000)
) |>
  mutate(cace_true = cace_true,
         bias = bias_finder(net_migration, cace_true, N_compliers))

write_csv(figure_a1_data, here::here("maintained", "output", "figure_a1_bias.csv"))
