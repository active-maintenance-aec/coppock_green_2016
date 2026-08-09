# coppock_green_2016/maintained/table_1_etov2006.R
# ETOV 2006 downstream experiment: Table 1
# Architecture: pmap over arm specs; within each arm, pivot_longer + nest_by(election) for IV

source(here::here("maintained", "helpers.R"))
load(here::here("original", "ETOV2006.rdata"))

elections <- c(
  "NOV2006", "JAN2008", "AUG2008", "NOV2008",
  "AUG2010", "NOV2010", "FEB2012", "AUG2012", "NOV2012"
)

# Pivot downstream elections to long format (used in each arm) ----
etov2006_long <- ETOV2006 |>
  pivot_longer(all_of(elections), names_to = "election", values_to = "voted_down") |>
  mutate(election = factor(election, levels = elections))

# Treatment arm specs ----
arm_specs <- tibble(
  arm   = c("treat1", "treat2", "treat3", "treat4"),
  label = c("Hawthorne", "Civic Duty", "Neighbors", "Self")
)

# IV per arm × election ----
arm_results <- pmap_dfr(arm_specs, function(arm, label) {
  etov2006_long |>
    filter(treat0 == 1 | .data[[arm]] == 1) |>
    nest_by(election, .keep = TRUE) |>
    mutate(
      fit  = list(iv_robust(
        as.formula(paste("voted_down ~ AUG2006 |", arm)),
        clusters = household, se_type = "stata", data = data
      )),
      cace = coef(fit)[["AUG2006"]],
      se   = fit$std.error[["AUG2006"]]
    ) |>
    select(election, cace, se) |>
    mutate(arm = arm, label = label)
})

# First stage per arm (Aug 2006 upstream) ----
firststage <- pmap_dfr(arm_specs, function(arm, label) {
  dat <- ETOV2006 |> filter(treat0 == 1 | .data[[arm]] == 1)
  fit <- lm_robust(
    as.formula(paste("AUG2006 ~", arm)),
    data = dat, clusters = household, se_type = "stata"
  )
  tibble(
    arm      = arm,
    label    = label,
    election = factor("AUG2006", levels = c("AUG2006", elections)),
    cace     = coef(fit)[[arm]],
    se       = fit$std.error[[arm]]
  )
})

# All-instruments (over-identified) on full sample ----
overid_results <- etov2006_long |>
  nest_by(election, .keep = TRUE) |>
  mutate(
    fit  = list(iv_robust(
      voted_down ~ AUG2006 | treat1 + treat2 + treat3 + treat4,
      clusters = household, se_type = "stata", data = data
    )),
    cace  = coef(fit)[["AUG2006"]],
    se    = fit$std.error[["AUG2006"]],
    arm   = "overid",
    label = "All Instruments"
  ) |>
  select(arm, label, election, cace, se)

# Control arm: turnout in the upstream election, and the subject counts ----
# Table 1 prints an n under every row label and a Control row in the First Stage
# column. Neither is in the deposit's own table script, so both are written here.
control_fit <- lm_robust(
  AUG2006 ~ 1,
  data = ETOV2006 |> filter(treat0 == 1), clusters = household, se_type = "stata"
)

control_row <- tibble(
  arm      = "treat0",
  label    = "Control",
  election = factor("AUG2006", levels = c("AUG2006", elections)),
  cace     = coef(control_fit)[["(Intercept)"]],
  se       = control_fit$std.error[["(Intercept)"]],
  type     = "control"
)

arm_n <- bind_rows(
  arm_specs |> mutate(n = map_int(arm, \(a) sum(ETOV2006[[a]] == 1))),
  tibble(arm = "treat0", label = "Control", n = sum(ETOV2006$treat0 == 1))
)

# Complier control means (min/max across arms per election) ----
complier_ctl <- pmap_dfr(arm_specs, function(arm, label) {
  map_dfr(elections, function(e) {
    sub <- ETOV2006 |> filter(treat0 == 1 | .data[[arm]] == 1)
    tibble(
      arm      = arm,
      election = e,
      ctl_mean = complier_control_mean(sub[[e]], sub$AUG2006, sub[[arm]])
    )
  })
}) |>
  group_by(election) |>
  summarise(ctl_lo = min(ctl_mean), ctl_hi = max(ctl_mean))

# Output ----
results <- bind_rows(
  firststage     |> mutate(type = "first_stage"),
  arm_results    |> mutate(type = "iv"),
  overid_results |> mutate(type = "overid"),
  control_row
) |>
  left_join(arm_n |> select(arm, n), by = "arm")

write_csv(results,      here::here("maintained", "output", "table_1_etov2006.csv"))
write_csv(complier_ctl, here::here("maintained", "output", "table_1_etov2006_complier_ctl.csv"))

# Key verification: Self first-stage and All Instruments downstream ----
firststage |>
  filter(arm == "treat4") |>
  mutate(check = "first stage, Self arm", across(c(cace, se), \(x) round(x, 3))) |>
  print()

overid_results |>
  filter(election %in% c("NOV2006", "JAN2008")) |>
  mutate(check = "all instruments downstream", across(c(cace, se), \(x) round(x, 3))) |>
  print()
