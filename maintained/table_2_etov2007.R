# coppock_green_2016/maintained/table_2_etov2007.R
# ETOV 2007 downstream experiment: Table 2
# Upstream: og2007 (Nov 2007 general), cluster: hh

source(here::here("maintained", "helpers.R"))
load(here::here("original", "ETOV2007.rdata"))

elections <- c(
  "JAN2008", "AUG2008", "NOV2008",
  "AUG2010", "NOV2010", "FEB2012", "AUG2012", "NOV2012"
)

# Pivot long on downstream elections ----
etov2007_long <- ETOV2007 |>
  pivot_longer(all_of(elections), names_to = "election", values_to = "voted_down") |>
  mutate(election = factor(election, levels = elections))

# Treatment arm specs ----
arm_specs <- tibble(
  arm   = c("treat1", "treat2", "treat3"),
  label = c("Civic Duty", "Shown 05 Vote", "Shown 06 Vote")
)

# IV per arm × election (upstream = og2007, cluster = hh) ----
arm_results <- pmap_dfr(arm_specs, function(arm, label) {
  etov2007_long |>
    filter(treat0 == 1 | .data[[arm]] == 1) |>
    nest_by(election, .keep = TRUE) |>
    mutate(
      fit  = list(iv_robust(
        as.formula(paste("voted_down ~ og2007 |", arm)),
        clusters = hh, se_type = "stata", data = data
      )),
      cace = coef(fit)[["og2007"]],
      se   = fit$std.error[["og2007"]]
    ) |>
    select(election, cace, se) |>
    mutate(arm = arm, label = label)
})

# First stage per arm ----
firststage <- pmap_dfr(arm_specs, function(arm, label) {
  dat <- ETOV2007 |> filter(treat0 == 1 | .data[[arm]] == 1)
  fit <- lm_robust(
    as.formula(paste("og2007 ~", arm)),
    data = dat, clusters = hh, se_type = "stata"
  )
  tibble(
    arm      = arm,
    label    = label,
    election = factor("NOV2007", levels = c("NOV2007", elections)),
    cace     = coef(fit)[[arm]],
    se       = fit$std.error[[arm]]
  )
})

# All-instruments (over-identified) ----
overid_results <- etov2007_long |>
  nest_by(election, .keep = TRUE) |>
  mutate(
    fit  = list(iv_robust(
      voted_down ~ og2007 | treat1 + treat2 + treat3,
      clusters = hh, se_type = "stata", data = data
    )),
    cace  = coef(fit)[["og2007"]],
    se    = fit$std.error[["og2007"]],
    arm   = "overid",
    label = "All Together"
  ) |>
  select(arm, label, election, cace, se)

# Complier control means ----
complier_ctl <- pmap_dfr(arm_specs, function(arm, label) {
  map_dfr(elections, function(e) {
    sub <- ETOV2007 |> filter(treat0 == 1 | .data[[arm]] == 1)
    tibble(
      arm      = arm,
      election = e,
      ctl_mean = complier_control_mean(sub[[e]], sub$og2007, sub[[arm]])
    )
  })
}) |>
  group_by(election) |>
  summarise(ctl_lo = min(ctl_mean), ctl_hi = max(ctl_mean))

# Output ----
results <- bind_rows(
  firststage     |> mutate(type = "first_stage"),
  arm_results    |> mutate(type = "iv"),
  overid_results |> mutate(type = "overid")
)

write_csv(results,      here::here("maintained", "output", "table_2_etov2007.csv"))
write_csv(complier_ctl, here::here("maintained", "output", "table_2_etov2007_complier_ctl.csv"))

# Key verification: All Together Jan 2008 and Aug 2008 ----
overid_results |>
  filter(election %in% c("JAN2008", "AUG2008")) |>
  mutate(check = "all together downstream", across(c(cace, se), \(x) round(x, 3))) |>
  print()

tibble(
  check = "control turnout, mean voted upstream in the control arm",
  value = round(mean(ETOV2007$og2007[ETOV2007$treat0 == 1]), 3)
) |>
  print()
