# coppock_green_2016/maintained/table_3_smg2009.R
# SMG 2009 Australian social pressure experiment: Table 3
# Multilevel design: hhsize = 1, 2, 3; cluster = clusterhh_id
# Pivot long on downstream elections, then nest_by(hhsize, election)

source(here::here("maintained", "helpers.R"))
load(here::here("original", "SMG2009.RData"))

elections <- c(
  "aec_voted_2010p", "aec_voted_2010g", "aec_voted_2011cp",
  "aec_voted_2011ce", "aec_voted_2012p", "aec_voted_2012g"
)
election_labels <- c("Feb 2010", "Nov 2010", "Feb 2011", "Apr 2011", "Mar 2012", "Nov 2012")

# Restrict to treated/control (exclude control-in-treatment) ----
smg_clean <- SMG |> filter(control_intreat == 0)

# IV formula: hhsize 2 includes corechosen, hhsize 1 and 3 do not ----
iv_smg_fit <- function(dat_sub, hhsz) {
  fmla <- if (hhsz == 2) {
    voted_down ~ voted + as.factor(assignlevel) + corechosen |
      Tind + as.factor(assignlevel) + corechosen
  } else {
    voted_down ~ voted + as.factor(assignlevel) | Tind + as.factor(assignlevel)
  }
  iv_robust(fmla, clusters = clusterhh_id, se_type = "stata", data = dat_sub)
}

# Pivot long and run IV per hhsize × election ----
smg_long <- smg_clean |>
  pivot_longer(all_of(elections), names_to = "election", values_to = "voted_down") |>
  mutate(election = factor(election, levels = elections))

hh_results <- map_dfr(1:3, function(hhsz) {
  smg_long |>
    filter(hhsize == hhsz) |>
    nest_by(election, .keep = TRUE) |>
    mutate(
      fit  = list(iv_smg_fit(data, hhsz)),
      cace = coef(fit)[["voted"]],
      se   = fit$std.error[["voted"]]
    ) |>
    select(election, cace, se) |>
    mutate(hhsize = hhsz)
})

# First stage per hhsize ----
fs_smg_fit <- function(dat_sub, hhsz) {
  fmla <- if (hhsz == 2) {
    voted ~ Tind + corechosen + as.factor(assignlevel)
  } else {
    voted ~ Tind + as.factor(assignlevel)
  }
  lm_robust(fmla, clusters = clusterhh_id, se_type = "stata", data = dat_sub)
}

firststage <- map_dfr(1:3, function(hhsz) {
  fit <- fs_smg_fit(smg_clean |> filter(hhsize == hhsz), hhsz)
  tibble(
    hhsize   = hhsz,
    election = factor("APR2009", levels = c("APR2009", elections)),
    cace     = coef(fit)[["Tind"]],
    se       = fit$std.error[["Tind"]]
  )
})

# Meta-analysis across hhsize for each election (FE and RE) ----
meta_results <- hh_results |>
  group_by(election) |>
  summarise(
    m      = list(run_meta(cace, se)),
    fe_cace = m[[1]]$fe_est,
    fe_se   = m[[1]]$fe_se,
    re_cace = m[[1]]$re_est,
    re_se   = m[[1]]$re_se,
    hhsize  = 0L,
    .groups = "drop"
  ) |>
  select(hhsize, election, fe_cace, fe_se, re_cace, re_se)

# Complier control means ----
complier_ctl <- map_dfr(1:3, function(hhsz) {
  sub <- if (hhsz == 2) {
    smg_clean |> filter(hhsize == hhsz, corechosen == 0)
  } else {
    smg_clean |> filter(hhsize == hhsz)
  }
  map_dfr(elections, function(e) {
    tibble(
      hhsize   = hhsz,
      election = e,
      ctl_mean = complier_control_mean(sub[[e]], sub$voted, sub$Tind)
    )
  })
})

# Pooled (meta-analytic) first stage across hhsize ----
pooled_fs <- run_meta(firststage$cace, firststage$se)
tibble(
  check  = "pooled first stage",
  fe_est = round(pooled_fs$fe_est, 3),
  fe_se  = round(pooled_fs$fe_se, 3),
  re_est = round(pooled_fs$re_est, 3),
  re_se  = round(pooled_fs$re_se, 3)
) |>
  print()

# Output ----
results <- bind_rows(
  firststage   |> mutate(type = "first_stage"),
  hh_results   |> mutate(type = "iv"),
  meta_results |> mutate(cace = fe_cace, se = fe_se, type = "meta_fe") |>
    bind_rows(meta_results |> mutate(cace = re_cace, se = re_se, type = "meta_re"))
)

write_csv(results,     here::here("maintained", "output", "table_3_smg2009.csv"))
write_csv(complier_ctl, here::here("maintained", "output", "table_3_smg2009_complier_ctl.csv"))

# Key verification: pooled first-stage and meta-analytic downstream ----
firststage |>
  mutate(check = "first stage by household size", across(c(cace, se), \(x) round(x, 3))) |>
  print()

meta_results |>
  filter(election %in% c("aec_voted_2010p", "aec_voted_2010g", "aec_voted_2011ce")) |>
  mutate(check = "meta-analytic downstream, Feb 2010 / Nov 2010 / Apr 2011",
         across(c(fe_cace, fe_se, re_cace, re_se), \(x) round(x, 3))) |>
  print()
