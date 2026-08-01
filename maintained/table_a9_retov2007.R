# coppock_green_2016/maintained/table_a9_retov2007.R
# RETOV 2007 follow-up: Appendix Table A9
# Two arms: shown_vote_nocontact (Shown Vote) and shown_vote_contact (Shown Vote + Recontact)
# Upstream: og2007, cluster: hh

source(here::here("maintained", "helpers.R"))
load(here::here("original", "RETOV2007.rdata"))

elections <- c("NOV2008", "AUG2010", "NOV2010", "FEB2012", "AUG2012", "NOV2012")

# Arm specs: instrument and the column used to exclude its complement ----
arm_specs <- tibble(
  arm        = c("norecontact",         "recontact"),
  label      = c("Shown Vote",          "Shown Vote + Recontact"),
  instrument = c("shown_vote_nocontact", "shown_vote_contact"),
  excl_col   = c("shown_vote_contact",   "shown_vote_nocontact")
)

# Pivot long and run IV per arm × election ----
retov_base <- RETOV2007 |> filter(treat1 == 0)

results <- pmap_dfr(arm_specs, function(arm, label, instrument, excl_col) {
  retov_base |>
    filter(.data[[excl_col]] != 1) |>
    pivot_longer(all_of(elections), names_to = "election", values_to = "voted_down") |>
    mutate(election = factor(election, levels = elections)) |>
    nest_by(election, .keep = TRUE) |>
    mutate(
      fit  = list(iv_robust(
        as.formula(paste("voted_down ~ og2007 |", instrument)),
        clusters = hh, se_type = "stata", data = data
      )),
      cace = coef(fit)[["og2007"]],
      se   = fit$std.error[["og2007"]]
    ) |>
    select(election, cace, se) |>
    mutate(arm = arm, label = label)
})

write_csv(results, here::here("maintained", "output", "table_a9_retov2007.csv"))

# Key verification: Shown Vote and Shown Vote + Recontact, Nov 2008 ----
results |>
  filter(election == "NOV2008") |>
  mutate(check = "Table A9, Nov 2008", across(c(cace, se), \(x) round(x, 3))) |>
  select(label, cace, se) |>
  print()
