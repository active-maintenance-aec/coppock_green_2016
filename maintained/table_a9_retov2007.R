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

# The follow-up experiment itself, which the appendix and footnote 5 describe ----
# A random subset of the subjects in either Self condition of the 2007 experiment was
# sent a refresher mailer before the November 2008 election. The quantity both passages
# report is the direct effect of that mailer on turnout, not a CACE, so it is estimated
# here rather than read off Table A9.
self_subjects <- RETOV2007 |>
  filter(shown_vote_contact == 1 | shown_vote_nocontact == 1)

refresher_fit <- lm_robust(
  NOV2008 ~ shown_vote_contact,
  data = self_subjects, clusters = hh, se_type = "stata"
)

recontact_stronger <- results |>
  select(label, election, cace) |>
  pivot_wider(names_from = label, values_from = cace) |>
  mutate(recontact_stronger = `Shown Vote + Recontact` > `Shown Vote`)

followup <- tibble(
  n_self_subjects   = nrow(self_subjects),
  n_recontacted     = sum(self_subjects$shown_vote_contact == 1),
  direct_effect     = coef(refresher_fit)[["shown_vote_contact"]],
  direct_effect_se  = refresher_fit$std.error[["shown_vote_contact"]],
  n_elections       = nrow(recontact_stronger),
  n_recontact_stronger = sum(recontact_stronger$recontact_stronger)
)

write_csv(followup, here::here("maintained", "output", "text_followup_experiment.csv"))
print(followup)

# Key verification: Shown Vote and Shown Vote + Recontact, Nov 2008 ----
results |>
  filter(election == "NOV2008") |>
  mutate(check = "Table A9, Nov 2008", across(c(cace, se), \(x) round(x, 3))) |>
  select(label, cace, se) |>
  print()
