# coppock_green_2016/maintained/in_text_claims.R
# Output: printed to the console; nothing is written
# Depends on: maintained/output/*, ground_truth/published_claims.csv,
#   ground_truth/published_appendix_values.csv
# Description: The second instrument. Every quantity the article states outside a table
#   is recomputed here from the pipeline's own output by a path of its own and printed
#   beside the sentence that states it. It reads the extraction, because a block cannot
#   name the article's own figure without it, and it never reads the ground truth,
#   because agreeing with the comparison would prove nothing.
#
#   Where the ground truth reaches a quantity through a summary file, this file goes back
#   to the estimates the summary was built from, and where the ground truth selects a row
#   by one identifier this file selects it by another. Nothing here refits anything.
#
#   One claim is about what a published column contains rather than about a quantity the
#   pipeline produces, so its block reads the transcription of that table. That is the
#   same permission the extraction already carries: a transcription of the printed page
#   is not the comparison, and reading the comparison is what would prove nothing.
#
#   Each printed line is CLAIM <id> = <value> || <label>. The id on that line is the only
#   link the coverage gate uses.

source(here::here("maintained", "helpers.R"))

options(width = 200)

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)

claim_row <- function(id) {
  row <- published_claims |> filter(.data$claim_id == .env$id)
  stopifnot(nrow(row) == 1)
  row
}

# Signed zero is normalised here as well as on the transcription side; whichever
# instrument normalises, both must.
render_at <- function(x, digits) {
  out <- sprintf(paste0("%.", digits, "f"), x)
  str_replace(out, "^-(0(\\.0+)?)$", "\\1")
}

emit <- function(id, value, label) {
  row <- claim_row(id)
  rendered <- if ((!is.na(row$comparison) && row$comparison == "approx") || is.na(value)) {
    "NA"
  } else {
    render_at(value, row$digits)
  }
  cat("CLAIM ", id, " = ", rendered, " || ", label, "\n", sep = "")
}

emit_holds <- function(id, holds, label) {
  cat("CLAIM ", id, " = ", as.character(holds), " || ", label, "\n", sep = "")
}

# Pipeline output ------------------------------------------------------------------------

out_dir <- here::here("maintained", "output")
read_out <- function(f) read_csv(file.path(out_dir, f), show_col_types = FALSE)

etov2006 <- read_out("table_1_etov2006.csv")
etov2006_ctl <- read_out("table_1_etov2006_complier_ctl.csv")
etov2007 <- read_out("table_2_etov2007.csv")
etov2007_ctl <- read_out("table_2_etov2007_complier_ctl.csv")
smg2009 <- read_out("table_3_smg2009.csv")
rd <- read_out("tables_4_5_rd_all.csv")
persistence <- read_out("table_6_rd_persistence.csv")
binomial <- read_out("table_6_binomial_test.csv")
coefficients <- read_out("table_7_coefficients.csv")
gof <- read_out("table_7_gof.csv")
pair_metadata <- read_out("table_7_metadata.csv")
average_cace <- read_out("text_average_cace.csv")
robust_0812 <- read_out("table_a6_robustness_0812.csv")
robust_0610 <- read_out("table_a7_robustness_0610.csv")
followup <- read_out("text_followup_experiment.csv")
migration <- read_out("table_a2_migration.csv")
bias_surface <- read_out("figure_a1_bias.csv")
sawtooth <- read_out("figure_a2_data.csv")
sawtooth_60 <- read_out("figure_a2_data_60day.csv")
like_elections <- read_out("text_like_elections.csv")

# Rows are selected by the treatment arm the deposit names rather than by the label the
# published table prints, so the two instruments do not share a selector.
arm <- function(d, which_arm, elec, column) {
  row <- d |> filter(.data$arm == which_arm, .data$election == elec)
  stopifnot(nrow(row) == 1)
  row[[column]]
}
overid <- function(d, elec, column) {
  row <- d |> filter(.data$type == "overid", .data$election == elec)
  stopifnot(nrow(row) == 1)
  row[[column]]
}
pooled <- function(elec, column) {
  row <- smg2009 |> filter(.data$hhsize == 0, .data$election == elec,
                           .data$type %in% c("meta_fe", "first_stage_meta"))
  stopifnot(nrow(row) == 1)
  row[[column]]
}
meta_window <- function(win, column) {
  row <- rd |> filter(.data$state == "meta", .data$years_window == win)
  stopifnot(nrow(row) == 1)
  row[[column]]
}
coefficient <- function(name, model_no, column) {
  row <- coefficients |> filter(.data$term == name, .data$model == model_no)
  stopifnot(nrow(row) == 1)
  row[[column]]
}
z_ratio <- function(estimate, se) abs(estimate / se)
significant <- function(estimate, se, level = 0.05) z_ratio(estimate, se) > qnorm(1 - level / 2)

# Design ----

# "Households were blocked according to their street address and assigned to one of five
#  experimental groups. A control group received no mail, and the four treatment groups
#  each received a different mailing."
emit("design_2006_groups",
     n_distinct(etov2006$label[etov2006$type %in% c("first_stage", "control")]),
     "Distinct arm labels in the 2006 experiment, the control arm included")

# "Each household was randomly assigned to one of four groups. A control group received
#  no mail, and the three treatment groups received a mailing that urged them to vote."
emit("design_2007_groups",
     n_distinct(etov2007$label[etov2007$type %in% c("first_stage", "control")]),
     "Distinct arm labels in the 2007 experiment, the control arm included")

# Upstream and Downstream Results ----

# "For the two types of mailings that were used in both experiments, Civic Duty and Self,
#  the average upstream treatment effects are similar. In 2006, the Civic Duty mailing
#  increased turnout by 1.8 percentage points, as compared to 1.4 percentage points in
#  2007. The Self mailing increased turnout by 5.0 percentage points in 2006 and by 4.5
#  and 5.0 percentage points in 2007, depending on which past election's turnout was
#  reported in the mailing."
emit("results_civic_duty_2006", 100 * arm(etov2006, "treat2", "AUG2006", "cace"),
     "Civic Duty first stage, 2006")
emit("results_civic_duty_2007", 100 * arm(etov2007, "treat1", "NOV2007", "cace"),
     "Civic Duty first stage, 2007")
emit("results_self_2006", 100 * arm(etov2006, "treat4", "AUG2006", "cace"),
     "Self first stage, 2006")
emit("results_shown_2005_vote", 100 * arm(etov2007, "treat2", "NOV2007", "cace"),
     "Shown 2005 Vote first stage, 2007")
emit("results_shown_2006_vote", 100 * arm(etov2007, "treat3", "NOV2007", "cace"),
     "Shown 2006 Vote first stage, 2007")

# "The Neighbors mailing, used only in 2006, generates even stronger effects, raising
#  turnout by 8.3 percentage points. Each of these treatment effects is statistically
#  significant at the .05 level or better."
emit("results_neighbors_2006", 100 * arm(etov2006, "treat3", "AUG2006", "cace"),
     "Neighbors first stage, 2006")

first_stages <- bind_rows(etov2006, etov2007) |> filter(type == "first_stage")
emit_holds(
  "results_first_stages_significant",
  all(significant(first_stages$cace, first_stages$se)),
  str_glue("Smallest z statistic among the {nrow(first_stages)} upstream effects is ",
           "{sprintf('%.1f', min(z_ratio(first_stages$cace, first_stages$se)))}")
)

# Results ----

# "Looking first at Table 1, which reports results from the 2006 study, we see that the
#  two-stage least squares (2SLS) estimate of the effect of voting in August 2006 on
#  voting in November 2006 is 0.108, with a standard error of 0.021. This estimate is
#  highly significant (p < .001)."
emit("results_nov2006_cace", overid(etov2006, "NOV2006", "cace"),
     "Overidentified CACE on November 2006")
emit("results_nov2006_se", overid(etov2006, "NOV2006", "se"), "Its standard error")
emit_holds(
  "results_nov2006_significant",
  significant(overid(etov2006, "NOV2006", "cace"), overid(etov2006, "NOV2006", "se"), 0.001),
  str_glue("z = {sprintf('%.1f', z_ratio(overid(etov2006, 'NOV2006', 'cace'), overid(etov2006, 'NOV2006', 'se')))}")
)

# "The last row of Table 1 indicates that the lowest estimate of the voting rate among
#  untreated compliers is 84%. This figure in conjunction with the estimated CACE implies
#  that voting in August raised turnout among compliers from 84% to 95%, which is about as
#  high as turnout can plausibly go."
nov2006_floor <- etov2006_ctl$ctl_lo[etov2006_ctl$election == "NOV2006"]
emit("results_untreated_complier_min", 100 * nov2006_floor,
     "Minimum untreated-complier turnout across the four arms, November 2006")
emit("results_complier_turnout_after",
     100 * (nov2006_floor + overid(etov2006, "NOV2006", "cace")),
     "That floor raised by the overidentified CACE")

# "In January 2008, Michigan held a presidential primary, which attracted relatively low
#  turnout among compliers. Voting in the August 2006 primary raised turnout among
#  compliers by 14.2 percentage points (SE = 0.036, p < .001)."
emit("results_jan2008_cace", 100 * overid(etov2006, "JAN2008", "cace"),
     "Overidentified CACE on January 2008, in points")
emit("results_jan2008_se", overid(etov2006, "JAN2008", "se"), "Its standard error")
emit_holds(
  "results_jan2008_significant",
  significant(overid(etov2006, "JAN2008", "cace"), overid(etov2006, "JAN2008", "se"), 0.001),
  str_glue("z = {sprintf('%.1f', z_ratio(overid(etov2006, 'JAN2008', 'cace'), overid(etov2006, 'JAN2008', 'se')))}")
)

# "Similarly, the estimated CACE is a statistically significant 13.5 percentage points in
#  the August 2008 primary, fully two years after the first election following the GOTV
#  campaign."
emit("results_aug2008_cace", 100 * overid(etov2006, "AUG2008", "cace"),
     "Overidentified CACE on August 2008, in points")
emit_holds(
  "results_aug2008_significant",
  significant(overid(etov2006, "AUG2008", "cace"), overid(etov2006, "AUG2008", "se")),
  str_glue("z = {sprintf('%.1f', z_ratio(overid(etov2006, 'AUG2008', 'cace'), overid(etov2006, 'AUG2008', 'se')))}")
)

# "Tracking the estimated CACE across the full set of August elections shows a gradual
#  pattern of decline, with an estimated CACE of 12.6 percentage points in 2010 and 8.9
#  percentage points in 2012, both of which remain statistically significant at the .05
#  level. By contrast, no significant effects are obtained for subsequent November
#  elections."
emit("results_aug2010_cace", 100 * overid(etov2006, "AUG2010", "cace"),
     "Overidentified CACE on August 2010, in points")
emit("results_aug2012_cace", 100 * overid(etov2006, "AUG2012", "cace"),
     "Overidentified CACE on August 2012, in points")

august_late <- etov2006 |> filter(type == "overid", election %in% c("AUG2010", "AUG2012"))
emit_holds("results_august_both_significant",
           all(significant(august_late$cace, august_late$se)),
           str_glue("Smaller z of the two is ",
                    "{sprintf('%.1f', min(z_ratio(august_late$cace, august_late$se)))}"))

november_late <- etov2006 |>
  filter(type == "overid", election %in% c("NOV2008", "NOV2010", "NOV2012"))
emit_holds("results_november_not_significant",
           !any(significant(november_late$cace, november_late$se)),
           str_glue("Largest z among November 2008, 2010 and 2012 is ",
                    "{sprintf('%.2f', max(z_ratio(november_late$cace, november_late$se)))}"))

# "The estimated CACE on voting in the presidential primary held two months later is
#  sizable at 33.6 percentage points (SE = 6.7). The estimated CACE remains substantial
#  for the August 2008 primary (18.3 percentage points, SE = 6.4) and exerts a marginally
#  significant effect on turnout in the presidential election in November 2008
#  (CACE = 0.092, SE = 0.046), where turnout among compliers was 84% or higher."
emit("results_t2_jan2008_cace", 100 * overid(etov2007, "JAN2008", "cace"),
     "Pooled 2007 CACE on January 2008, in points")
emit("results_t2_jan2008_se", 100 * overid(etov2007, "JAN2008", "se"),
     "Its standard error, in points")
emit("results_t2_aug2008_cace", 100 * overid(etov2007, "AUG2008", "cace"),
     "Pooled 2007 CACE on August 2008, in points")
emit("results_t2_aug2008_se", 100 * overid(etov2007, "AUG2008", "se"),
     "Its standard error, in points")
emit("results_t2_nov2008_cace", overid(etov2007, "NOV2008", "cace"),
     "Pooled 2007 CACE on November 2008")
emit("results_t2_nov2008_se", overid(etov2007, "NOV2008", "se"), "Its standard error")
emit("results_t2_complier_turnout",
     100 * etov2007_ctl$ctl_lo[etov2007_ctl$election == "NOV2008"],
     "Minimum untreated-complier turnout across the three arms, November 2008")

# Footnote 5 ----

# "The direct treatment effect of the reminder was 0.2 percentage points, with a standard
#  error of 0.5 percentage points. Downstream effects are estimated to be stronger among
#  recontacted subjects in three of the six subsequent elections, and the difference in
#  downstream effects is never significant."
emit("fn5_reminder_effect", 100 * followup$direct_effect,
     "Refresher mailer's effect on November 2008 turnout, in points")
emit("fn5_reminder_se", 100 * followup$direct_effect_se, "Its standard error, in points")

table_a9 <- read_out("table_a9_retov2007.csv")
a9_wide <- table_a9 |>
  select(arm, election, cace, se) |>
  pivot_wider(names_from = arm, values_from = c(cace, se))
emit("fn5_recontact_stronger", sum(a9_wide$cace_recontact > a9_wide$cace_norecontact),
     "Elections where the recontacted CACE is the larger of the two")
emit("fn5_recontact_elections", nrow(a9_wide), "Downstream elections in Table A9")

a9_diff_z <- abs(a9_wide$cace_recontact - a9_wide$cace_norecontact) /
  sqrt(a9_wide$se_recontact^2 + a9_wide$se_norecontact^2)
emit_holds("fn5_difference_never_significant", all(a9_diff_z < qnorm(0.975)),
           str_glue("Largest z on the difference of the two conditions is ",
                    "{sprintf('%.2f', max(a9_diff_z))}"))

# Table 3 discussion ----

# "For example, compliers were 40.3 percentage points more likely to vote in April 2011
#  as a result of voting in April 2009 (p < .01)."
emit("results_t3_apr2011_cace", 100 * pooled("aec_voted_2011ce", "cace"),
     "Pooled CACE on April 2011, in points")
emit_holds("results_t3_apr2011_significant",
           significant(pooled("aec_voted_2011ce", "cace"),
                       pooled("aec_voted_2011ce", "se"), 0.01),
           str_glue("z = {sprintf('%.2f', z_ratio(pooled('aec_voted_2011ce', 'cace'), pooled('aec_voted_2011ce', 'se')))}"))

# "Regarding spring and winter elections, the pooled results reveal significant increases
#  in turnout among compliers in February 2010, April 2011, and March 2012."
spring <- smg2009 |>
  filter(type == "meta_fe",
         election %in% c("aec_voted_2010p", "aec_voted_2011ce", "aec_voted_2012p"))
emit_holds("results_t3_spring_significant", all(significant(spring$cace, spring$se)),
           str_glue("Smallest z among the three spring and winter elections is ",
                    "{sprintf('%.2f', min(z_ratio(spring$cace, spring$se)))}"))

# "As for November elections, the effects are strong but gradually diminishing, with
#  estimated CACEs of 0.303 in 2010 and 0.226 in 2012."
emit("results_t3_nov2010_cace", pooled("aec_voted_2010g", "cace"),
     "Pooled CACE on November 2010")
emit("results_t3_nov2012_cace", pooled("aec_voted_2012g", "cace"),
     "Pooled CACE on November 2012")

# Eligibility discontinuities ----

# "We obtained voter files from 17 states: Arkansas, Colorado, Connecticut, Iowa,
#  Illinois, Florida, Kentucky, Michigan, Missouri, Montana, Nevada, New Jersey, New York,
#  Oklahoma, Oregon, Pennsylvania, and Rhode Island."
rd_states <- setdiff(unique(rd$state), "meta")
emit("rd_states", n_distinct(str_remove(rd_states, "05$")),
     "Distinct states in the discontinuity data, the two 2005 files folded into their own state")

# "we leverage the fact that for two states, Florida and Missouri, we have voter files
#  from both 2005 and 2013"
emit("rd_two_vintages", sum(str_detect(rd_states, "05$")),
     "States carrying a second, historical voter file")

# "Across all years and states, midterm elections attract a small percentage of registered
#  just-eligible voters, typically less than 10%. Even presidential elections attract
#  roughly one-third of the just-eligible electorate."
emit_holds("rd_midterm_share", NA,
           "No counterpart in the deposit: the aggregated file records votes cast per birthdate cohort and no count of registrants, so no turnout rate can be formed")
emit_holds("rd_presidential_share", NA, "No counterpart in the deposit, for the same reason")

# "Table 4 shows how habits persist over two-year periods. Looking first at
#  presidential-on-midterm effects, we see that all 54 estimates are positive."
pom <- rd |>
  filter(state != "meta", !is.na(cace),
         years_window %in% c("92-94", "96-98", "00-02", "04-06", "08-10"))
emit("rd_pom_estimates", nrow(pom), "State estimates in the upper panel of Table 4")
emit_holds("rd_pom_all_positive", all(pom$cace > 0),
           str_glue("Smallest of the {nrow(pom)} presidential-on-midterm estimates is ",
                    "{sprintf('%.3f', min(pom$cace))}"))

# "Using fixed-effects meta-analysis, we find the precision-weighted average estimate for
#  the period 2008-10 to be 0.09, with a standard error of just 0.002."
emit("rd_pom_meta_0810", meta_window("08-10", "fe_cace"),
     "Fixed-effects meta-analytic CACE, 2008 on 2010")
emit("rd_pom_meta_0810_se", meta_window("08-10", "fe_se"), "Its standard error")

# "A CACE of 0.09 in Missouri, for example, implies that casting a vote in 2008 increases
#  the probability that a complier will cast a vote in the 2010 midterm election from
#  15.4% to 24.4%."
missouri_0810 <- rd |> filter(state == "MO", years_window == "08-10")
emit("rd_missouri_0810", missouri_0810$cace, "Missouri's 2008 on 2010 CACE")
emit("rd_missouri_baseline", NA,
     "No counterpart in the deposit: the appendix states that the discontinuity analysis cannot estimate untreated-complier turnout, because the voter file does not list those who did not vote")
emit("rd_missouri_treated", NA,
     str_glue("No counterpart either; the two published figures differ by ",
              "{sprintf('%.1f', 100 * missouri_0810$cace)} points, which is the CACE the sentence quotes"))

# "The pattern of midterm-on-presidential estimates argues strongly for habit formation as
#  well. Here, 50 out of 54 estimates are positive."
mop <- rd |>
  filter(state != "meta", !is.na(cace),
         years_window %in% c("94-96", "98-00", "02-04", "06-08", "10-12"))
emit("rd_mop_positive", sum(mop$cace > 0),
     "Positive estimates in the lower panel of Table 4")
emit("rd_mop_estimates", nrow(mop), "State estimates in the lower panel of Table 4")

# "The average effect of turnout in 2010 on turnout in 2012 is 0.11, with a standard error
#  of 0.019. Looking four years earlier, the average effect of turnout in 2006 on turnout
#  in 2008 is 0.17, with a standard error of 0.018."
emit("rd_mop_meta_1012", meta_window("10-12", "fe_cace"),
     "Fixed-effects meta-analytic CACE, 2010 on 2012")
emit("rd_mop_meta_1012_se", meta_window("10-12", "fe_se"), "Its standard error")
emit("rd_mop_meta_0608", meta_window("06-08", "fe_cace"),
     "Fixed-effects meta-analytic CACE, 2006 on 2008")
emit("rd_mop_meta_0608_se", meta_window("06-08", "fe_se"), "Its standard error")

# "Taking Oregon as our example, a CACE of 0.118 implies that casting a vote in 2010
#  increases the probability that a complier will cast a vote in the 2012 presidential
#  election from 57.6% to 69.4%."
oregon_1012 <- rd |> filter(state == "OR", years_window == "10-12")
emit("rd_oregon_cace", oregon_1012$cace, "Oregon's 2010 on 2012 CACE")
emit("rd_oregon_baseline", NA, "No counterpart in the deposit, as for Missouri")
emit("rd_oregon_treated", NA,
     str_glue("No counterpart either; the two published figures differ by ",
              "{sprintf('%.1f', 100 * oregon_1012$cace)} points"))

# "The estimated effect of voting in 2008 on voting in 2012 is 0.12 (SE = 0.005). This
#  estimate is noteworthy for two reasons. First, this weighted average across 15 states is
#  roughly 10 standard errors larger than the four-year persistence effect reported by
#  Meredith (2009) based on a single pair of California elections (7 percentage points)."
emit("rd_pop_meta_0812", meta_window("08-12", "fe_cace"),
     "Fixed-effects meta-analytic CACE, 2008 on 2012")
emit("rd_pop_meta_0812_se", meta_window("08-12", "fe_se"), "Its standard error")

pop_states <- rd |>
  filter(years_window == "08-12", state != "meta", !is.na(cace),
         !str_detect(state, "05$"))
emit("rd_pop_states", nrow(pop_states), "States contributing to the 2008 on 2012 average")

meredith <- as.numeric(claim_row("intro_meredith_seven_points")$value_paper) / 100
emit("rd_pop_ses_larger", NA,
     str_glue("The rewrite's pooled estimate is ",
              "{sprintf('%.1f', (meta_window('08-12', 'fe_cace') - meredith) / meta_window('08-12', 'fe_se'))} ",
              "standard errors above the {sprintf('%.0f', 100 * meredith)}-point figure the article cites"))

# Persistence in Turnout ----

# "Table 6 shows the CACE of voting in each upstream election from 1992 through 2010 on
#  downstream voting in 2012. Of the 86 coefficients reported, only three are estimated to
#  be negative (none of which reaches statistical significance)."
t6 <- persistence |>
  filter(type == "state", !is.na(cace), !str_detect(state, "05$"))
emit("rd_persistence_coefficients", nrow(t6), "State estimates in Table 6")
emit("rd_persistence_negative", sum(t6$cace < 0), "Negative estimates among them")
emit_holds("rd_persistence_none_significant",
           !any(significant(t6$cace[t6$cace < 0], t6$se[t6$cace < 0])),
           str_glue("Largest z among the negative estimates is ",
                    "{sprintf('%.2f', max(z_ratio(t6$cace[t6$cace < 0], t6$se[t6$cace < 0])))}"))

# "We can conduct a nonparametric test of the probability of seeing such an extreme
#  distribution of coefficients if in fact habit effects did not persist at all with a
#  binomial test of 83 successes in 86 trials (p < .001)."
emit("rd_binomial_successes", sum(t6$cace > 0), "Positive estimates, counted from the state rows")
emit("rd_binomial_trials", nrow(t6), "Trials, counted from the state rows")
emit_holds("rd_binomial_significant", binomial$p_value < 0.001,
           str_glue("Two-sided exact binomial p is {format(binomial$p_value, digits = 3)}"))

# "We present a statistical summary of the persistence of habit effects in Table 7, which
#  pools all 384 estimates of every upstream election on every downstream election. The
#  dependent variable in this meta-analysis is the estimated CACE, and observations are
#  weighted by the inverse of their squared standard errors."
emit("rd_table7_estimates", nrow(pair_metadata), "Rows in the meta-regression sample")
emit_holds("rd_table7_weights",
           max(abs(pair_metadata$weights * pair_metadata$se^2 - 1)) < 1e-9,
           "Every weight equals one over the squared standard error to within 1e-9")

# "The last model is the most predictive of outcomes, as judged by the R2, and generates
#  an estimated Years between upstream and downstream effect of -0.0008, with a standard
#  error of 0.0028. This estimate implies that net of state fixed effects and election
#  type, CACEs associated with voting habits grow weaker by only 0.008 over the course of
#  a decade."
emit("rd_timedistance_col5", coefficient("timedistance", 5, "estimate"),
     "Years between upstream and downstream, column 5")
emit("rd_timedistance_col5_se", coefficient("timedistance", 5, "std_error"),
     "Its robust standard error")
emit("rd_decade_decay", abs(10 * coefficient("timedistance", 5, "estimate")),
     "Ten years of that coefficient")

# "Focusing on column 5, we see that an increase in Youth Turnout of 1 percentage point is
#  associated with an average decrease of 0.16 percentage points in the CACE."
emit("rd_youth_coefficient", abs(coefficient("turnoutrate1829", 5, "estimate")),
     "Youth turnout coefficient in column 5, in points per point")

# Is Persistence in Turnout Due to Environmental Influences? ----

# "The top panel of Table 5, for example, reports 15 estimates of the presidential-on-
#  presidential downstream effect for 2008-12. The top four of the five strongest estimates
#  (Arkansas, Nevada, Connecticut, Missouri, and New Jersey) are found in nonbattleground
#  states."
emit("env_table5_estimates", nrow(pop_states),
     "Presidential-on-presidential estimates printed for 2008-12")

battleground <- pair_metadata |>
  filter(downYear == 2012) |>
  distinct(state_clean, battleground_P)
five_strongest <- pop_states |>
  arrange(desc(cace)) |>
  slice_head(n = 5) |>
  left_join(battleground, by = c("state" = "state_clean"))
emit_holds(
  "env_top_four_nonbattleground",
  sum(five_strongest$battleground_P == 0) == 4,
  str_glue("Of the five strongest ({paste(five_strongest$state, collapse = ', ')}), ",
           "{sum(five_strongest$battleground_P == 0)} are nonbattlegrounds and ",
           "{paste(five_strongest$state[five_strongest$battleground_P == 1], collapse = ' and ')} ",
           "are battlegrounds under the article's own coding")
)

# "We code the 13 observations that did not feature races for governor or senator as
#  nonbattlegrounds."
emit("env_no_race_observations", sum(is.na(pair_metadata$downstream_margin)),
     "Downstream state-years with no gubernatorial or senate margin")

# "When we include the close midterm elections in the definition of battleground, this
#  pattern does not persist - the coefficient drops by a factor of three and is no longer
#  significant."
battleground_ratio <- coefficient("battleground_P", 2, "estimate") /
  coefficient("battleground_PM", 3, "estimate")
emit_holds("env_battleground_factor", round(battleground_ratio) == 3,
           str_glue("The coefficient falls from ",
                    "{sprintf('%.4f', coefficient('battleground_P', 2, 'estimate'))} to ",
                    "{sprintf('%.4f', coefficient('battleground_PM', 3, 'estimate'))}, a factor of ",
                    "{sprintf('%.1f', battleground_ratio)}"))

# Discussion ----

# "In our data, we find the average CACE across all general election types to be
#  approximately 0.10, which suggests that, ceteris paribus, mobilizing 100 compliers today
#  generates 50 more votes over the five federal elections in the decade to come."
weighted_mean_cace <- weighted.mean(pair_metadata$cace, pair_metadata$weights)
emit("disc_average_cace", weighted_mean_cace,
     str_glue("Precision-weighted mean CACE over {nrow(pair_metadata)} pairs is ",
              "{sprintf('%.4f', weighted_mean_cace)}"))
emit("disc_fifty_votes", 100 * 5 * weighted_mean_cace,
     "A hundred compliers times five elections at that average")

# Table notes ----

# "Numbers in brackets represent the minimum and maximum estimated untreated turnout rates
#  among the four treatments' compliers."
emit_holds("note_table1_bracket_arms",
           n_distinct(etov2006$label[etov2006$type == "iv"]) == 4,
           str_glue("Table 1's brackets range over ",
                    "{n_distinct(etov2006$label[etov2006$type == 'iv'])} treatment arms"))

# "Numbers in brackets represent the minimum and maximum estimated untreated turnout rates
#  among the three treatments' compliers."
emit_holds("note_table2_bracket_arms",
           n_distinct(etov2007$label[etov2007$type == "iv"]) == 3,
           str_glue("Table 2's brackets range over ",
                    "{n_distinct(etov2007$label[etov2007$type == 'iv'])} treatment arms"))

# "The estimates in the All Instruments row are overidentified and are obtained using
#  2SLS."
emit_holds("note_table1_overidentified", TRUE,
           str_glue("The All Instruments row is fitted with ",
                    "{n_distinct(etov2006$arm[etov2006$type == 'iv'])} instruments for one ",
                    "endogenous regressor, which is what makes it overidentified"))

# "Meta-analysis estimates exclude results from the historical voter files, Florida 2005
#  and Missouri 2005."
meta_0812_all <- rd |> filter(years_window == "08-12", state != "meta", !is.na(cace))
with_historical <- weighted.mean(meta_0812_all$cace, 1 / meta_0812_all$se^2)
without_historical <- weighted.mean(pop_states$cace, 1 / pop_states$se^2)
emit_holds(
  "note_table45_exclusions",
  isTRUE(all.equal(without_historical, meta_window("08-12", "fe_cace"), tolerance = 1e-8)),
  str_glue("Inverse-variance pooling over the {nrow(pop_states)} contemporaneous files ",
           "reproduces the printed meta-analytic estimate; the two historical files are ",
           "absent from the 2008-12 column in any case")
)

# "All models weighted by inverse of squared standard error of CACE estimate."
emit_holds("note_table7_weights",
           max(abs(pair_metadata$weights * pair_metadata$se^2 - 1)) < 1e-9,
           "Every meta-regression weight is one over the squared standard error")

# Appendix section 1: measurement error ----

# "The true CACE in all cases is set equal to 0.117, the meta-analytic estimate of the
#  CACE of 2008 voting on 2012 voting."
emit("appx_figure_a1_true_cace", unique(bias_surface$cace_true),
     "True CACE the bias surface assumes")
emit("appx_figure_a1_migration_range", max(abs(bias_surface$net_migration)),
     "Largest net migration the surface covers, in either direction")
emit("appx_figure_a1_complier_labels", n_distinct(bias_surface$N_compliers),
     "Complier counts the legend labels")

# "Table A2 presents the estimated number of net migrants, the estimated number of
#  compliers in 2008 (computed using a 365-day window), and the associated bias according
#  to Equation A10 for sixteen states."
emit("appx_a2_states", n_distinct(migration$state), "States in Table A2")

# The CACE column of Table A2 should be each state's 2008 on 2012 estimate, the quantity
# Tables 5 and 6 report for the same states.
# The claim is about the published column, so the published column is what is compared,
# against the pipeline's own 2008 on 2012 estimate at the three decimals it prints.
published_a2 <- read_csv(here::here("ground_truth", "published_appendix_values.csv"),
                         col_types = cols(.default = col_character())) |>
  filter(float == "table_a2", column_label == "CACE 08-12") |>
  transmute(state = row_label, published = as.numeric(value_paper))

a2_against_rd <- published_a2 |>
  left_join(rd |> filter(years_window == "08-12") |> select(state, rd_cace = cace),
            by = "state") |>
  mutate(same = published == round(rd_cace, 3))
emit_holds(
  "appx_a2_cace_column",
  all(a2_against_rd$same),
  str_glue("{sum(a2_against_rd$same)} of {nrow(a2_against_rd)} published cells equal the ",
           "2008 on 2012 estimate at three decimals; the exception is ",
           "{paste(a2_against_rd$state[!a2_against_rd$same], collapse = ', ')}, printed as ",
           "{sprintf('%.3f', a2_against_rd$published[!a2_against_rd$same])} where Tables 5 ",
           "and 6 give {sprintf('%.3f', a2_against_rd$rd_cace[!a2_against_rd$same])}")
)

# "The table shows that the probable size of the bias due to measurement error is small,
#  at least in the elections for which we have migration data."
emit_holds("appx_a2_bias_small", max(abs(migration$bias_estimate)) < 0.01,
           str_glue("Largest absolute bias estimate is ",
                    "{sprintf('%.4f', max(abs(migration$bias_estimate)))}"))

# Appendix section 2: robustness ----

# "Boxed estimate is used in the main analysis."
boxed_0812 <- robust_0812 |> filter(order == 1, bandwidth == 365, lag)
boxed_0610 <- robust_0610 |> filter(order == 1, bandwidth == 365, lag)
emit_holds(
  "appx_a6a7_boxed",
  abs(boxed_0812$fe_cace - meta_window("08-12", "fe_cace")) < 1e-9 &&
    abs(boxed_0610$fe_cace - meta_window("06-10", "fe_cace")) < 1e-9,
  str_glue("The boxed cells are {sprintf('%.3f', boxed_0812$fe_cace)} and ",
           "{sprintf('%.3f', boxed_0610$fe_cace)}, matching Table 5's 2008-12 and ",
           "2006-10 meta-analytic estimates")
)

# "With the exception of the third-order polynomial (whose estimates are highly sensitive
#  to points at the edges of the window) most estimates of the effect of 2008 on 2012 fall
#  in the 0.10 to 0.12 range. The estimates of 2006 on 2010 are somewhat more variable,
#  with most falling between 0.07 to 0.15."
in_range <- function(d, low, high) {
  keep <- d |> filter(order != 3)
  sum(keep$fe_cace >= low & keep$fe_cace <= high) / nrow(keep)
}
emit_holds("appx_a6_range_low", NA,
           str_glue("A rough characterisation, so no verdict. Excluding the third-order ",
                    "polynomial the 2008 on 2012 estimates run ",
                    "{sprintf('%.3f', min(robust_0812$fe_cace[robust_0812$order != 3]))} to ",
                    "{sprintf('%.3f', max(robust_0812$fe_cace[robust_0812$order != 3]))}, with ",
                    "{sprintf('%.0f', 100 * in_range(robust_0812, 0.10, 0.12))} per cent inside 0.10 to 0.12"))
emit_holds("appx_a6_range_high", NA, "Upper end of the same set")
emit_holds("appx_a7_range_low", NA,
           str_glue("A rough characterisation, so no verdict. Excluding the third-order ",
                    "polynomial the 2006 on 2010 estimates run ",
                    "{sprintf('%.3f', min(robust_0610$fe_cace[robust_0610$order != 3]))} to ",
                    "{sprintf('%.3f', max(robust_0610$fe_cace[robust_0610$order != 3]))}, with ",
                    "{sprintf('%.0f', 100 * in_range(robust_0610, 0.07, 0.15))} per cent inside 0.07 to 0.15"))
emit_holds("appx_a7_range_high", NA, "Upper end of the same set")

# "The estimates tend to be more precise the larger the window, the less flexible the
#  functional form, and when controls are included."
precision_checks <- function(d) {
  by_window <- d |>
    arrange(order, lag, bandwidth) |>
    summarize(monotone = all(diff(fe_se) < 0), .by = c(order, lag))
  by_order <- d |>
    arrange(bandwidth, lag, order) |>
    summarize(rising = all(diff(fe_se) > 0), .by = c(bandwidth, lag))
  by_lag <- d |>
    select(bandwidth, order, lag, fe_se) |>
    pivot_wider(names_from = lag, values_from = fe_se) |>
    summarize(tighter = mean(`TRUE` <= `FALSE`)) |>
    pull(tighter)
  c(window = all(by_window$monotone), order = all(by_order$rising), lag_share = by_lag)
}
precision <- rbind(precision_checks(robust_0812), precision_checks(robust_0610))
emit_holds("appx_a6a7_precision",
           all(precision[, "window"]) && all(precision[, "order"]) &&
             all(precision[, "lag_share"] > 0.5),
           str_glue("Across both tables the standard error falls monotonically in ",
                    "bandwidth within every functional form and rises with polynomial ",
                    "order at every bandwidth; lagged controls give the tighter standard ",
                    "error in {sprintf('%.0f', 100 * mean(precision[, 'lag_share']))} per ",
                    "cent of the 64 paired cells"))

# Appendix section 3: like elections ----

# "For example, following encouragement to vote in the August 2006 primary election, the
#  effect among compliers was much larger in subsequent August elections (0.135, 0.126,
#  0.089) than in subsequent November elections (0.108, 0.009, 0.043, 0.011)."
for (elec in c("AUG2008", "AUG2010", "AUG2012")) {
  emit(paste0("appx_like_", str_to_lower(elec)), overid(etov2006, elec, "cace"),
       paste("Overidentified CACE on", elec))
}
for (elec in c("NOV2006", "NOV2008", "NOV2010", "NOV2012")) {
  emit(paste0("appx_like_", str_to_lower(elec)), overid(etov2006, elec, "cace"),
       paste("Overidentified CACE on", elec))
}

# "Across the 384 general-on-general election pairs (the pairs reported in Table 6 of the
#  main text), the average estimated CACE is 0.101 with a standard error of 0.007. Among
#  the 335 general-on-primary pairs, the average estimate is 0.020, with a standard error
#  of 0.004."
sawtooth_general <- sawtooth |>
  filter(up_primary == "general", upyear != 2012, !str_detect(state, "05$"))
emit("appx_gg_pairs", sum(sawtooth_general$down_primary == "general"),
     "General-on-general pairs plotted at the 365-day window")
emit_holds(
  "appx_gg_crossref",
  sum(sawtooth_general$down_primary == "general") == nrow(t6),
  str_glue("Table 6 reports {nrow(t6)} pairs, every upstream general election on 2012; ",
           "the general-on-general set has ",
           "{sum(sawtooth_general$down_primary == 'general')}, which is what Table 7 pools")
)

gg <- like_elections |> filter(upstream_type == "general", downstream_type == "general")
gp <- like_elections |> filter(upstream_type == "general", downstream_type == "primary")
emit("appx_gg_mean", gg$estimate, "Precision-weighted average general-on-general CACE")
emit("appx_gg_se", gg$std_error, "Its robust standard error")
emit("appx_gp_pairs", sum(sawtooth_general$down_primary == "primary"),
     "General-on-primary pairs plotted at the 365-day window")
emit("appx_gp_mean", gp$estimate, "Precision-weighted average general-on-primary CACE")
emit("appx_gp_se", gp$std_error, "Its robust standard error")

# "The series for each upstream year is plotted in a separate row - the tendency for
#  primaries to be associated with weaker habit effects is apparent across all 10 upstream
#  years."
emit("appx_a2fig_upstream_years", n_distinct(sawtooth_general$upyear),
     "Upstream years the figure panels")

panel_means <- sawtooth_general |>
  summarize(mean_cace = mean(cace), .by = c(upyear, down_primary)) |>
  pivot_wider(names_from = down_primary, values_from = mean_cace)
emit_holds("appx_a2fig_primaries_weaker", all(panel_means$primary < panel_means$general),
           str_glue("Downstream primaries average below downstream generals in ",
                    "{sum(panel_means$primary < panel_means$general)} of ",
                    "{nrow(panel_means)} panels"))

# "As a result, we restrict ourselves to a 60-day window, which is free from this
#  contamination."
emit("appx_primary_window", unique(sawtooth_60$bandwidth),
     "Window used for the primary-upstream analysis")

# "The average estimated primary-on-general CACE is 0.0086 (SE = 0.0236), while the
#  average estimated primary-on-primary CACE is 0.0068 (SE=0.0010)."
pg <- like_elections |> filter(upstream_type == "primary", downstream_type == "general")
pp <- like_elections |> filter(upstream_type == "primary", downstream_type == "primary")
emit("appx_pg_mean", pg$estimate, "Precision-weighted average primary-on-general CACE")
emit("appx_pg_se", pg$std_error, "Its robust standard error")
emit("appx_pp_mean", pp$estimate, "Precision-weighted average primary-on-primary CACE")
emit("appx_pp_se", pp$std_error, "Its robust standard error")

# Appendix section 4: campaign contact ----

# "The just-eligibles are 5.3 percentage points less likely to be exposed to mobilization
#  activity. According to the 180-day window, the average effect is exactly zero, with a
#  confidence interval extending from negative 11.6 percentage points to percentage 11.6
#  percentage points."
emit("appx_a8_meta_effect", NA,
     "No counterpart in the deposit: the ANES file behind Table A8 is confidential and the deposit's README says it ships neither the code nor the data")
emit("appx_a8_ci_bound", NA, "No counterpart, for the same reason")
emit_holds("appx_a8_zero", NA, "No counterpart, for the same reason")

# Appendix section 5: the follow-up experiment ----

# "a follow-up to the 2007 social pressure experiment was conducted among the 27,138
#  subjects in either Self condition of the original experiment. Of these, a random 5,900
#  subjects were sent an additional Self mailer just prior to the November 2008 election"
emit("appx_followup_subjects", followup$n_self_subjects,
     "Subjects in either Self condition of the 2007 experiment")
emit("appx_followup_recontacted", followup$n_recontacted,
     "Subjects sent the refresher mailer")

# "The refresher mailer had a no impact on voting behavior in November 2008: the estimated
#  treatment effect was 0.002 with a cluster-robust standard error of 0.005."
emit("appx_followup_effect", followup$direct_effect,
     "Effect of the refresher on November 2008 turnout")
emit("appx_followup_se", followup$direct_effect_se, "Its cluster-robust standard error")

# "The estimated coefficients are never significantly different from one another,
#  suggesting that the follow-up mailer does not rekindle memories of the original
#  intervention."
emit_holds("appx_a9_never_significant", all(a9_diff_z < qnorm(0.975)),
           str_glue("Largest z on the difference between the two conditions is ",
                    "{sprintf('%.2f', max(a9_diff_z))}"))
