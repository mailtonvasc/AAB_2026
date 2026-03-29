# title:          "Sleep disruption as an endophenotype of Autism"
# purpose:        "Missingness inspection — Step 1b (Handling Missing Data and Power Analysis)"
# date:           "28/03/2026"
# author:         "Mailton Vasconcelos"
# depends on:     Get-vars-new_20260316v01.R (AAB_data must be loaded)

library(dplyr)
library(tidyr)
library(ggplot2)
library(visdat)
library(naniar)   # install.packages("naniar") if not yet installed

# =============================================================================
# 0. SCOPE
# =============================================================================
# Children only (participant types 1, 3 and 4) are the analytic sample for Step 2.
# Parents (5–6) are excluded here; their missingness is handled separately
# when pedigree-based heritability models are set up (Steps 3–4).
#
# Variables inspected:
#   Outcome      : cshq_total_prorated (+ 8 subscales)
#   Grouping     : diagnosis_group, participant_type_f
#   Demographics : age, sex, site, ethnicity_collapsed
#   Covariates   : tanner_genital, melatonin_ever, melatonin_current
#   SES          : ses_composite_complete, ses_composite_partial, ses_n_components
#   Pedigree     : family_id, pedigree_linked, multiplex_approx
# =============================================================================

children <- AAB_data %>%
  filter(participant_type %in% c(1, 3, 4))

# Define the analysis variable set
analysis_vars <- c(
  # Outcome
  "cshq_total_prorated",
  "cshq_bedtime_resistance",
  "cshq_sleep_onset_delay",
  "cshq_sleep_duration",
  "cshq_sleep_anxiety",
  "cshq_night_wakings",
  "cshq_parasomnias",
  "cshq_sdb",
  "cshq_daytime_sleepiness",
  # Grouping / diagnosis
  "diagnosis_group",
  # Demographics
  "age",
  "sex",
  "site",
  "ethnicity_collapsed",
  # Pubertal / medication covariates
  "tanner_genital",
  "melatonin_ever",
  "melatonin_current",
  # SES
  "ses_composite_complete",
  "ses_composite_partial",
  "ses_n_components",
  # Pedigree
  "family_id",
  "pedigree_linked",
  "multiplex_approx"
)

# Subset to analysis vars (drop any that don't yet exist — future-proofs the script)
available_vars <- intersect(analysis_vars, names(children))
missing_vars   <- setdiff(analysis_vars, names(children))

if (length(missing_vars) > 0) {
  message("⚠ The following variables are not yet in AAB_data and will be skipped:\n  ",
          paste(missing_vars, collapse = ", "))
}

rm(missing_vars, key_vars, available_vars, analysis_vars)

children_av <- children %>% select(participant_type_f, all_of(available_vars))

# =============================================================================
# 1. OVERALL MISSINGNESS SUMMARY TABLE
# =============================================================================
# n missing, % missing, n complete — for the whole analytic sample

miss_summary <- children_av %>%
  select(all_of(available_vars)) %>%
  summarise(across(
    everything(),
    list(
      n_total   = ~ n(),
      n_missing = ~ sum(is.na(.x)),
      pct_miss  = ~ round(mean(is.na(.x)) * 100, 1),
      n_present = ~ sum(!is.na(.x))
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to  = c("variable", ".value"),
    names_sep = "__"
  ) %>%
  arrange(desc(pct_miss))

cat("\n=== 1. OVERALL MISSINGNESS (children, n =", nrow(children_av), ") ===\n")
print(miss_summary, n = Inf)

# Decision, since Tanner Stages is not applicable to a considerable part of the sample
# it will not enter the models, age you be the sole proxy of development.

# =============================================================================
# 2. MISSINGNESS BY PARTICIPANT TYPE
# =============================================================================
# Reveals whether missingness is concentrated in a specific group
# (e.g., controls missing CSHQ by design vs. probands missing by omission)

miss_by_type <- children_av %>%
  group_by(participant_type_f) %>%
  summarise(across(
    all_of(available_vars),
    ~ round(mean(is.na(.x)) * 100, 1),
    .names = "{.col}"
  )) %>%
  pivot_longer(-participant_type_f,
               names_to  = "variable",
               values_to = "pct_missing") %>%
  pivot_wider(names_from = participant_type_f, values_from = pct_missing)

cat("\n=== 2. % MISSING BY PARTICIPANT TYPE ===\n")
print(miss_by_type, n = Inf)

# Decision: SES is 100% missing for Comparison children since SES is rerived from parents.
# The plan now is to drop SES from the main Step 2 models entirely and run a sensitivity 
# analysis in probands + siblings only where SES is available

# =============================================================================
# 3. MISSINGNESS BY SITE
# =============================================================================
# The README flags WA (site 1) as having a family-linkage problem;
# check whether sleep data missingness also clusters there.

if ("site" %in% available_vars) {
  miss_by_site <- children_av %>%
    group_by(site) %>%
    summarise(across(
      all_of(setdiff(available_vars, "site")),
      ~ round(mean(is.na(.x)) * 100, 1),
      .names = "{.col}"
    )) %>%
    pivot_longer(-site,
                 names_to  = "variable",
                 values_to = "pct_missing") %>%
    pivot_wider(names_from = site, values_from = pct_missing)
  
  cat("\n=== 3. % MISSING BY SITE ===\n")
  print(miss_by_site, n = Inf)
}

# Decision: The family_id and pedigree_linked missingness is concentrated in WA (16.1% and 13.6%).
# Consistent with the known WA data entry omission for parents. WA should be a sensitivity 
# analysis stratum. Run models with and without WA site to test whether results are
# robust to this data quality issue.

# =============================================================================
# 4. CSHQ-SPECIFIC MISSINGNESS DETAIL
# =============================================================================
# For Step 2, cshq_total_prorated is the primary outcome.
# Understand who is missing it and why.

cat("\n=== 4. CSHQ TOTAL PRORATED — MISSINGNESS DETAIL ===\n")

# 4a. Cross-tab: CSHQ availability by participant type × diagnosis group
children %>%
  count(participant_type_f, diagnosis_group,
        cshq_available = !is.na(cshq_total_prorated)) %>%
  pivot_wider(names_from = cshq_available,
              values_from = n,
              names_prefix = "cshq_") %>%
  rename(cshq_missing = `cshq_FALSE`, cshq_present = `cshq_TRUE`) %>%
  mutate(pct_missing = round(cshq_missing / (cshq_missing + cshq_present) * 100, 1)) %>%
  print()

# 4b. Cross-tab: CSHQ availability by site
if ("site" %in% names(children)) {
  children %>%
    count(site, cshq_available = !is.na(cshq_total_prorated)) %>%
    pivot_wider(names_from = cshq_available,
                values_from = n,
                names_prefix = "cshq_") %>%
    rename(cshq_missing = `cshq_FALSE`, cshq_present = `cshq_TRUE`) %>%
    mutate(pct_missing = round(cshq_missing / (cshq_missing + cshq_present) * 100, 1)) %>%
    print()
}

# 4c. Among those missing CSHQ — what other key variables are available?
#     Helps judge whether complete-case analysis would be systematically biased.
cat("\n--- Among participants with MISSING cshq_total_prorated ---\n")
children %>%
  filter(is.na(cshq_total_prorated),
         participant_type %in% c(1, 3, 4)) %>%   # probands, siblings, controls
  summarise(
    n                       = n(),
    pct_age_present         = round(mean(!is.na(age)) * 100, 1),
    pct_sex_present         = round(mean(!is.na(sex)) * 100, 1),
    pct_diagnosis_present   = round(mean(!is.na(diagnosis_group)) * 100, 1),
    pct_ses_present         = round(mean(!is.na(ses_composite_complete)) * 100, 1),
    pct_tanner_present      = round(mean(!is.na(tanner_genital)) * 100, 1),
    pct_family_linked       = round(mean(!is.na(family_id)) * 100, 1)
  ) %>%
  print()

# =============================================================================
# 5. KEY COVARIATE AVAILABILITY AMONG CSHQ-COMPLETE CASES
# =============================================================================
# The Step 2 mixed model requires: cshq_total_prorated, diagnosis_group,
# age, sex, melatonin_ever, family_id.
# Check how many cases would be dropped by complete-case analysis.

step2_vars <- c("cshq_total_prorated", "diagnosis_group",
                "age", "sex", "melatonin_ever", "family_id")
step2_vars_available <- intersect(step2_vars, names(children))

cat("\n=== 5. COMPLETE CASES FOR STEP 2 MODEL VARIABLES ===\n")
cat("Variables checked:", paste(step2_vars_available, collapse = ", "), "\n\n")

children %>%
  filter(participant_type %in% c(1, 3, 4)) %>%   # ASD, Sibling, Comparison
  mutate(
    complete_step2 = complete.cases(across(all_of(step2_vars_available)))
  ) %>%
  group_by(participant_type_f) %>%
  summarise(
    n_total    = n(),
    n_complete = sum(complete_step2),
    n_dropped  = n_total - n_complete,
    pct_retained = round(n_complete / n_total * 100, 1)
  ) %>%
  print()

# Optional: show which variable is the bottleneck for incomplete cases
children %>%
  filter(participant_type %in% c(1, 3, 4),
         !complete.cases(across(all_of(step2_vars_available)))) %>%
  summarise(across(
    all_of(step2_vars_available),
    ~ sum(is.na(.x)),
    .names = "missing_{.col}"
  )) %>%
  print()

# =============================================================================
# 6. MISSING DATA PATTERN — naniar upset plot
# =============================================================================
# Shows co-occurrence of missingness across variables.
# Useful for deciding whether missingness is MCAR, MAR, or MNAR.

key_vars_plot <- c(
  "cshq_total_prorated",
  "diagnosis_group",
  "age",
  "sex",
  "tanner_genital",
  "ses_composite_complete",
  "melatonin_ever",
  "ethnicity_collapsed",
  "family_id"
)

key_vars_plot <- intersect(key_vars_plot, names(children))

p_upset <- children %>%
  filter(participant_type %in% c(1, 3, 4)) %>%
  select(all_of(key_vars_plot)) %>%
  gg_miss_upset(nsets = length(key_vars_plot), nintersects = 20)

print(p_upset)

# =============================================================================
# 7. VISUAL OVERVIEW — visdat heatmap
# =============================================================================
# Colour-coded grid: present / missing / class, sorted by missingness.
# Gives an at-a-glance sense of the data structure before modelling.

p_visdat <- children %>%
  filter(participant_type %in% c(1, 3, 4)) %>%
  select(participant_type_f, all_of(key_vars_plot)) %>%
  vis_miss(sort_miss = TRUE, show_perc = TRUE) +
  labs(title = "Missingness heatmap — analytic sample (ASD / Sibling / Comparison)",
       subtitle = paste0("n = ", sum(children$participant_type %in% c(1, 3, 4)),
                         " children; sorted by % missing")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_visdat)

# =============================================================================
# 8. MISSINGNESS AND KEY PREDICTORS — MAR CHECKS
# =============================================================================
# Test whether CSHQ missingness is related to observed variables.
# If it is, data are MAR (not MCAR) → multiple imputation is justified.
# These are exploratory; formal Little's MCAR test can be added if needed.

cat("\n=== 8. MAR CHECKS — is CSHQ missingness predicted by observed variables? ===\n")

children_mar <- children %>%
  filter(participant_type %in% c(1, 3, 4)) %>%
  mutate(cshq_missing = as.integer(is.na(cshq_total_prorated)))

# 8a. Missingness by diagnosis group (chi-squared)
if ("diagnosis_group" %in% names(children_mar)) {
  cat("\n--- 8a. CSHQ missing × diagnosis_group ---\n")
  tbl_diag <- table(children_mar$cshq_missing, children_mar$diagnosis_group,
                    useNA = "ifany")
  print(tbl_diag)
  print(chisq.test(tbl_diag))
}

# 8b. Missingness by site (chi-squared)
if ("site" %in% names(children_mar)) {
  cat("\n--- 8b. CSHQ missing × site ---\n")
  tbl_site <- table(children_mar$cshq_missing, children_mar$site, useNA = "ifany")
  print(tbl_site)
  print(chisq.test(tbl_site))
}

# 8c. Missingness by sex (chi-squared)
if ("sex" %in% names(children_mar)) {
  cat("\n--- 8c. CSHQ missing × sex ---\n")
  tbl_sex <- table(children_mar$cshq_missing, children_mar$sex, useNA = "ifany")
  print(tbl_sex)
  print(chisq.test(tbl_sex))
}

# 8d. Age difference between CSHQ-present and CSHQ-missing (t-test)
if ("age" %in% names(children_mar)) {
  cat("\n--- 8d. Age ~ CSHQ missingness ---\n")
  print(t.test(age ~ cshq_missing, data = children_mar))
}

# 8e. SES difference (t-test)
if ("ses_composite_complete" %in% names(children_mar)) {
  cat("\n--- 8e. SES ~ CSHQ missingness ---\n")
  print(t.test(ses_composite_complete ~ cshq_missing, data = children_mar))
}

# =============================================================================
# 9. PEDIGREE LINKAGE — impact on analytic N
# =============================================================================
# Steps 3–4 (heritability, genetic correlation) require pedigree linkage.
# Document here how many CSHQ-complete probands would be excluded.

cat("\n=== 9. PEDIGREE LINKAGE AMONG CSHQ-COMPLETE PROBANDS ===\n")

if ("pedigree_linked" %in% names(children)) {
  children %>%
    filter(participant_type == 1, !is.na(cshq_total_prorated)) %>%
    count(pedigree_linked, useNA = "always") %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    print()
}

# =============================================================================
# 10. SUMMARY FLAGS — quick reference for methods section
# =============================================================================

cat("\n=== 10. SUMMARY FLAGS ===\n")

n_children      <- nrow(children %>% filter(participant_type %in% c(1, 3, 4)))
n_cshq_complete <- sum(!is.na(children$cshq_total_prorated[children$participant_type %in% c(1, 3, 4)]))
n_step2_complete <- children %>%
  filter(participant_type %in% c(1, 3, 4)) %>%
  filter(complete.cases(across(all_of(step2_vars_available)))) %>%
  nrow()

cat(sprintf("  Analytic sample (ASD + Sibling + Comparison):  n = %d\n",  n_children))
cat(sprintf("  With cshq_total_prorated:                       n = %d (%.1f%%)\n",
            n_cshq_complete, n_cshq_complete / n_children * 100))
cat(sprintf("  Complete cases for Step 2 model:                n = %d (%.1f%%)\n",
            n_step2_complete, n_step2_complete / n_children * 100))
cat("\n  → If CSHQ missingness is MAR (supported by section 8), proceed with:\n")
cat("      Primary:     cshq_total_prorated (prorated complete cases)\n")
cat("      Sensitivity: multiple imputation via mice()\n")

rm(children)