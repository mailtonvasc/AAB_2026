# title:          "Sleep disruption as an endophenotype of Autism"
# purpose:        "Output for FamilySLeeps"
# date:           "11/02/2026"
# author:         "Mailton Vasconcelos"

# ---- Packages ----
library(tidyr)
library(dplyr)
library(visdat) #for inspecting missingness
library(lubridate)
library(psych)    # for describe(), skewness/kurtosis
library(ggpubr)   # nice ggplot-based QQ plots


# ---- Descriptive statistics ----
#Pay attention if we're going to need gender and participant type as factor

## 1 Define variable groups for Step 1a
### Demographics
demo_num   <- c("age_at_fhq")
demo_cat   <- c("participant_type", "gender")

### All FHQ H16 items (observed + reverse-coded)
fhq_items  <- grep("^fhq_h16_", names(AAB_step_1), value = TRUE)

### CSHQ subscales + total (your main candidate endophenotypes)
cshq_vars <- c(
  "s_cshq_bedtime_resistance",
  "s_cshq_sleep_onset_delay",
  "s_cshq_sleep_duration",
  "s_cshq_sleep_anxiety",
  "s_cshq_night_wakings",
  "s_cshq_parasomnias",
  "s_cshq_sdb",
  "s_cshq_daytime_sleepiness",
  "cshq_total_33"
)

AAB_step_1 <- AAB_step_1 %>%
  mutate(across(all_of(c(demo_num, cshq_vars)), as.numeric))

## 2 Sample size overall and by participant_type

### Overall N = 2887
nrow(AAB_step_1)

### By participant type
AAB_step_1 %>%
  count(participant_type) %>%
  mutate(prop = n / sum(n))

#I've stoped here
# ---- Continue ----

## 3 Descriptive stats for numeric variables (age + CSHQ)

describe(AAB_step_1[, c(demo_num, cshq_vars)])

#I've run it quickly and I see that age isn't the right variable.

3.3 Categorical variables (gender, participant_type)
# Overall distributions
table(AAB_step_1$gender)
prop.table(table(AAB_step_1$gender))

table(AAB_step_1$participant_type)
prop.table(table(AAB_step_1$participant_type))

# Gender by participant_type
tab_pt_gender <- table(AAB_step_1$participant_type, AAB_step_1$gender)
tab_pt_gender
prop.table(tab_pt_gender, margin = 1)  # row-wise percentages

3.4 Descriptive stats by participant_type (for main sleep traits)
This is often what you’ll want to report in the methods/results when introducing the candidate endophenotypes.
AAB_step_1 %>%
  group_by(participant_type) %>%
  summarise(
    n = n(),
    across(
      all_of(c(demo_num, cshq_vars)),
      list(
        mean = ~mean(., na.rm = TRUE),
        sd   = ~sd(., na.rm = TRUE),
        med  = ~median(., na.rm = TRUE),
        p25  = ~quantile(., 0.25, na.rm = TRUE),
        p75  = ~quantile(., 0.75, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

4. Descriptive stats for FHQ items
The FHQ items are likely binary or ordinal, so frequency tables make more sense than means.

# Quick frequencies for all FHQ items
fhq_summary <- lapply(fhq_items, function(v) {
  tab <- table(AAB_step_1[[v]], useNA = "ifany")
  prop <- prop.table(tab)
  list(var = v, counts = tab, proportions = prop)
})

# Example: look at first FHQ item in the list
fhq_summary[[1]]

OR tidy version


AAB_step_1 %>%
  pivot_longer(cols = all_of(fhq_items),
               names_to = "item",
               values_to = "response") %>%
  group_by(item, response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(item) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(item, response)

5. Distribution checks: visual
Now we check if the numeric variables (especially CSHQ subscales & total, age_at_fhq) are approximately normal or heavily skewed. This will inform whether you can use parametric tests later or if transformations are needed.
5.1 Histograms & density plots (example for cshq_total_33)

ggplot(AAB_step_1, aes(x = cshq_total_33)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, color = "black", fill = "skyblue") +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() +
  labs(title = "Distribution of CSHQ total score",
       x = "CSHQ Total (33-item)", y = "Density")

To see it by participant_type:

  ggplot(AAB_step_1, aes(x = cshq_total_33, fill = participant_type)) +
  geom_histogram(position = "identity",
                 alpha = 0.4, bins = 30) +
  theme_minimal() +
  labs(title = "CSHQ total by participant type",
       x = "CSHQ Total (33-item)", y = "Count")

Repeat the same structure for other subscales, e.g.:
  
  ggplot(AAB_step_1, aes(x = s_cshq_sleep_duration)) +
  geom_histogram(bins = 30, color = "black", fill = "lightgreen") +
  theme_minimal() +
  labs(title = "CSHQ Sleep Duration Subscale",
       x = "Score", y = "Count")

5.2 Automate histograms for all CSHQ variables (optional)

for (v in cshq_vars) {
  p <- ggplot(AAB_step_1, aes(x = .data[[v]])) +
    geom_histogram(bins = 30, color = "black", fill = "steelblue") +
    theme_minimal() +
    labs(title = paste("Distribution of", v), x = v, y = "Count")
  print(p)
}
``

6. Distribution checks: QQ plots & normality tests
6.1 QQ plot for CSHQ total

ggqqplot(AAB_step_1$cshq_total_33,
         title = "QQ plot of CSHQ Total Score")

6.2 Shapiro–Wilk test (for smaller subsamples)
Shapiro–Wilk is sensitive with large N (often rejects normality), but it’s useful as a formal check.
# Overall
shapiro.test(AAB_step_1$cshq_total_33)  # ideally if n <= 5000

# By participant_type (example)
AAB_step_1 %>%
  group_by(participant_type) %>%
  summarise(
    shapiro_p = if (sum(!is.na(cshq_total_33)) >= 3 &&
                    sum(!is.na(cshq_total_33)) <= 5000) {
      shapiro.test(cshq_total_33)$p.value
    } else {
      NA_real_
    }
  )

7. Skewness and kurtosis (numeric summary of distribution shape)
psych::describe() already gives skew and kurtosis, but you can extract them:
  
  desc_cshq <- describe(AAB_step_1[, cshq_vars])
desc_cshq[, c("n", "mean", "sd", "skew", "kurtosis")]

Rules of thumb:
  
  |skew| < 1 → roughly symmetric
skew > 1 or < -1 → noticeably skewed
kurtosis > 3 (depending on definition) → heavier tails than normal

This can feed into decisions later (e.g., transformations, non-parametric models).

8. Quick missingness check (even though it’s more Step 1b)
Even at Step 1a, it’s helpful to see missingness per variable before heavier modelling.

vars_of_interest <- c(demo_num, demo_cat, cshq_vars, fhq_items)

missing_summary <- tibble(
  variable = vars_of_interest,
  n_missing = sapply(AAB_step_1[vars_of_interest], function(x) sum(is.na(x))),
  prop_missing = n_missing / nrow(AAB_step_1)
)

missing_summary

You’ll build on this in Step 1b (Missing Data Handling), but it’s nice to already see where data are sparse.

9. How this aligns with your research plan

You’ve defined candidate endophenotypes (CSHQ total & subscales) and relevant covariates. [Reseach Pl...0251121v02 | Word]
The code above gives:
  
  Descriptive stats (means, SDs, medians, IQRs, min/max)
Categorical summaries (participant type, gender, FHQ responses)
Distribution checks (histograms, density, QQ, skew & kurtosis, Shapiro tests)


These outputs will feed directly into:
  
  Reporting your sample characteristics in the manuscript.
Deciding on transformations or modelling choices (e.g., if CSHQ variables are strongly skewed before mixed models, heritability analyses, etc.)

# Summary by group
table1(~ CSHQ_total + CSHQ_bedtime_resistance + CSHQ_sleep_onset_delay +
         CSHQ_sleep_duration + CSHQ_night_wakings + CSHQ_parasomnias +
         CSHQ_sleep_disordered_breathing + CSHQ_daytime_sleepiness | participant_group,
       data = AAB_subset_1)

#Key Things to Look For


##Sample Size per Group

###Check if groups are balanced or if one group dominates.
###Important for interpreting later statistical tests and power.



##Age Distribution

###Are probands younger or older than siblings/controls?
###Age differences can confound sleep trait comparisons.



##Sex Distribution

###Is there a strong male bias in probands (common in autism cohorts)?
###Consider adjusting for sex in later models.



##CSHQ Total Score

###Compare mean and SD across groups.
###Higher scores in probands would support the hypothesis that sleep disruption is linked to autism.



##CSHQ Subscales

###Look for specific patterns:

####Bedtime resistance or sleep onset delay higher in probands?
####Daytime sleepiness differences across groups?
####These patterns help prioritise traits for endophenotype testing.


#Missingness

#Are some traits missing more often in certain groups?
# If probands have more missing data, plan imputation carefully.



#Site Effects

#If included, check whether sites differ in demographics or sleep scores.
#Could indicate batch effects or recruitment differences.

# --- Missingness check ---
miss_var_summary(AAB_data[, sleep_vars])
vis_miss(AAB_data[, sleep_vars])

airquality %>%
  arrange(Temp) %>%
  vis_miss()

# --- Distribution plots ---
##Check simmetry
AAB_data %>%
  pivot_longer(cols = all_of(sleep_vars), names_to = "trait", values_to = "score") %>%
  ggplot(aes(x = score)) +
  geom_histogram(binwidth = 1, fill = "steelblue", colour = "white") +
  facet_wrap(~ trait, scales = "free") +
  theme_minimal()

# --- Normality check ---
# Shapiro-Wilk for each trait
lapply(sleep_vars, function(v) shapiro.test(AAB_data[[v]]))


# --- Check outliers --- with boxplots

# Old from cathy

vars <-c('src_subject_id',
         'interview_date',
         'eventname',
         'site_id_l',
         'interview_age',
         'sex',
         'reshist_addr1_adi_wsum',
         'reshist_addr1_adi_perc',
         'household.income.3level',
         'household.income.10level',
         'pds_total_py',
         'race.6level.p',
         'race.4level.p',
         'race.6level.y',
         'race.4level.y',
         'race_ethnicity',
         'household.educ',
         'married',
         'married.or.livingtogether',
         'rel_family_id',
         'rel_birth_id',
         'demo_roster_v2',
         'sleepdisturb1_p',
         "sds_p_ss_dims",   # Disorders of Initiating and Maintaining Sleep (DIMS) SUM
         "sds_p_ss_sbd",    # Sleep Breathing disorders (SBD)
         "sds_p_ss_da",     # Disorder of Arousal (DA
         "sds_p_ss_swtd",   # Sleep-Wake transition Disorders (SWTD)
         "sds_p_ss_does",   # Disorders of Excessive Somnolence (DOES) SUM:
         "sds_p_ss_shy",    # Sleep Hyperhydrosis (SHY) SUM
         "sds_p_ss_total",
         "ssrs_p_ss_sum",   # SRS Score
         'physical_activity1_y',
         'screentime_total_p',
         'pds_total_py_male',
         'pds_total_py_female',
         'mctq_sjlrel_calc', 	  #	Relative social jetlag
         'mctq_sjl_calc',#	Absolute social jetlag
         'chronotype',
         'mctq_msfsc_calc',
         'mctq_msfsc_correct', #get rid of two peaks
         'mctq_sow_calc',#	Sleep onset, workday
         'mctq_sof_calc',	#	Sleep onset, freeday
         'mctq_guw_calc',	#	Local time of getting out of bed, workday
         'mctq_guf_calc',	#	Local time of getting out of bed, freeday
         "nihtbx_totalcomp_fc", #Cognition Total Composite Score Fully-Corrected T-score)
         "cbcl_scr_syn_internal_t", #	Internal CBCL Syndrome Scale (t-score)
         "cbcl_scr_syn_external_t" #	External CBCL Syndrome Scale (t-score)	
)

head(ABCD5_core_srs$sds_p_ss_total)
head(ABCD5_core$sds_p_ss_total)
#make a new dataset for the srs models

ABCD5_core_srs <- ABCD5_core[,vars]
#setwd("~/ABCD/Autistic_Traits")
#======================================================================================
#
# 1. Get age-adjusted sleep data 
#
#======================================================================================

# this parameter was not included in the models because it was affected by eventname

# sleep_model_data <- ABCD5_core_srs[, c("src_subject_id", "eventname", "sds_p_ss_total", "interview_age")]
# sleep_model_data <- sleep_model_data[complete.cases(sleep_model_data), ]
# 
# sleep_adj_model <- lm(sds_p_ss_total ~ interview_age, data = sleep_model_data)
# 
# # Add residuals directly to the model data (which already has identifiers)
# sleep_model_data$SDS_total_adj <- resid(sleep_adj_model)
# 
# # Merge back using primary keys
# ABCD5_core_srs <- merge(
#   ABCD5_core_srs,
#   sleep_model_data[, c("src_subject_id", "eventname", "SDS_total_adj")],
#   by = c("src_subject_id", "eventname"),
#   all.x = TRUE
# )
# 
# # Validation checks
# cat("Rows in ABCD5_core_srs after merge:", nrow(ABCD5_core_srs), "\n")
# missing_resids <- sum(is.na(ABCD5_core_srs$SDS_total_adj))
# cat("Number of missing residuals:", missing_resids, "\n")
# 
# # More comprehensive validation
# cat("Original sleep values available:", sum(!is.na(ABCD5_core_srs$sds_p_ss_total)), "\n")
# cat("Age values available:", sum(!is.na(ABCD5_core_srs$interview_age)), "\n")
# cat("Complete cases (both sleep & age):", sum(!is.na(ABCD5_core_srs$sds_p_ss_total) & !is.na(ABCD5_core_srs$interview_age)), "\n")
# 
# # Check alignment: compare a few cases manually
# head(ABCD5_core_srs[!is.na(ABCD5_core_srs$SDS_total_adj), 
#                     c("src_subject_id", "eventname", "sds_p_ss_total", "interview_age", "SDS_total_adj")])
# 
# # Verify residuals sum to approximately zero within each eventname - this includes NA
# print(aggregate(SDS_total_adj ~ eventname, data = ABCD5_core_srs, FUN = function(x) round(mean(x, na.rm = TRUE), 6)))
# 
# # Additional safety check: verify the residuals match expected values for a subset
# # Take first 10 complete cases and manually verify - names are differenct but values ok
# test_subset <- ABCD5_core_srs[!is.na(ABCD5_core_srs$SDS_total_adj), ][1:min(10, sum(!is.na(ABCD5_core_srs$SDS_total_adj))), ]
# manual_residuals <- test_subset$sds_p_ss_total - predict(sleep_adj_model, newdata = test_subset)
# cat("Manual residual calculation matches:", all.equal(test_subset$SDS_total_adj, manual_residuals, tolerance = 1e-10), "\n")

#=====================================================================================================================================
#
# Make a dataset with only year 1 and year 3 - input some variables from other events - makes age a categorical factor (eventname)
#
#=====================================================================================================================================


#-----sex - get baseline and fill in all NAs in other eventnames
table(ABCD5_core_srs$sex, ABCD5_core_srs$eventname, useNA="always")

sex_base <- ABCD5_core_srs %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, sex_base = sex)

ABCD5_core_srs <- ABCD5_core_srs %>%
  left_join(sex_base, by = "src_subject_id")

tapply(ABCD5_core_srs$sex_base, ABCD5_core_srs$eventname, summary)


ABCD5_core_srs$sex <- factor(as.numeric(ABCD5_core_srs$sex),    levels = 1:2, labels = c("Male", "Female"))

#-----site_id -fine
table(ABCD5_core_srs$site_id_l, ABCD5_core_srs$eventname,useNA="always")
tapply(ABCD5_core_srs$site_id_l, ABCD5_core_srs$eventname, summary)
summary(ABCD5_core_srs$site_id_l)

#-----interview_age - fine
tapply(ABCD5_core_srs$interview_age, ABCD5_core_srs$eventname, summary)


#-----deprivation percentile - get from baseline - rename as SEP
tapply(ABCD5_core_srs$reshist_addr1_adi_perc, ABCD5_core_srs$eventname, summary)

SEP <- ABCD5_core_srs %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, SEP = reshist_addr1_adi_perc)

ABCD5_core_srs <- ABCD5_core_srs %>%
  left_join(SEP, by = "src_subject_id")

tapply(ABCD5_core_srs$SEP, ABCD5_core_srs$eventname, summary)

#-----ethnicity - fine
table(ABCD5_core_srs$race.4level.p, ABCD5_core_srs$eventname)
ABCD5_core_srs$race.4level.p <- factor(ABCD5_core_srs$race.4level.p)
ABCD5_core_srs$race.4level.p <- relevel(ABCD5_core_srs$race.4level.p, ref = "White")

#-----physical activity = replace year 1 with baseline and keep year 3 as it is
tapply(ABCD5_core_srs$physical_activity1_y, ABCD5_core_srs$eventname, summary)

# Extract baseline PA values
baseline_pa <- ABCD5_core_srs %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, baseline_pa = physical_activity1_y)

ABCD5_core_srs <- ABCD5_core_srs %>%
  left_join(baseline_pa, by = "src_subject_id") %>%
  mutate(PA = case_when(
    eventname == "1_year_follow_up_y_arm_1" ~ baseline_pa,
    eventname == "3_year_follow_up_y_arm_1" ~ physical_activity1_y,
    TRUE ~ NA_real_
  )) %>%
  dplyr::select(-baseline_pa)  

tapply(ABCD5_core_srs$PA, ABCD5_core_srs$eventname, summary)
# Baseline
ABCD5_core_srs %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, physical_activity1_y) %>%
  head(10)

# 1-year follow-up
ABCD5_core_srs %>%
  filter(eventname == "1_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, PA) %>%
  head(10)


#-----screentime - replace with 2 year for time 1 and time 2 is 3-year (right data) 

tapply(ABCD5_core_srs$screentime_total_p, ABCD5_core_srs$eventname, summary)

# Extract 2yr screentime values
screen2 <- ABCD5_core_srs %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, screen2_val = screentime_total_p)

# Join and create the new 'screens' column
ABCD5_core_srs <- ABCD5_core_srs %>%
  left_join(screen2, by = "src_subject_id") %>%
  mutate(screens = case_when(
    eventname == "1_year_follow_up_y_arm_1" ~ screen2_val,
    eventname == "3_year_follow_up_y_arm_1" ~ screentime_total_p,
    TRUE ~ NA_real_
  )) %>%
  dplyr::select(-screen2_val)

tapply(ABCD5_core_srs$screens, ABCD5_core_srs$eventname, summary)
tapply(ABCD5_core_srs$screentime_total_p, ABCD5_core_srs$eventname, summary)

ABCD5_core_srs %>%
  filter(eventname == "3_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, screens) %>%
  head(10)

ABCD5_core_srs %>%
  filter(eventname == "3_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, screentime_total_p) %>%
  head(10)


# -----Cognition Total Composite Score Fully-Corrected T-score - replace with baseline
tapply(ABCD5_core_srs$nihtbx_totalcomp_fc, ABCD5_core_srs$eventname, summary)

cognition <- ABCD5_core_srs %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, cognition = nihtbx_totalcomp_fc)

ABCD5_core_srs <- ABCD5_core_srs %>%
  left_join(cognition, by = "src_subject_id")

tapply(ABCD5_core_srs$cognition, ABCD5_core_srs$eventname, summary)


#	------------Internal CBCL Syndrome Scale (t-score) nihtbx_totalcomp_fc - fine
tapply(ABCD5_core_srs$cbcl_scr_syn_internal_t, ABCD5_core_srs$eventname, summary)


#-------------External CBCL Syndrome Scale (t-score)cbcl_scr_syn_external_t - fine
tapply(ABCD5_core_srs$cbcl_scr_syn_external_t, ABCD5_core_srs$eventname, summary)


# ---------------------SRS ABCD5_core_srs$ssrs_p_ss_sum - copy year 1 to year3 - in the model they both (the timepoints) have the same baseline srs
tapply(ABCD5_core_srs$ssrs_p_ss_sum, ABCD5_core_srs$eventname, summary)

social <- ABCD5_core_srs %>%
  filter(eventname == "1_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, srs = ssrs_p_ss_sum)

ABCD5_core_srs <- ABCD5_core_srs %>%
  left_join(social, by = "src_subject_id")

tapply(ABCD5_core_srs$srs, ABCD5_core_srs$eventname, summary)


# --------------------- relatedness - move from baseline
tapply(ABCD5_core_srs$rel_family_id, ABCD5_core_srs$eventname, summary)

related <- ABCD5_core_srs %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, related = rel_family_id)

ABCD5_core_srs <- ABCD5_core_srs %>%
  left_join(related, by = "src_subject_id")

tapply(ABCD5_core_srs$related, ABCD5_core_srs$eventname, summary)

# --------------------- int sleep disturbance - fine 
tapply(ABCD5_core_srs$sds_p_ss_total, ABCD5_core_srs$eventname, summary)


# --------------------- cat sleep disturbance - fine 

tapply(ABCD5_core_srs$sds_p_ss_total, ABCD5_core_srs$eventname, summary)

ABCD5_core_srs <- ABCD5_core_srs %>%
  mutate(sds_p_ss_total = as.numeric(sds_p_ss_total),  # Ensure it's numeric
         sleep_disturb_cat = case_when(
           !is.na(sds_p_ss_total) & sds_p_ss_total <= 39 ~ "Normal sleep",
           !is.na(sds_p_ss_total) & sds_p_ss_total >= 40 & sds_p_ss_total <= 48 ~ "Borderline sleep disturbance",
           !is.na(sds_p_ss_total) & sds_p_ss_total > 48 ~ "Clinically significant sleep disturbance",
           TRUE ~ NA_character_  # Preserve missing values
         ),
         sleep_disturb_cat = factor(sleep_disturb_cat, levels = c(
           "Normal sleep", 
           "Borderline sleep disturbance", 
           "Clinically significant sleep disturbance"
         )))  # Explicitly define order


ABCD5_core_srs$sleep_disturb_cat <- relevel(ABCD5_core_srs$sleep_disturb_cat, ref = "Normal sleep")

table(ABCD5_core_srs$sleep_disturb_cat, ABCD5_core_srs$eventname)


# --------------------- Relative social jetlag - includes negatives - replace time 1 with year 2, time 2 is fine  ------
tapply(ABCD5_core_srs$mctq_sjlrel_calc, ABCD5_core_srs$eventname, summary)

# Extract 2yr
socialJL <- ABCD5_core_srs %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, sjl_2 = mctq_sjlrel_calc)


ABCD5_core_srs <- ABCD5_core_srs %>%
  left_join(socialJL, by = "src_subject_id") %>%
  mutate(sjl = case_when(
    eventname == "1_year_follow_up_y_arm_1" ~ sjl_2,
    eventname == "3_year_follow_up_y_arm_1" ~ mctq_sjlrel_calc,
    
    TRUE ~ NA_real_
  )) %>%
  dplyr::select(-sjl_2)

tapply(ABCD5_core_srs$sjl, ABCD5_core_srs$eventname, summary)
tapply(ABCD5_core_srs$mctq_sjlrel_calc, ABCD5_core_srs$eventname, summary)
tapply(ABCD5_core_srs$mctq_sjl_calc, ABCD5_core_srs$eventname, summary)

ABCD5_core_srs %>%
  filter(eventname == "1_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, sjl) %>%
  head(10)

ABCD5_core_srs %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, mctq_sjlrel_calc) %>%
  head(10)

tapply(ABCD5_core_srs$sjl, ABCD5_core_srs$eventname, summary)



# --------------------- absolute social jetlag - only postive - replace time 1 with year 2, time 2 is fine  ------

tapply(ABCD5_core_srs$mctq_sjl_calc, ABCD5_core_srs$eventname, summary)

# Extract 2yr
socialJL_abs <- ABCD5_core_srs %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, sjl_a = mctq_sjl_calc)

ABCD5_core_srs <- ABCD5_core_srs %>%
  left_join(socialJL_abs, by = "src_subject_id") %>%
  mutate(sjl_abs = case_when(
    eventname == "1_year_follow_up_y_arm_1" ~ sjl_a,
    eventname == "3_year_follow_up_y_arm_1" ~ mctq_sjl_calc,
    
    TRUE ~ NA_real_
  )) %>%
  dplyr::select(-sjl_a)

tapply(ABCD5_core_srs$sjl_abs, ABCD5_core_srs$eventname, summary)
tapply(ABCD5_core_srs$mctq_sjlrel_calc, ABCD5_core_srs$eventname, summary)
tapply(ABCD5_core_srs$mctq_sjl_calc, ABCD5_core_srs$eventname, summary)

ABCD5_core_srs %>%
  filter(eventname == "1_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, sjl_abs) %>%
  head(10)

ABCD5_core_srs %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, mctq_sjl_calc) %>%
  head(10)

tapply(ABCD5_core_srs$sjl_abs, ABCD5_core_srs$eventname, summary)


# --------------------- chronotype - replace time 1 with year 2, time 2 is fine  ------
tapply(ABCD5_core_srs$mctq_msfsc_correct, ABCD5_core_srs$eventname, summary)
table(ABCD5_core_srs$chronotype, ABCD5_core_srs$eventname)

# Extract 2yr data
chrono <- ABCD5_core_srs %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, chrono_2 = mctq_msfsc_correct)

ABCD5_core_srs <- ABCD5_core_srs %>%
  left_join(chrono, by = "src_subject_id") %>%
  mutate(chrono_msf = case_when(
    eventname == "1_year_follow_up_y_arm_1" ~ chrono_2,
    eventname == "3_year_follow_up_y_arm_1" ~ mctq_msfsc_correct,
    TRUE ~ NA_real_
  )) %>%
  dplyr::select(-chrono_2)

tapply(ABCD5_core_srs$chrono_msf, ABCD5_core_srs$eventname, summary)
tapply(ABCD5_core_srs$mctq_msfsc_correct, ABCD5_core_srs$eventname, summary)

ABCD5_core_srs %>%
  filter(eventname == "1_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, chrono_msf) %>%
  head(10)

ABCD5_core_srs %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, mctq_msfsc_correct) %>%
  head(10)

# --------------------- chronotype categorical - replace time 1 with year 2, time 2 is fine  ------
tapply(ABCD5_core_srs$chronotype, ABCD5_core_srs$eventname, summary)
table(ABCD5_core_srs$chronotype, ABCD5_core_srs$eventname)

chrono_categ <- ABCD5_core_srs %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, chrono_C = chronotype)

ABCD5_core_srs <- ABCD5_core_srs %>%
  left_join(chrono_categ, by = "src_subject_id") %>%
  mutate(chrono_cat = case_when(
    eventname == "1_year_follow_up_y_arm_1" ~ chrono_C,
    eventname == "3_year_follow_up_y_arm_1" ~ chronotype,
    TRUE ~ NA_character_
  )) %>%
  dplyr::select(-chrono_C)

ABCD5_core_srs$chrono_cat <- factor(ABCD5_core_srs$chrono_cat)
ABCD5_core_srs$chrono_cat <- relevel(ABCD5_core_srs$chrono_cat, ref = "Morning")

table(ABCD5_core_srs$chrono_cat, ABCD5_core_srs$eventname)
table(ABCD5_core_srs$chronotype, ABCD5_core_srs$eventname)

ABCD5_core_srs %>%
  filter(eventname == "1_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, chrono_cat) %>%
  head(10)

ABCD5_core_srs %>%
  filter(eventname == "2_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, chronotype) %>%
  head(10)

# var to use - vars replace with other eventname values were renamed to avoid confusion with real values
#  cbcl_scr_syn_external_t	    
#  cbcl_scr_syn_internal_t     
#  chronotype
#  chrono_cat
#  chrono_msf
# sjl
# srs
# sleep_disturb_cat
# SDS_total_adj 
# sex_base 
# PA 
# screens 
# cognition
#  interview_age              
#  SEP         
#  race.4level.p
#  site_id

# Define the follow-up time point to keep baseline (1 year) and follow up (3 year)
valid_timepoints <- c("1_year_follow_up_y_arm_1", "3_year_follow_up_y_arm_1")
ABCD5_core_srs_year13 <- ABCD5_core_srs %>%
  mutate(eventname = as.character(eventname)) %>%
  filter(eventname %in% valid_timepoints)%>%
  mutate(eventname = as.factor(eventname)) 

table(ABCD5_core_srs_year13$eventname)

ABCD5_core_srs_year13$eventname <- factor(ABCD5_core_srs_year13$eventname)
ABCD5_core_srs_year13$eventname <- relevel(ABCD5_core_srs_year13$eventname, ref = "1_year_follow_up_y_arm_1")

######################################################################################################################
#
# Research Question 1 - Is SRS at 10 years old associated with sleep disturbance - fixed timepoints (year 1 and year3)
#
######################################################################################################################

#  only 1 year follow-up data rows with sleep data only for year 1 no random effect because no repeated measures

#checks
sum(is.na(ABCD5_core_srs_year13$srs)) 
sum(is.na(ABCD5_core_srs_year13$sex_base)) 
ABCD5_core_srs_year13$site_id_l <- droplevels(ABCD5_core_srs_year13$site_id_l)
ABCD5_core_srs_year13$src_subject_id <- as.factor(ABCD5_core_srs_year13$src_subject_id)
ABCD5_core_srs_year13$sds_p_ss_total <- as.numeric(ABCD5_core_srs_year13$sds_p_ss_total)
ABCD5_core_srs_year13 <- ABCD5_core_srs_year13[!is.na(ABCD5_core_srs_year13$sds_p_ss_total), ]

length(ABCD5_core_srs_year13$sds_p_ss_total)
summary(ABCD5_core_srs_year13$SDS_total_adj)

length(ABCD5_core_srs_year13$ssrs_p_ss_sum)
length(ABCD5_core_srs_year13$eventname)
length(ABCD5_core_srs_year13$sex)
summary(ABCD5_core_srs_year13$sds_p_ss_total)

ABCD5_core_srs_year13$sleepdisturb1_p <- as.numeric(ABCD5_core_srs_year13$sleepdisturb1_p)  # Ensure numeric
ABCD5_core_srs_year13$ssrs_p_ss_sum <- as.numeric(ABCD5_core_srs_year13$ssrs_p_ss_sum)  # Ensure numeric
table(ABCD5_core_srs_year13$sex_base, ABCD5_core_srs_year13$eventname, useNA = "always")

# centre srs as used in interaction term
ABCD5_core_srs_year13$srs_c <- scale(ABCD5_core_srs_year13$srs, center = TRUE, scale = FALSE)

# MM with nested random effects
model_gaussian1 <-  glmmTMB(
  sds_p_ss_total ~ 
    srs_c + sex_base  + race.4level.p + eventname +
    (1 | related/src_subject_id),
  family = gaussian(link = "identity"),
  data = ABCD5_core_srs_year13
)

summary(model_gaussian1)

model_gaussian2 <-  glmmTMB(
  sds_p_ss_total ~ 
    srs_c   + sex_base  + race.4level.p + eventname +
    site_id_l     + SEP    +   chrono_cat  + PA + screens +
    (1 | related/src_subject_id),
  
  family = gaussian(link = "identity"),
  data = ABCD5_core_srs_year13
)

model_gaussian3 <- glmmTMB(
  sds_p_ss_total  ~ 
    srs_c  + sex_base  + race.4level.p + 
    site_id_l     + SEP    +      PA + screens +
    cbcl_scr_syn_external_t	+ cbcl_scr_syn_internal_t  + eventname  + chrono_cat*srs_c+
    (1 | related/src_subject_id)
  # (1|src_subject_id  )
  ,
  family = gaussian(link = "identity"),
  data = ABCD5_core_srs_year13
)

summary(model_gaussian3)

# checking for colinearity
lm_model <- lm(sds_p_ss_total ~ 
                 srs_c  + sex_base  + race.4level.p + 
                 site_id_l     + SEP    +   chrono_cat  + PA + screens +
                 cbcl_scr_syn_external_t	+ cbcl_scr_syn_internal_t  + cognition + eventname ,
               data = ABCD5_core_srs_year13)  

# Compute VIFs
vif_values <- vif(lm_model)

# make regression table
tab_model(model_gaussian1, model_gaussian2, model_gaussian3) 



#  get standaridsed betas for forest plot

cont_vars <- c("srs_c", "SEP", "PA", "screens", "cbcl_scr_syn_external_t", "cbcl_scr_syn_internal_t")


ABCD5_core_srs_year13_std <- ABCD5_core_srs_year13

model_gaussian3_std <- glmmTMB(
  sds_p_ss_total ~ 
    srs_c + sex_base + race.4level.p + 
    site_id_l + SEP + PA + screens + 
    cbcl_scr_syn_external_t + cbcl_scr_syn_internal_t + eventname + chrono_cat * srs_c +
    (1 | src_subject_id),
  family = gaussian(link = "identity"),
  data = ABCD5_core_srs_year13_std
)


#=====================================================================================================================================
#
# Tables and descriptive statistics
#
#=====================================================================================================================================

#make tables
my.render.cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=4), c("","Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))}
my.render.cat <- function(x) {c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))}
my.render.NP_cont <- function(x) {with(stats.apply.rounding(stats.default(x), digits=0), c("","Median (IQR)"=sprintf("%s (&plusmn; %s)", MEDIAN, IQR)))}

# format continuous vars with 95% CI
my.render.cont = function(x) {c("", "mean (95% CI)"=sprintf("%s (%s, %s)",
                                                            round(stats.default(x)$MEAN,2),
                                                            round(stats.default(x)$MEAN-qt(0.975,stats.default(x)$N-1)*stats.default(x)$SD/sqrt(stats.default(x)$N),2),
                                                            round(stats.default(x)$MEAN+qt(0.975,stats.default(x)$N-1)*stats.default(x)$SD/sqrt(stats.default(x)$N),2)
))
}

# Table 1   Demography stratify 
---------------------------------------------------------------------------------------------------------------------
  summary(ABCD5_core_srs_year13$sds_p_ss_total)

# Create the table of demography
table1(~ sex_base  + interview_age +  race.4level.p + SEP + sds_p_ss_total  + sleepdisturb1_p+ + chrono_cat  + PA + screens +
         
         + srs_c +     cbcl_scr_syn_external_t	+ cbcl_scr_syn_internal_t  
       | eventname,
       data = ABCD5_core_srs_year13,
       overall = FALSE,  
       render.continuous = my.render.cont, 
       render.categorical = my.render.cat)


## Create Table 1 stratified  to get p-values
vars<-c("sex_base"  , "interview_age" ,  "race.4level.p" , "SEP" , "sds_p_ss_total"  , "sleepdisturb1_p" , "chrono_cat"  , "PA" , "screens" ,
        
        "srs_c" ,     "cbcl_scr_syn_external_t"	, "cbcl_scr_syn_internal_t" 
)

tableOne <- CreateTableOne(vars = vars,
                           
                           strata = c("eventname"), 
                           data = ABCD5_core_srs_year13, 
                           #test = FALSE, 
                           factorVars = c())
summary (tableOne)


# Table of demography stratified by srs quartiles at year 1 only - this is our baseline timepont (year1) - year 3 is "follow up"

subset_1year <- ABCD5_core_srs_year13[ABCD5_core_srs_year13$eventname == "1_year_follow_up_y_arm_1", ]

# quartiles of SRS scores
quartiles <- quantile(subset_1year$ssrs_p_ss_sum, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Create a new variable in the original dataset using these quartiles
ABCD5_core_srs_year13$srs_quartile_1year <- cut(
  ABCD5_core_srs_year13$ssrs_p_ss_sum,
  breaks = quartiles,
  include.lowest = TRUE,
  labels = c("Q1", "Q2", "Q3", "Q4")
)

table1(
  ~ sex_base + interview_age + race.4level.p + SEP + ssrs_p_ss_sum + sleepdisturb1_p + chrono_cat + PA + screens +
    cbcl_scr_syn_external_t + cbcl_scr_syn_internal_t|srs_quartile_1year,
  data = ABCD5_core_srs_year13[ABCD5_core_srs_year13$eventname == "1_year_follow_up_y_arm_1" &
                                 !is.na(ABCD5_core_srs_year13$srs_quartile_1year), 
  ],
  overall = FALSE,  
  render.continuous = my.render.cont, 
  render.categorical = my.render.cat
)


## Create Table 1 stratified  to get p-values
vars<-c("sex_base" , "interview_age" , "race.4level.p" , "SEP" , "ssrs_p_ss_sum" , "sleepdisturb1_p" , "chrono_cat" , "PA" , "screens" ,
        "cbcl_scr_syn_external_t" , "cbcl_scr_syn_internal_t")

mydata = ABCD5_core_srs_year13[ABCD5_core_srs_year13$eventname == "1_year_follow_up_y_arm_1" & is.na(ABCD5_core_srs_year13$srs_quartile_1year),] 
tableOne <- CreateTableOne(vars = vars,
                           
                           strata = c("srs_quartile_1year"), 
                           data = mydata,
                           #test = FALSE, 
                           factorVars = c())

summary (tableOne)


#####################################################################################
#
# Plots for Sleep Disruption
#
#======================================================================================



# Fig 1: Interaction Plot with Predicted Values ============================================================================

# Generate predicted values for the interaction - SRS × Chronotype Interaction on Sleep Disruption, Mixed-effects model predictions with 95% confidence intervals
pred_data <- ggpredict(model_gaussian3, terms = c("srs_c [all]", "chrono_cat"))

p1 <- ggplot(pred_data, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, color = NA) +
  labs(
    x = "SRS Score (centered)",
    y = "Predicted Sleep Disruption Score",
    color = "Chronotype",
    fill = "Chronotype",
    title = NULL,
    subtitle = NULL
  ) +
  theme_pubr(base_size = 16) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.title.x = element_text(size = 16, margin = margin(t = 15)),
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),
    axis.text.x = element_text(size = 14, margin = margin(t = 5)),
    axis.text.y = element_text(size = 14, margin = margin(r = 5)),
    
    panel.border = element_blank(),
    panel.background = element_blank(),
    
    # Remove gridlines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "black", size = 0.5)
  ) +
  scale_color_manual(values = c(
    "Morning" = "#2E86AB", 
    "Intermediate" = "#A23B72", 
    "Evening" = "#F18F01"
  )) +
  scale_fill_manual(values = c(
    "Morning" = "#2E86AB", 
    "Intermediate" = "#A23B72", 
    "Evening" = "#F18F01"
  ))

ggsave("interaction_plot_sds.png", p1, width = 6, height = 6, dpi = 300)
ggsave("interaction_plot_sds.svg", p1, width = 6, height = 6, dpi = 300)



# Fig 2 Effect Size Visualization - Effect Size Comparison Across Chronotypes - SRS effect on Sleep Disruption by Chronotype
#============================================================================
slope_data <- data.frame(
  Chronotype = c("Morning", "Intermediate", "Evening"),
  SRS_slope = c(0.161924,  # Main effect (Morning reference)
                0.161924 + 0.145876,  # Main + Intermediate interaction
                0.161924 + 0.427922), # Main + Evening interaction
  SE = c(0.019946,  # SE for main effect
         sqrt(0.019946^2 + 0.026958^2),  # Combined SE (approximate)
         sqrt(0.019946^2 + 0.097119^2))  # Combined SE (approximate)
)

# Calculate confidence intervals
slope_data$Lower <- slope_data$SRS_slope - 1.96 * slope_data$SE
slope_data$Upper <- slope_data$SRS_slope + 1.96 * slope_data$SE

p2 <- ggplot(slope_data, aes(x = Chronotype, y = SRS_slope, fill = Chronotype)) +
  geom_col(alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, color = Chronotype),
                width = 0, size = 1) +
  labs(
    x = "Chronotype",
    y = "SRS Slope Coefficient",
    title = ""
  ) +
  
  theme_pubr(base_size = 16) +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.5),
    axis.title.x = element_text(size = 16, margin = margin(t = 15)),  # move x-axis title down
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),  # move y-axis title left
    axis.text.x = element_text(size = 14, margin = margin(t = 5)),    # smaller tick labels, margin tweaks
    axis.text.y = element_text(size = 14, margin = margin(r = 5)),
    strip.background = element_blank(),
    strip.text = element_blank()) +
  
  scale_fill_manual(values = c(
    "Morning" = "#2E86AB", 
    "Intermediate" = "#A23B72", 
    "Evening" = "#F18F01"
  )) +
  scale_color_manual(values = c(
    "Morning" = "#2E86AB", 
    "Intermediate" = "#A23B72", 
    "Evening" = "#F18F01"
  ))


ggsave("effect_sizes_srs.png", p2, 
       width = 6, height = 6, dpi = 300)

ggsave("effect_sizes_srs.svg", p2, 
       width = 6, height = 6, dpi = 300)



# Fig 2  Sleep Disturbance by SRS Quartile - Associateion between SRS and sleep disruption persists at  follow up 3 yr
#===========================================================================================================================

srs_1yr <- ABCD5_core_srs_year13 %>%
  filter(eventname == "1_year_follow_up_y_arm_1") %>%
  dplyr::select(src_subject_id, srs_quartile_1year_1yr = srs_quartile_1year)

ABCD5_core_srs_year13 <- ABCD5_core_srs_year13 %>%
  left_join(srs_1yr, by = "src_subject_id") %>%
  mutate(
    srs_quartile_1year = if_else(
      eventname == "3_year_follow_up_y_arm_1",
      srs_quartile_1year_1yr,
      srs_quartile_1year
    )
  ) %>%
  dplyr::select(-srs_quartile_1year_1yr)

table(ABCD5_core_srs_year13$srs_quartile_1year, ABCD5_core_srs_year13$eventname)
head(c(ABCD5_core_srs_year13$srs_quartile_1year, ABCD5_core_srs_year13$src_subject_id,ABCD5_core_srs_year13$eventname))

# Prepare data with combined factor for colors
ABCD5_core_srs_year13 <- ABCD5_core_srs_year13 %>%
  filter(!is.na(srs_quartile_1year)) %>%
  mutate(
    quartile_event = paste0(srs_quartile_1year, "_", eventname)
  )

# Define quartile gradients for each eventname
quartile_colors <- c(
  # Year 1 gradient - Blue shades
  "Q1_1_year_follow_up_y_arm_1" = "#B2D9E8",
  "Q2_1_year_follow_up_y_arm_1" = "#7DBFD6",
  "Q3_1_year_follow_up_y_arm_1" = "#4BA8C4",
  "Q4_1_year_follow_up_y_arm_1" = "#2E86AB",
  
  # Year 3 gradient - Magenta shades (original)
  "Q1_3_year_follow_up_y_arm_1" = "#E2B8C4",
  "Q2_3_year_follow_up_y_arm_1" = "#D17894",
  "Q3_3_year_follow_up_y_arm_1" = "#B24569",
  "Q4_3_year_follow_up_y_arm_1" = "#A23B72"
)

p <- ggplot(ABCD5_core_srs_year13, aes(x = srs_quartile_1year, y = sds_p_ss_total,
                                       fill = quartile_event, color = quartile_event)) +
  geom_boxplot(outlier.shape = NA, width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = quartile_colors) +
  scale_color_manual(values = quartile_colors) +
  coord_cartesian(ylim = c(30, 65)) +
  facet_wrap(~eventname, nrow = 1) +
  labs(
    x = "Social Reponsiveness Score (Quartile)",
    y = "Sleep Disturbance Score"
  ) +
  theme_pubr(base_size = 16) +
  theme(
    
    axis.title.x = element_text(size = 16, margin = margin(t = 15)),  # move x-axis title down
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),  # move y-axis title left
    axis.text.x = element_text(size = 14, margin = margin(t = 5)),    # smaller tick labels, margin tweaks
    axis.text.y = element_text(size = 14, margin = margin(r = 5)),
    strip.background = element_blank(),
    strip.text = element_blank(),   # Remove facet labels
    panel.spacing = unit(2, "lines"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) +
  guides(fill = "none", color = "none")  # Hide default legends


# Save the plot
ggsave("Boxplot_SDS_by_SRS_Quartile.svg", plot = p,
       width = 10, height = 6, dpi = 300)
ggsave("Boxplot_SDS_by_SRS_Quartile.png", plot = p,
       width = 10, height = 6, dpi = 300)


# Fig 3  Forest plot showing standardized beta estimates and 95% CI for association between SRS and sleep disruption 
#===========================================================================================================================

coef_data_clean <- broom.mixed::tidy(model_gaussian3_std, effects = "fixed") 

coef_data_clean <- coef_data_clean %>%
  filter(
    term != "(Intercept)",                         # remove intercept
    !grepl("site_id", term),                      # remove site_id terms
    !grepl("^sd_", term),                         # remove random effect SDs
    term != "srs_c:chrono_catEvening",            # remove interaction term
    term != "srs_c:chrono_catIntermediate"        # remove interaction term
  )

ref_row1 <- data.frame(
  estimate = 0,
  std.error = NA,
  term = "chrono_catMorning",
  term_label = "Chronotype: Morning"
)

ref_row2 <- data.frame(
  estimate = 0,
  std.error = NA,
  term = "sex_basemale",
  term_label = "Sex: Male")  

ref_row3 <- data.frame(
  estimate = 0,
  std.error = NA,
  term = "race.4level.pWhite",
  term_label = "Sex: White"
)

ref_row4 <- data.frame(
  estimate = 0,
  std.error = NA,
  term = "eventname1_year_follow_up_y_arm_1",
  term_label = "Study Wave: Age 11 (Ref)"
) 

coef_data_clean <- bind_rows(coef_data_clean, ref_row1, ref_row2, ref_row3, ref_row4)

#SRS	Blue	"#2E86AB" #blue
#Sex	Magenta / Dark Pink	"#A23B72" #pink
#SEP	Orange	"#F18F01" #orange
#Physical Activity	Dark Green	"#006400" #green
#Screens	Red-Pink (like Crimson)	"#DA4167" #crimson
#Externalising	Purple	"#8F3985" #purple
#Internalising	Teal	"#00A6A6" #teal
#Timepoint	Bright Orange-Red	"#F46036" #red
#Chronotype	Cyan-Blue	"#1B9AAA" # cyna
#SRS x Chronotype	Deep Red / Burgundy	"#9A031E" # burgundy
#Other	Grey	"#808080" #grey

custom_labels <- c(
  "srs_c" = "Social Responsiveness Score",
  "sex_basefemale" = "Sex: Female",
  "race.4level.pBlack" =  "Ethnicity: Black"  ,            
  "race.4level.pAsian" =   "Ethnicity: Asian",            
  "race.4level.pOther/Mixed"= "Ethnicity: Mixed",
  "SEP"   =  "Socioeconomic Position",                          
  "PA"  = "Physical Activity",  
  "screens" = "Screen Time" ,                          
  "cbcl_scr_syn_external_t"= "Externalising Behaviours" ,          
  "cbcl_scr_syn_internal_t"=          "Internalising Behaviours",
  "eventname3_year_follow_up_y_arm_1" = "Study Wave: Age 13",
  "chrono_catEvening"  =               "Chronotype: Evening",
  "chrono_catIntermediate"=           "Chronotype: Intermediate",
  "chrono_catMorning"= "Chronotype: Morning (ref)",
  "sex_basemale" = "Sex: Male (ref)",
  "eventname1_year_follow_up_y_arm_1" = "Study Wave: Age 11 (ref)",
  "race.4level.pWhite" = "Ethnicity: White (ref)"
)

p <- ggplot(coef_data_clean, aes(x = estimate, y = term)) +
  geom_errorbarh(
    aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error),
    height = 0.2, color = "#2E86AB") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  
  geom_point(
    shape = 21,            # Hollow point
    size = 4,              # Slightly larger for visibility
    fill = "white",        # Transparent center
    color = "#2E86AB",     # Outline color
    stroke = 1             # Thickness of border
  )+
  
  scale_y_discrete(labels = custom_labels) +
  
  labs(
    x = "Standardised Estimate (95% CI)",
    y = NULL
  ) +
  theme_pubr(base_size = 16) +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", size = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16, margin = margin(t = 15)),
    axis.text.x = element_text(size = 14, margin = margin(t = 5)),
    axis.text.y = element_text(size = 14, margin = margin(r = 5))
  )

ggsave("forestplot.png", plot = p, width = 6, height = 6, dpi = 300)
ggsave("forestplot.svg", plot = p, width = 6, height = 6, dpi = 300)



#############################################################################################################################
#
#  Repeat for social jetlag 
#
############################################################################################################################


str(ABCD5_core_srs_year13$sjl)
hist(ABCD5_core_srs_year13$sjl)png("sjl_density_by_year.png", width = 1200, height = 600)
png("sjl_density_by_year.png", width = 1200, height = 600)

# Set up plotting area: 1 row, 2 columns
par(mfrow = c(1, 2))

# Filter SJL data for each year
sjl_year1 <- ABCD5_core_srs_year13$sjl[ABCD5_core_srs_year13$eventname == "1_year_follow_up_y_arm_1"]
sjl_year3 <- ABCD5_core_srs_year13$sjl[ABCD5_core_srs_year13$eventname == "3_year_follow_up_y_arm_1"]

# Common x-axis limits for comparability
xlim_range <- range(c(sjl_year1, sjl_year3), na.rm = TRUE)

# Density plot for Year 1
plot(
  density(sjl_year1, na.rm = TRUE), 
  main = "Density of SJL - Baseline (Year 1)", 
  xlab = "Social Jetlag (hours)", 
  col = "blue", 
  lwd = 2, 
  xlim = xlim_range
)

# Density plot for Year 3
plot(
  density(sjl_year3, na.rm = TRUE), 
  main = "Density of SJL - Year 3 Follow-up", 
  xlab = "Social Jetlag (hours)", 
  col = "darkgreen", 
  lwd = 2, 
  xlim = xlim_range
)

dev.off()


#centreing
ABCD5_core_srs_year13$sjl_c <- scale(ABCD5_core_srs_year13$sjl, center = TRUE, scale = FALSE)
ABCD5_core_srs_year13sjl_abs_c <- scale(ABCD5_core_srs_year13_std$sjl_abs, center = TRUE, scale = FALSE)
ABCD5_core_srs_year13$srs_c <- scale(ABCD5_core_srs_year13$srs, center = TRUE, scale = FALSE)

# include sleep duration
summary(ABCD5_core_srs_year13$sleepdisturb1_p)
ABCD5_core_srs_year13$sleep_disturb_cat

table(ABCD5_core_srs_year13$sleepdisturb1_p)

ABCD5_core_srs_year13$sleepdisturb1_p <- factor(ABCD5_core_srs_year13$sleepdisturb1_p,
                                                levels=1:5,
                                                labels= c( "9-11h",
                                                           "8-9h", 
                                                           "7-8h",
                                                           "5-7h",
                                                           "<5h")
)

# MM with year1 and year3
model_gaussian1_sjl <-  glmmTMB(
  sjl_abs ~ 
    srs_c + sex_base  + race.4level.p + eventname  +
    (1 | related/src_subject_id),
  family = gaussian(link = "identity"),
  data = ABCD5_core_srs_year13
)

summary(model_gaussian1_sjl )


model_gaussian2_sjl <-  glmmTMB(
  sjl_abs  ~ 
    srs_c   + sex_base  + race.4level.p + eventname +
    site_id_l     + SEP    +   chrono_cat  + PA + screens + chrono_cat*srs_c + sleepdisturb1_p +
    (1 | related/src_subject_id),
  
  family = gaussian(link = "identity"),
  data = ABCD5_core_srs_year13
)


summary(model_gaussian2_sjl)

model_gaussian3_sjl <- glmmTMB(
  sjl_abs  ~ 
    srs_c  + sex_base  + race.4level.p + eventname  +
    site_id_l     + SEP    +  chrono_cat +    PA + screens + chrono_cat*srs_c+ sleepdisturb1_p +
    cbcl_scr_syn_external_t	+ cbcl_scr_syn_internal_t +
    
    sleepdisturb1_p +
    (1 | related/src_subject_id)
  # (1|src_subject_id  )
  ,
  family = gaussian(link = "identity"),
  data = ABCD5_core_srs_year13
)

summary(model_gaussian3_sjl )

tab_model(model_gaussian1_sjl,model_gaussian2_sjl,model_gaussian3_sjl)


##-----------------------------    Plots for SJL --------------------------------------------------------------------------


# Generate predicted values for the interaction - SRS × Chronotype Interaction on SJL model predictions with 95% confidence intervals
pred_data <- ggpredict(model_gaussian3_sjl, terms = c("srs_c [all]", "chrono_cat"))

p1 <- ggplot(pred_data, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, color = NA) +
  labs(
    x = "SRS Score (centered)",
    y = "Predicted Social Jetlag",
    color = "Chronotype",
    fill = "Chronotype",
    title = NULL,
    subtitle = NULL
  ) +
  theme_pubr(base_size = 16) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.title.x = element_text(size = 16, margin = margin(t = 15)),
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),
    axis.text.x = element_text(size = 14, margin = margin(t = 5)),
    axis.text.y = element_text(size = 14, margin = margin(r = 5)),
    
    panel.border = element_blank(),
    panel.background = element_blank(),
    
    # Remove gridlines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "black", size = 0.5)
  ) +
  scale_color_manual(values = c(
    "Morning" = "#2E86AB", 
    "Intermediate" = "#A23B72", 
    "Evening" = "#F18F01"
  )) +
  scale_fill_manual(values = c(
    "Morning" = "#2E86AB", 
    "Intermediate" = "#A23B72", 
    "Evening" = "#F18F01"
  ))

ggsave("interaction_plot_sjl.png", p1, width = 6, height = 6, dpi = 300)
ggsave("interaction_plot_sjl.svg", p1, width = 6, height = 6, dpi = 300)

# these are not useful plots - evening chronotypes have high SJL regardless of SRS - explains why interaction is not significant for them.  Initial significance of SRS on SJL was because of chronotype, high srs tend to be evening



# Fig 2 Effect Size Visualization - Effect Size Comparison Across Chronotypes - SRS effect on Sleep Disruption by Chronotype
#============================================================================
slope_data <- data.frame(
  Chronotype = c("Morning", "Intermediate", "Evening"),
  SJL_slope = c(-0.0046743,  # Main effect (Morning reference)
                -0.0046743 + 0.0150732,  # Main + Intermediate interaction
                -0.0046743 + 0.0225821), # Main + Evening interaction
  SE = c(0.0033356,  # SE for main effect
         sqrt(0.0033356^2 + 0.0047946^2),  # Combined SE (approximate)
         sqrt(0.0033356^2 + 0.0179171^2))  # Combined SE (approximate)
)

# Calculate confidence intervals
slope_data$Lower <- slope_data$SJL_slope - 1.96 * slope_data$SE
slope_data$Upper <- slope_data$SJL_slope + 1.96 * slope_data$SE

p2 <- ggplot(slope_data, aes(x = Chronotype, y = SJL_slope, fill = Chronotype)) +
  geom_col(alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper, color = Chronotype),
                width = 0, size = 1) +
  labs(
    x = "Chronotype",
    y = "SJL Slope Coefficient",
    title = ""
  ) +
  
  theme_pubr(base_size = 16) +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.5),
    axis.title.x = element_text(size = 16, margin = margin(t = 15)),  # move x-axis title down
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),  # move y-axis title left
    axis.text.x = element_text(size = 14, margin = margin(t = 5)),    # smaller tick labels, margin tweaks
    axis.text.y = element_text(size = 14, margin = margin(r = 5)),
    strip.background = element_blank(),
    strip.text = element_blank()) +
  
  scale_fill_manual(values = c(
    "Morning" = "#2E86AB", 
    "Intermediate" = "#A23B72", 
    "Evening" = "#F18F01"
  )) +
  scale_color_manual(values = c(
    "Morning" = "#2E86AB", 
    "Intermediate" = "#A23B72", 
    "Evening" = "#F18F01"
  ))


ggsave("effect_sizes_sjl.png", p2, 
       width = 6, height = 6, dpi = 300)

ggsave("effect_sizes_sjl.svg", p2, 
       width = 6, height = 6, dpi = 300)




##  Plot of the assoc between chronotype and SJL at different timepoints, not very useful, evening is a huge predictor but high variation and low sample size


# Filter out NAs and set chronotype order
plot_data <- ABCD5_core_srs_year13 %>%
  filter(!is.na(chrono_cat), !is.na(sjl_abs)) %>%
  mutate(chrono_cat = factor(chrono_cat, levels = c("Morning", "Intermediate", "Evening")))

chronotype_colors <- c("Morning" = "#2E86AB", 
                       "Intermediate" = "#A23B72", 
                       "Evening" = "#F18F01")

p <- ggplot(plot_data, aes(x = chrono_cat, y = sjl_abs,
                           fill = chrono_cat, color = chrono_cat)) +
  geom_boxplot(outlier.shape = NA, width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = chronotype_colors) +
  scale_color_manual(values = chronotype_colors) +
  coord_cartesian(ylim = c(0, 12)) +
  facet_wrap(~eventname, nrow = 1) +
  labs(
    x = "Chronotype",
    y = "Social Jet Lag (Absolute Hours)"
  ) +
  theme_pubr(base_size = 16) +
  theme(
    axis.title.x = element_text(size = 16, margin = margin(t = 15)),
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),
    axis.text.x = element_text(size = 14, margin = margin(t = 5), angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14, margin = margin(r = 5)),
    strip.background = element_blank(),
    strip.text = element_blank(),   
    panel.spacing = unit(2, "lines"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) +
  guides(fill = "none", color = "none") 

# Save the plot
ggsave("Boxplot_SJL_by_Chronotype.svg", plot = p,
       width = 10, height = 6, dpi = 300)
ggsave("Boxplot_SJL_by_Chronotype.png", plot = p,
       width = 10, height = 6, dpi = 300)
