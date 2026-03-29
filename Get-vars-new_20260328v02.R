# title:          "Sleep disruption as an endophenotype of Autism"
# purpose:        "Data cleaning pipeline — AAB FamilySleeps"
# date:           "28/03/2026"
# author:         "Mailton Vasconcelos"


# ---- Packages ----------------------------------------------------------------
library(tidyr)
library(dplyr)
library(readxl)
library(visdat)
library(lubridate)
library(stringr)
library(purrr)

# ---- Load the data -----------------------------------------------------------
AAB_data     <- read.csv("AustralianAutismBiob-AccessRequest2024047_DATA_2025-05-14_1023.csv",
                         stringsAsFactors = FALSE)
AAB_safe_copy <- AAB_data


# ==============================================================================
# BLOCK 1: Demographic variables
# ==============================================================================

# ---- Participant type --------------------------------------------------------

## Inspect
table(AAB_data$participant_type, useNA = "always")
unique(AAB_data$participant_type)
summary(AAB_data$participant_type)
str(AAB_data$participant_type)

## Recode to factor
AAB_data <- AAB_data %>%
  filter(!is.na(id), !is.na(participant_type)) %>%
  mutate(
    participant_type_f = factor(participant_type,
                                levels = 1:6,
                                labels = c("Proband", "ASD-Q", "Sibling",
                                           "Comparison", "Mother", "Father"))
  )

table(AAB_data$participant_type_f, useNA = "always")


# ---- Age (children) ----------------------------------------------------------
# Sources used in order of priority:
# 1. proforma_dob + proforma_doa
# 2. ados_dob + ados_doe
# 3. ados_dob + vabs_testdate
# 4. vabs_dob + acl_childsample_date
# 5. vabs_dob + vabs_testdate
# 6. acl_child_dob + acl_proforma_date
# 7. acl_child_dob + acl_cog_date
# 8. acl_child_dob + acl_childsample_date

str(AAB_data$age_at_assessment)
summary(AAB_data$age_at_assessment)

names(AAB_data) %>%
  grep("date|dob|doa|doe|birth|assess", ., value = TRUE, ignore.case = TRUE)

AAB_data <- AAB_data %>%
  mutate(
    # Parse all date fields
    proforma_dob_d         = ymd(proforma_dob),
    proforma_doa_d         = ymd(proforma_doa),
    ados_dob_d             = ymd(ados_dob),
    ados_doe_d             = ymd(ados_doe),
    vabs_dob_d             = ymd(vabs_dob),
    vabs_testdate_d        = ymd(vabs_testdate),
    acl_child_dob_d        = ymd(acl_child_dob),
    acl_childsample_date_d = ymd(acl_childsample_date),
    acl_proforma_date_d    = ymd(acl_proforma_date),
    acl_cog_date_d         = ymd(acl_cog_date),
    
    # Recover age in priority order
    age_at_assessment_clean = case_when(
      !is.na(proforma_dob_d)  & !is.na(proforma_doa_d)         ~
        as.numeric(interval(proforma_dob_d,  proforma_doa_d)         / years(1)),
      !is.na(ados_dob_d)      & !is.na(ados_doe_d)             ~
        as.numeric(interval(ados_dob_d,      ados_doe_d)             / years(1)),
      !is.na(ados_dob_d)      & !is.na(vabs_testdate_d)        ~
        as.numeric(interval(ados_dob_d,      vabs_testdate_d)        / years(1)),
      !is.na(vabs_dob_d)      & !is.na(acl_childsample_date_d) ~
        as.numeric(interval(vabs_dob_d,      acl_childsample_date_d) / years(1)),
      !is.na(vabs_dob_d)      & !is.na(vabs_testdate_d)        ~
        as.numeric(interval(vabs_dob_d,      vabs_testdate_d)        / years(1)),
      !is.na(acl_child_dob_d) & !is.na(acl_proforma_date_d)   ~
        as.numeric(interval(acl_child_dob_d, acl_proforma_date_d)    / years(1)),
      !is.na(acl_child_dob_d) & !is.na(acl_cog_date_d)        ~
        as.numeric(interval(acl_child_dob_d, acl_cog_date_d)         / years(1)),
      !is.na(acl_child_dob_d) & !is.na(acl_childsample_date_d) ~
        as.numeric(interval(acl_child_dob_d, acl_childsample_date_d) / years(1)),
      TRUE ~ NA_real_
    ),
    
    # Drop intermediate parsed date columns
    proforma_dob_d         = NULL,
    proforma_doa_d         = NULL,
    ados_dob_d             = NULL,
    ados_doe_d             = NULL,
    vabs_dob_d             = NULL,
    vabs_testdate_d        = NULL,
    acl_child_dob_d        = NULL,
    acl_childsample_date_d = NULL,
    acl_proforma_date_d    = NULL,
    acl_cog_date_d         = NULL
  )

## Verification
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  summarise(
    n_total         = n(),
    n_recovered     = sum(!is.na(age_at_assessment_clean)),
    n_still_missing = sum(is.na(age_at_assessment_clean)),
    min             = min(age_at_assessment_clean,  na.rm = TRUE),
    max             = max(age_at_assessment_clean,  na.rm = TRUE),
    mean            = mean(age_at_assessment_clean, na.rm = TRUE),
    sd              = sd(age_at_assessment_clean,   na.rm = TRUE)
  )


# ---- Age (parents) -----------------------------------------------------------

parent_dob <- AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  select(acl_mother_id, acl_father_id, fhq_b9, fhq_b24_dob, proforma_doa) %>%
  pivot_longer(cols      = c(acl_mother_id, acl_father_id),
               names_to  = "parent_type",
               values_to = "parent_id") %>%
  mutate(
    parent_dob = case_when(
      parent_type == "acl_mother_id" ~ ymd(fhq_b9),
      parent_type == "acl_father_id" ~ ymd(fhq_b24_dob)
    )
  ) %>%
  filter(!is.na(parent_id)) %>%
  distinct(parent_id, .keep_all = TRUE) %>%
  select(parent_id, parent_dob, assessment_date = proforma_doa)

AAB_data <- AAB_data %>%
  left_join(parent_dob, by = c("id" = "parent_id")) %>%
  mutate(
    age_parent = as.numeric(interval(parent_dob, assessment_date) / years(1))
  )

rm(parent_dob)


# ---- Unified age variable ----------------------------------------------------

AAB_data <- AAB_data %>%
  mutate(
    age = case_when(
      participant_type %in% c(1, 2, 3, 4) ~ age_at_assessment_clean,
      participant_type %in% c(5, 6)        ~ age_parent
    )
  )

## Final check across all groups
AAB_data %>%
  group_by(participant_type_f) %>%
  summarise(
    n         = n(),
    n_age     = sum(!is.na(age)),
    n_missing = sum(is.na(age)),
    min       = min(age, na.rm = TRUE),
    max       = max(age, na.rm = TRUE),
    mean      = mean(age, na.rm = TRUE),
    sd        = sd(age, na.rm = TRUE)
  )

## Drop intermediate age/date columns
AAB_data <- AAB_data %>%
  select(-age_at_assessment_clean, -age_parent, -parent_dob, -assessment_date)


# ---- Sex ---------------------------------------------------------------------

AAB_data %>%
  group_by(participant_type_f) %>%
  count(proforma_sex)

names(AAB_data) %>% grep("sex|gender", ., value = TRUE, ignore.case = TRUE)

# ados_gender coding is reversed relative to proforma_sex:
# ados_gender 1 = Female, 2 = Male  |  proforma_sex 1 = Male, 2 = Female
AAB_data <- AAB_data %>%
  mutate(
    ados_gender_recoded = case_when(
      ados_gender == 1 ~ 2,
      ados_gender == 2 ~ 1,
      TRUE             ~ NA_real_
    )
  )

## Derive sex: proforma_sex → fhq_b2 → ados_gender_recoded
AAB_data <- AAB_data %>%
  mutate(
    sex = case_when(
      !is.na(proforma_sex)             ~ proforma_sex,
      !is.na(fhq_b2) & fhq_b2 != 999  ~ fhq_b2,
      !is.na(ados_gender_recoded)      ~ ados_gender_recoded,
      TRUE                             ~ NA_real_
    ),
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))
  )

## Assign sex deterministically for enrolled parents
AAB_data <- AAB_data %>%
  mutate(
    sex = case_when(
      !is.na(sex)           ~ sex,
      participant_type == 5 ~ factor("Female", levels = c("Male", "Female")),
      participant_type == 6 ~ factor("Male",   levels = c("Male", "Female")),
      TRUE                  ~ NA
    )
  )

## Final check
AAB_data %>%
  group_by(participant_type_f) %>%
  summarise(
    n_male    = sum(sex == "Male",   na.rm = TRUE),
    n_female  = sum(sex == "Female", na.rm = TRUE),
    n_missing = sum(is.na(sex))
  )

AAB_data$ados_gender_recoded <- NULL


# ---- Site --------------------------------------------------------------------

AAB_data <- AAB_data %>%
  mutate(
    site = case_when(
      substr(as.character(id), 1, 2) == "11" ~ 1,  # WA  — Telethon Kids Institute
      substr(as.character(id), 1, 2) == "22" ~ 2,  # VIC — La Trobe University
      substr(as.character(id), 1, 2) == "33" ~ 3,  # NSW — University of New South Wales
      substr(as.character(id), 1, 2) == "44" ~ 4   # QLD — Lady Cilento Children's Hospital
    ),
    site = factor(site, levels = 1:4, labels = c("WA", "VIC", "NSW", "QLD"))
  )

AAB_data %>%
  group_by(site, participant_type_f) %>%
  count() %>%
  pivot_wider(names_from = participant_type_f, values_from = n)


# ==============================================================================
# BLOCK 2: Diagnosis confirmation and grouping
# ==============================================================================
# fhq_h1 codes: 1=Autism, 2=PDD-NOS, 3=Asperger, 4=Other, 5=999 (entry error)

AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  count(fhq_h1, useNA = "always")

## IDs with code 4 (Other) but no valid ASD diagnosis on review of fhq_h1_detail
no_asd_ids <- c(1101589, 2202623, 2202629, 2202630, 2202631,
                2202641, 2202651, 2202664, 2202673, 4406384)

AAB_data <- AAB_data %>%
  mutate(
    diagnosis_type = case_when(
      fhq_h1 == 1                        ~ "Autism",
      fhq_h1 == 2                        ~ "PDD-NOS",
      fhq_h1 == 3                        ~ "Asperger",
      fhq_h1 == 4 & !id %in% no_asd_ids ~ "Other/Multiple",
      fhq_h1 == 4 &  id %in% no_asd_ids ~ NA_character_,
      fhq_h1 == 5                        ~ NA_character_,
      TRUE                               ~ NA_character_
    ),
    diagnosis_type = factor(diagnosis_type,
                            levels = c("Autism", "PDD-NOS", "Asperger", "Other/Multiple"))
  )

## Verify
AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  count(diagnosis_type, useNA = "always")

## Create analytic diagnosis group
# ASD-Q (type 2) are excluded from diagnosis_group — only 1/15 had confirmed diagnosis.
# They are retained in the dataset with NA for sensitivity analyses.
AAB_data <- AAB_data %>%
  mutate(
    diagnosis_group = case_when(
      participant_type == 1 & !is.na(diagnosis_type) ~ "ASD",
      participant_type == 3                           ~ "Sibling",
      participant_type == 4                           ~ "Comparison",
      TRUE                                            ~ NA_character_
    ),
    diagnosis_group = factor(diagnosis_group,
                             levels = c("Comparison", "Sibling", "ASD"))
  )

## Check
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(participant_type_f, diagnosis_group, useNA = "always")

rm(no_asd_ids)

# ==============================================================================
# BLOCK 3: CSHQ — Children's Sleep Habits Questionnaire
# ==============================================================================

## All 34 raw item variables
cshq_items <- c(
  "fhq_h16_a","fhq_h16_b","fhq_h16_c","fhq_h16_d","fhq_h16_e","fhq_h16_f",
  "fhq_h16_g","fhq_h16_h","fhq_h16_i","fhq_h16_j","fhq_h16_k","fhq_h16_l",
  "fhq_h16_m","fhq_h16_n","fhq_h16_o","fhq_h16_p","fhq_h16_q","fhq_h16_r",
  "fhq_h16_s","fhq_h16_t","fhq_h16_u","fhq_h16_v","fhq_h16_w","fhq_h16_x",
  "fhq_h16_y","fhq_h16_z","fhq_h16_aa","fhq_h16_ab","fhq_h16_ac","fhq_h16_ad",
  "fhq_h16_ae","fhq_h16_af","fhq_h16_ag","fhq_h16_ah"
)

## Recode 999 → NA
AAB_data <- AAB_data %>%
  mutate(across(all_of(cshq_items), ~ ifelse(.x == 999, NA, .x)))

## Reverse-score positively worded items (4 - x: 1→3, 2→2, 3→1)
## New _r variables created; originals preserved for audit
cshq_reverse_items <- c(
  "fhq_h16_a",   # Goes to bed at same time
  "fhq_h16_b",   # Falls asleep in own bed
  "fhq_h16_g",   # Falls asleep within 20 minutes
  "fhq_h16_i",   # Sleeps the right amount
  "fhq_h16_j",   # Sleeps same amount each day
  "fhq_h16_aa"   # Wakes by himself
)

AAB_data <- AAB_data %>%
  mutate(across(
    all_of(cshq_reverse_items),
    ~ ifelse(is.na(.x), NA, 4 - .x),
    .names = "{.col}_r"
  ))

## ---- Subscale item vectors --------------------------------------------------

cshq_bedtime_resistance <- c(
  "fhq_h16_a_r",   # Goes to bed at same time (R)
  "fhq_h16_b_r",   # Falls asleep in own bed (R)
  "fhq_h16_c",     # Falls asleep in other's bed
  "fhq_h16_d",     # Needs parent in room to sleep
  "fhq_h16_e",     # Struggles at bedtime
  "fhq_h16_f"      # Afraid / shows fear of sleeping alone
)

cshq_sleep_onset_delay <- c(
  "fhq_h16_g_r"    # Falls asleep within 20 minutes (R)
)

cshq_sleep_duration <- c(
  "fhq_h16_h",     # Sleeps too little
  "fhq_h16_i_r",   # Sleeps the right amount (R)
  "fhq_h16_j_r"    # Sleeps same amount each day (R)
)

cshq_sleep_anxiety <- c(
  "fhq_h16_d",     # Needs parent in room to sleep (shared with Bedtime Resistance)
  "fhq_h16_k",     # Afraid of sleeping in the dark
  "fhq_h16_l",     # Afraid of sleeping alone (duplicate of fhq_h16_f — in subscale, excluded from total)
  "fhq_h16_m"      # Trouble sleeping away from home
)

cshq_night_wakings <- c(
  "fhq_h16_n",     # Moves to other's bed at night
  "fhq_h16_o",     # Awakes once during the night
  "fhq_h16_p"      # Awakes more than once during the night
)

cshq_parasomnias <- c(
  "fhq_h16_q",     # Wets the bed at night
  "fhq_h16_r",     # Talks during sleep
  "fhq_h16_s",     # Becomes restless / moves a lot
  "fhq_h16_t",     # Sleepwalks
  "fhq_h16_u",     # Grinds teeth
  "fhq_h16_v",     # Awakens screaming and/or sweating
  "fhq_h16_w"      # Expresses alarm at a scary dream
)

cshq_sdb <- c(
  "fhq_h16_x",     # Snores loudly
  "fhq_h16_y",     # Stops breathing
  "fhq_h16_z"      # Snorts and gasps
)

cshq_daytime_sleepiness <- c(
  "fhq_h16_aa_r",  # Wakes by himself (R)
  "fhq_h16_ab",    # Wakes up in negative mood
  "fhq_h16_ac",    # Gets woken up by others
  "fhq_h16_ad",    # Hard time getting out of bed
  "fhq_h16_ae",    # Takes a long time to be alert
  "fhq_h16_af",    # Seems tired
  "fhq_h16_ag",    # Falls asleep while watching TV
  "fhq_h16_ah"     # Falls asleep while riding in a car
)

# Total score: 33 unique items.
# fhq_h16_d counted once (Bedtime Resistance; shared with Sleep Anxiety).
# fhq_h16_l excluded (conceptual duplicate of fhq_h16_f per Owens et al., 2000).
cshq_total_items <- c(
  "fhq_h16_a_r", "fhq_h16_b_r", "fhq_h16_c", "fhq_h16_d",
  "fhq_h16_e",   "fhq_h16_f",
  "fhq_h16_g_r",
  "fhq_h16_h",   "fhq_h16_i_r", "fhq_h16_j_r",
  "fhq_h16_k",   "fhq_h16_m",
  "fhq_h16_n",   "fhq_h16_o",   "fhq_h16_p",
  "fhq_h16_q",   "fhq_h16_r",   "fhq_h16_s",  "fhq_h16_t",
  "fhq_h16_u",   "fhq_h16_v",   "fhq_h16_w",
  "fhq_h16_x",   "fhq_h16_y",   "fhq_h16_z",
  "fhq_h16_aa_r","fhq_h16_ab",  "fhq_h16_ac", "fhq_h16_ad",
  "fhq_h16_ae",  "fhq_h16_af",  "fhq_h16_ag", "fhq_h16_ah"
)

length(cshq_total_items)  # should be 33

## ---- Compute subscale and total scores --------------------------------------

AAB_data <- AAB_data %>%
  mutate(
    cshq_bedtime_resistance = rowSums(across(all_of(cshq_bedtime_resistance)), na.rm = FALSE),
    cshq_sleep_onset_delay  = rowSums(across(all_of(cshq_sleep_onset_delay)),  na.rm = FALSE),
    cshq_sleep_duration     = rowSums(across(all_of(cshq_sleep_duration)),     na.rm = FALSE),
    cshq_sleep_anxiety      = rowSums(across(all_of(cshq_sleep_anxiety)),      na.rm = FALSE),
    cshq_night_wakings      = rowSums(across(all_of(cshq_night_wakings)),      na.rm = FALSE),
    cshq_parasomnias        = rowSums(across(all_of(cshq_parasomnias)),        na.rm = FALSE),
    cshq_sdb                = rowSums(across(all_of(cshq_sdb)),                na.rm = FALSE),
    cshq_daytime_sleepiness = rowSums(across(all_of(cshq_daytime_sleepiness)), na.rm = FALSE),
    cshq_total              = rowSums(across(all_of(cshq_total_items)),        na.rm = FALSE)
  )

## Check
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  group_by(participant_type_f) %>%
  summarise(
    n         = sum(!is.na(cshq_total)),
    n_missing = sum(is.na(cshq_total)),
    min       = min(cshq_total, na.rm = TRUE),
    max       = max(cshq_total, na.rm = TRUE),
    mean      = mean(cshq_total, na.rm = TRUE),
    sd        = sd(cshq_total, na.rm = TRUE)
  )

## ---- Prorating (Owens et al., 2000: prorate if ≤ 3 items missing) ----------

AAB_data <- AAB_data %>%
  mutate(
    n_items_present = rowSums(!is.na(across(all_of(cshq_total_items)))),
    n_items_missing = 33 - n_items_present,
    
    cshq_total_prorated = case_when(
      !is.na(cshq_total)                             ~ cshq_total,
      n_items_missing > 0 & n_items_missing <= 3     ~
        round(rowSums(across(all_of(cshq_total_items)), na.rm = TRUE) *
                (33 / n_items_present), 1),
      TRUE                                           ~ NA_real_
    )
  )

## Check recovery
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  summarise(
    n_complete = sum(!is.na(cshq_total)),
    n_prorated = sum(!is.na(cshq_total_prorated)) - sum(!is.na(cshq_total)),
    n_missing  = sum(is.na(cshq_total_prorated)),
    mean       = mean(cshq_total_prorated, na.rm = TRUE),
    sd         = sd(cshq_total_prorated, na.rm = TRUE)
  )

## Item-level missingness among partial responders
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4),
         is.na(cshq_total),
         n_items_missing > 0 & n_items_missing <= 33) %>%
  summarise(across(all_of(cshq_total_items), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "item", values_to = "n_missing") %>%
  arrange(desc(n_missing))

## Drop intermediate item-count columns
AAB_data <- AAB_data %>%
  select(-n_items_present, -n_items_missing)

## FIX: remove only objects that actually exist
# cshq_vars and cshq_total_items_33 were referenced in v01 but never created.
rm(cshq_reverse_items)


# ==============================================================================
# BLOCK 4: Family ID derivation
# ==============================================================================
# Strategy: resolve family clusters from probands outward using
# acl_mother_id / acl_father_id (linking children → parents).
# Mother ID is the primary family anchor (higher completeness);
# father ID is used as fallback in single-parent families.
# Comparison children receive unique singleton family IDs.

## Step 1: Gather child–parent linkage rows
child_parent_links <- AAB_data %>%
  filter(participant_type %in% c(1, 2, 3)) %>%
  select(id, acl_mother_id, acl_father_id) %>%
  filter(!is.na(acl_mother_id) | !is.na(acl_father_id))

## Step 2: One-row-per-family anchor table
family_anchors <- child_parent_links %>%
  mutate(
    family_anchor = case_when(
      !is.na(acl_mother_id) ~ as.character(acl_mother_id),
      !is.na(acl_father_id) ~ as.character(acl_father_id),
      TRUE                  ~ NA_character_
    )
  ) %>%
  filter(!is.na(family_anchor)) %>%
  select(id, family_anchor)

## Step 3: Map every participant to its family anchor

# Children
child_family_map <- family_anchors %>%
  rename(participant_id = id)

# Mothers: their own ID is the anchor
mother_map <- child_parent_links %>%
  filter(!is.na(acl_mother_id)) %>%
  distinct(acl_mother_id) %>%
  mutate(participant_id = as.character(acl_mother_id),
         family_anchor  = as.character(acl_mother_id)) %>%
  select(participant_id, family_anchor)

# Fathers in single-parent families: their own ID is the anchor
father_map <- child_parent_links %>%
  filter(!is.na(acl_father_id)) %>%
  group_by(acl_father_id) %>%
  filter(all(is.na(acl_mother_id))) %>%
  ungroup() %>%
  distinct(acl_father_id) %>%
  mutate(participant_id = as.character(acl_father_id),
         family_anchor  = as.character(acl_father_id)) %>%
  select(participant_id, family_anchor)

# Fathers in two-parent families: inherit the mother anchor
father_in_complete_family <- child_parent_links %>%
  filter(!is.na(acl_mother_id), !is.na(acl_father_id)) %>%
  distinct(acl_mother_id, acl_father_id) %>%
  mutate(participant_id = as.character(acl_father_id),
         family_anchor  = as.character(acl_mother_id)) %>%
  select(participant_id, family_anchor)

## Step 4: Stack and deduplicate
all_family_map <- bind_rows(
  child_family_map %>% mutate(participant_id = as.character(participant_id)),
  mother_map,
  father_map,
  father_in_complete_family
) %>%
  distinct(participant_id, .keep_all = TRUE)

## Step 5: Integer family_id
anchor_to_fid <- all_family_map %>%
  distinct(family_anchor) %>%
  arrange(family_anchor) %>%
  mutate(family_id = row_number())

all_family_map <- all_family_map %>%
  left_join(anchor_to_fid, by = "family_anchor")

## Step 6: Join back to AAB_data
AAB_data <- AAB_data %>%
  left_join(
    all_family_map %>%
      select(participant_id, family_id) %>%
      mutate(participant_id = as.integer(participant_id)),
    by = c("id" = "participant_id")
  )

## Step 7: Singleton family IDs for Comparison children
max_fid <- max(AAB_data$family_id, na.rm = TRUE)

AAB_data <- AAB_data %>%
  mutate(
    family_id = case_when(
      participant_type == 4 & is.na(family_id) ~ max_fid + row_number(),
      TRUE                                     ~ family_id
    )
  )

## Approximate multiplex flag: > 1 proband/ASD-Q per family
multiplex_approx <- AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  count(family_id, name = "n_asd_children") %>%
  mutate(multiplex_approx = n_asd_children > 1)

AAB_data <- AAB_data %>%
  left_join(multiplex_approx, by = "family_id")

## Pedigree linkage flag
AAB_data <- AAB_data %>%
  mutate(
    pedigree_linked = case_when(
      participant_type %in% c(1, 2, 3) & is.na(acl_mother_id) & is.na(acl_father_id) ~ FALSE,
      participant_type %in% c(1, 2, 3)                                                ~ TRUE,
      TRUE                                                                             ~ NA
    )
  )

## Clean up
rm(child_parent_links, family_anchors, child_family_map,
   mother_map, father_map, father_in_complete_family,
   all_family_map, anchor_to_fid, max_fid, multiplex_approx)

## Verification
AAB_data %>%
  group_by(participant_type_f) %>%
  summarise(
    n             = n(),
    n_with_fid    = sum(!is.na(family_id)),
    n_missing_fid = sum(is.na(family_id)),
    n_families    = n_distinct(family_id, na.rm = TRUE)
  )

# ==============================================================================
# BLOCK 5: Melatonin use variables
# ==============================================================================
# Source: fhq_h13_[a–f] (prescribed) and fhq_h14_[a–f] (natural therapies)
# + fhq_h13_other (catch-all free-text field)

## Helper — detect melatonin mentions (case-insensitive, common misspellings)
is_melatonin <- function(x) {
  grepl(
    paste("melat", "melot", "mellot", "circadi", "cirdadi",
          "melatone", "sleep.?tone", sep = "|"),
    x, ignore.case = TRUE, perl = TRUE
  )
}

## Helper — clean free-text age-at-first-use field
clean_age_field <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  case_when(
    is.na(x_num)  ~ NA_real_,
    x_num >= 1900 ~ NA_real_,   # calendar year entered by mistake
    x_num < 0     ~ NA_real_,
    x_num > 30    ~ NA_real_,   # implausible age
    TRUE          ~ x_num
  )
}

## Inspect unique medication names
all_unique_meds <- AAB_data[
  names(AAB_data)[grepl("^fhq_h1[34]_[a-f]_name$", names(AAB_data))]
] %>%
  unlist(use.names = FALSE) %>%
  as.character() %>%
  trimws() %>%
  unique()

old <- getOption("max.print"); options(max.print = 999999)
all_unique_meds
options(max.print = old); rm(old)

## Step 1: Build long-format medication table

prescribed_slots <- letters[1:6]

prescribed_long <- map_dfr(prescribed_slots, function(slot) {
  name_col    <- paste0("fhq_h13_", slot, "_name")
  if (!name_col %in% names(AAB_data)) return(NULL)
  AAB_data %>%
    select(id,
           med_name    = all_of(paste0("fhq_h13_", slot, "_name")),
           med_age     = all_of(paste0("fhq_h13_", slot, "_age")),
           med_dur     = all_of(paste0("fhq_h13_", slot, "_duration")),
           med_current = all_of(paste0("fhq_h13_", slot, "_current")),
           med_reason  = all_of(paste0("fhq_h13_", slot, "_reason")),
           med_improve = all_of(paste0("fhq_h13_", slot, "_improve"))) %>%
    mutate(slot = slot, med_section = "prescribed")
}) %>%
  filter(!is.na(med_name), str_trim(med_name) != "")

natural_long <- map_dfr(prescribed_slots, function(slot) {
  name_col <- paste0("fhq_h14_", slot, "_name")
  if (!name_col %in% names(AAB_data)) return(NULL)
  AAB_data %>%
    select(id,
           med_name    = all_of(paste0("fhq_h14_", slot, "_name")),
           med_age     = all_of(paste0("fhq_h14_", slot, "_age")),
           med_reason  = all_of(paste0("fhq_h14_", slot, "_reason")),
           med_improve = all_of(paste0("fhq_h14_", slot, "_improve"))) %>%
    mutate(slot = slot, med_section = "natural",
           med_dur = NA_character_, med_current = NA_real_)
}) %>%
  filter(!is.na(med_name), str_trim(med_name) != "")

other_mentions <- AAB_data %>%
  filter(!is.na(fhq_h13_other), is_melatonin(fhq_h13_other)) %>%
  select(id, med_name = fhq_h13_other) %>%
  mutate(slot = "other", med_section = "prescribed_other",
         med_age = NA_character_, med_dur = NA_character_,
         med_current = NA_real_, med_reason = NA_character_,
         med_improve = NA_real_)

all_meds_long <- bind_rows(prescribed_long, natural_long, other_mentions)

## Step 2: Flag melatonin rows
all_meds_long <- all_meds_long %>%
  mutate(is_mel = is_melatonin(med_name))

all_meds_long %>%
  filter(is_mel) %>%
  count(med_section, med_name) %>%
  arrange(desc(n))

## Step 3: Participant-level melatonin summary
melatonin_summary <- all_meds_long %>%
  group_by(id) %>%
  summarise(
    melatonin_ever    = any(is_mel, na.rm = TRUE),
    melatonin_current = any(is_mel & med_current %in% c(1, 3), na.rm = TRUE),
    melatonin_age_first = suppressWarnings(
      min(clean_age_field(med_age[is_mel]), na.rm = TRUE)
    ),
    melatonin_duration_raw = paste(
      na.omit(med_dur[is_mel & !is.na(med_dur)]), collapse = "; "
    ),
    melatonin_improved = case_when(
      any(is_mel & med_improve == 2, na.rm = TRUE) ~ 1L,
      any(is_mel & med_improve == 1, na.rm = TRUE) ~ 0L,
      TRUE                                         ~ NA_integer_
    ),
    melatonin_source = case_when(
      any(is_mel & med_section == "prescribed", na.rm = TRUE) &
        any(is_mel & med_section == "natural",  na.rm = TRUE) ~ "both",
      any(is_mel & med_section == "prescribed", na.rm = TRUE) ~ "prescribed",
      any(is_mel & med_section == "natural",    na.rm = TRUE) ~ "natural",
      TRUE                                                     ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    melatonin_age_first    = if_else(is.infinite(melatonin_age_first),
                                     NA_real_, melatonin_age_first),
    melatonin_duration_raw = if_else(melatonin_duration_raw == "",
                                     NA_character_, melatonin_duration_raw),
    melatonin_ever         = as.integer(melatonin_ever),
    melatonin_current      = as.integer(melatonin_current),
    melatonin_current_f    = factor(melatonin_current,
                                    levels = c(0, 1), labels = c("No", "Yes"))
  )

# Flag participants who concatenated full medication history into one name field
# (melatonin_ever is valid; ancillary fields are not)
multi_entry_ids <- all_meds_long %>%
  filter(is_mel, str_count(med_name, "[,;]") >= 2) %>%
  pull(id) %>%
  unique()

## Step 4: Join back to AAB_data
AAB_data <- AAB_data %>%
  left_join(melatonin_summary, by = "id") %>%
  mutate(
    melatonin_ever         = replace_na(melatonin_ever, 0L),
    melatonin_current      = replace_na(melatonin_current, 0L),
    melatonin_current_f    = factor(melatonin_current,
                                    levels = c(0, 1), labels = c("No", "Yes")),
    melatonin_entry_concat = as.integer(id %in% multi_entry_ids)
  )

## Step 5: Verification
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  group_by(participant_type_f, melatonin_ever, melatonin_current) %>%
  count() %>%
  print(n = 50)

AAB_data %>%
  filter(melatonin_ever == 1) %>%
  summarise(
    n        = n(),
    n_age    = sum(!is.na(melatonin_age_first)),
    min_age  = min(melatonin_age_first,  na.rm = TRUE),
    max_age  = max(melatonin_age_first,  na.rm = TRUE),
    mean_age = mean(melatonin_age_first, na.rm = TRUE),
    sd_age   = sd(melatonin_age_first,   na.rm = TRUE)
  )

AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4), !is.na(diagnosis_group)) %>%
  count(diagnosis_group, melatonin_ever) %>%
  pivot_wider(names_from = melatonin_ever, values_from = n,
              names_prefix = "mel_ever_")

AAB_data %>%
  filter(melatonin_entry_concat == 1) %>%
  count(participant_type_f, melatonin_current)

## FIX: single rm() call covering all melatonin intermediates
rm(prescribed_long, natural_long, other_mentions, all_meds_long,
   melatonin_summary, prescribed_slots, multi_entry_ids,
   is_melatonin, clean_age_field, all_unique_meds)


# ==============================================================================
# BLOCK 6: Tanner pubertal staging
# ==============================================================================
# Source: tanner form — tanner_sex, tanner_boy_genital, tanner_girl_genital
# Genital/breast development used as the sole domain, consistent with
# Yap et al. (2023, Nat Med) and Emmanuel & Bokor (2022, StatPearls).

## Inspect completion flag (1=Yes | 0=No | 2=N/A | 999=error)
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(acl_tanner_completed, useNA = "always")

## Recode 999 → NA across Tanner items and the completion flag
tanner_items <- c("tanner_sex", "tanner_boy_genital", "tanner_boy_pubichair",
                  "tanner_girl_genital", "tanner_girl_pubichair")

AAB_data <- AAB_data %>%
  mutate(
    across(all_of(tanner_items), ~ ifelse(.x == 999, NA, .x)),
    acl_tanner_completed = ifelse(acl_tanner_completed == 999, NA, acl_tanner_completed)
  )

## Sex concordance check between tanner_sex and derived sex variable
# tanner_sex: 1=Boys, 2=Girls  |  sex (derived): 1=Male, 2=Female — same coding
AAB_data <- AAB_data %>%
  mutate(
    tanner_sex_mismatch = case_when(
      participant_type %in% c(5, 6) ~ NA,
      is.na(tanner_sex)             ~ NA,
      is.na(as.integer(sex))        ~ NA,
      tanner_sex != as.integer(sex) ~ TRUE,
      TRUE                          ~ FALSE
    )
  )

AAB_data %>%
  filter(tanner_sex_mismatch == TRUE) %>%
  select(id, participant_type_f, sex, tanner_sex)
# Known mismatch: participant 1131608

## FIX: create tanner_sex_resolved before it is used in the genital mutate.
# Use tanner_sex where concordant; fall back to derived sex where tanner_sex
# is absent but sex is known; set to NA for the mismatched case (1131608).
AAB_data <- AAB_data %>%
  mutate(
    tanner_sex_resolved = case_when(
      tanner_sex_mismatch == TRUE  ~ NA_real_,        # sex-scale mismatch — cannot map
      !is.na(tanner_sex)           ~ as.numeric(tanner_sex),  # concordant tanner record
      !is.na(as.integer(sex))      ~ as.numeric(as.integer(sex)), # fall back to derived sex
      TRUE                         ~ NA_real_
    )
  )

## Unified Tanner genital score
AAB_data <- AAB_data %>%
  mutate(
    tanner_genital = case_when(
      participant_type %in% c(5, 6) ~ NA_real_,
      tanner_sex_resolved == 1      ~ as.numeric(tanner_boy_genital),
      tanner_sex_resolved == 2      ~ as.numeric(tanner_girl_genital),
      TRUE                          ~ NA_real_
    ),
    tanner_genital_f = factor(tanner_genital, levels = 1:5,
                              labels = c("Stage 1", "Stage 2", "Stage 3",
                                         "Stage 4", "Stage 5"),
                              ordered = TRUE)
  )

## Verification
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  group_by(participant_type_f, sex) %>%
  summarise(
    n          = n(),
    n_complete = sum(!is.na(tanner_genital)),
    n_missing  = sum(is.na(tanner_genital)),
    mean       = mean(tanner_genital, na.rm = TRUE),
    sd         = sd(tanner_genital,   na.rm = TRUE),
    .groups    = "drop"
  )

AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(tanner_genital_f, useNA = "always")

## Missingness reason flag
AAB_data <- AAB_data %>%
  mutate(
    tanner_missing_reason = case_when(
      participant_type %in% c(5, 6)                              ~ "not_applicable",
      !is.na(tanner_genital)                                     ~ "complete",
      id == 1131608                                              ~ "sex_scale_mismatch_genital_not_mappable",
      acl_tanner_completed == 0                                  ~ "assessment_not_done",
      acl_tanner_completed == 2                                  ~ "not_applicable_na",
      acl_tanner_completed == 1 & is.na(tanner_genital)         ~ "done_but_items_missing",
      is.na(acl_tanner_completed)                               ~ "no_checklist_record",
      TRUE                                                       ~ "unknown"
    )
  )

## Drop all Tanner intermediate variables
AAB_data <- AAB_data %>%
  select(-tanner_sex_mismatch, -tanner_sex_resolved)

rm(tanner_items)

# ==============================================================================
# BLOCK 7: Ethnicity
# ==============================================================================
# Source: fhq_b8 (maternal), fhq_b23 (paternal)
# Strategy: maternal ethnicity as child proxy; paternal as fallback.
# Propagate within families (FHQ completed once per family).
# Codes: 1=Caucasian | 2=Aboriginal | 3=Asian | 4=Maori/Pacific Islander | 5=Other

AAB_data %>% count(fhq_b8,  useNA = "always")
AAB_data %>% count(fhq_b23, useNA = "always")

## Recode 999 → NA
AAB_data <- AAB_data %>%
  mutate(
    fhq_b8_clean  = if_else(fhq_b8  == 999, NA_real_, as.numeric(fhq_b8)),
    fhq_b23_clean = if_else(fhq_b23 == 999, NA_real_, as.numeric(fhq_b23))
  )

## Derive proxy ethnicity
AAB_data <- AAB_data %>%
  mutate(
    ethnicity_source = case_when(
      !is.na(fhq_b8_clean)  ~ "mother",
      !is.na(fhq_b23_clean) ~ "father",
      TRUE                  ~ NA_character_
    ),
    ethnicity_raw = case_when(
      !is.na(fhq_b8_clean)  ~ fhq_b8_clean,
      !is.na(fhq_b23_clean) ~ fhq_b23_clean,
      TRUE                  ~ NA_real_
    ),
    ethnicity_f = factor(ethnicity_raw, levels = 1:5,
                         labels = c("Caucasian", "Aboriginal", "Asian",
                                    "Maori/Pacific Islander", "Other"))
  )

## Discordant parental ethnicity flag
AAB_data <- AAB_data %>%
  mutate(
    ethnicity_discordant = case_when(
      !is.na(fhq_b8_clean) & !is.na(fhq_b23_clean) &
        fhq_b8_clean != fhq_b23_clean ~ TRUE,
      !is.na(fhq_b8_clean) & !is.na(fhq_b23_clean) &
        fhq_b8_clean == fhq_b23_clean ~ FALSE,
      TRUE                            ~ NA
    )
  )

AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(ethnicity_discordant, useNA = "always")

AAB_data <- AAB_data %>%
  select(-fhq_b8_clean, -fhq_b23_clean)

## Propagate ethnicity within families (FHQ completed once per family)
family_ethnicity <- AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4),
         !is.na(family_id), !is.na(ethnicity_raw)) %>%
  group_by(family_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(family_id,
         ethnicity_raw_fam        = ethnicity_raw,
         ethnicity_f_fam          = ethnicity_f,
         ethnicity_source_fam     = ethnicity_source,
         ethnicity_discordant_fam = ethnicity_discordant)

AAB_data <- AAB_data %>%
  left_join(family_ethnicity, by = "family_id") %>%
  mutate(
    ethnicity_resolution = case_when(
      !is.na(ethnicity_raw)     ~ "own_row",
      !is.na(ethnicity_raw_fam) ~ "family_propagated",
      TRUE                      ~ NA_character_
    ),
    ethnicity_raw        = coalesce(ethnicity_raw,        ethnicity_raw_fam),
    ethnicity_f          = coalesce(ethnicity_f,          ethnicity_f_fam),
    ethnicity_source     = coalesce(ethnicity_source,     ethnicity_source_fam),
    ethnicity_discordant = coalesce(ethnicity_discordant, ethnicity_discordant_fam)
  ) %>%
  select(-ethnicity_raw_fam, -ethnicity_f_fam,
         -ethnicity_source_fam, -ethnicity_discordant_fam)

rm(family_ethnicity)

AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(ethnicity_resolution, useNA = "always")

## Collapsed 4-level factor for regression
# NOTE: Aboriginal and Maori/Pacific Islander combined into "Indigenous/Pacific"
# due to small cell sizes (n≈11 and n≈14). This grouping is acknowledged as a
# limitation per NHMRC guidelines on research with Aboriginal and Torres Strait
# Islander peoples.
AAB_data <- AAB_data %>%
  mutate(
    ethnicity_collapsed = case_when(
      ethnicity_f == "Caucasian"                             ~ "Caucasian",
      ethnicity_f == "Asian"                                 ~ "Asian",
      ethnicity_f %in% c("Aboriginal", "Maori/Pacific Islander") ~ "Indigenous/Pacific",
      ethnicity_f == "Other"                                 ~ "Other",
      TRUE                                                   ~ NA_character_
    ),
    ethnicity_collapsed = factor(ethnicity_collapsed,
                                 levels = c("Caucasian", "Asian",
                                            "Indigenous/Pacific", "Other")),
    ethnicity_binary = case_when(
      ethnicity_f == "Caucasian" ~ "Caucasian",
      !is.na(ethnicity_f)        ~ "Non-Caucasian",
      TRUE                       ~ NA_character_
    ),
    ethnicity_binary = factor(ethnicity_binary,
                              levels = c("Caucasian", "Non-Caucasian"))
  )

AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(ethnicity_collapsed, useNA = "always")

AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(ethnicity_binary, useNA = "always")


# ==============================================================================
# BLOCK 8: SES composite variable
# ==============================================================================
# Sources: fhq_b36 (income), fhq_b10 (maternal edu), fhq_b25 (paternal edu)
# Unit: family level — derived from proband row, propagated via family_id.
# Method: equal-weight z-score average (Avvisati et al., 2020).

## Clean income
# fhq_b36: 12-point ordinal (1=$1–$8k, 12=>$104k)
# Code 13="prefer not to say"; 999=error → both NA
AAB_data <- AAB_data %>%
  mutate(
    income_raw = case_when(
      fhq_b36 %in% c(13, 999) ~ NA_real_,
      TRUE                    ~ as.numeric(fhq_b36)
    )
  )

AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  count(income_raw, useNA = "always") %>%
  arrange(income_raw)

## Clean education
# fhq_b10 (mother) / fhq_b25 (father): 7-point ordinal
# 1=<10yrs, 2=10yrs, 3=11yrs, 4=12yrs, 5=Trade/Cert, 6=Degree, 7=Other→NA
AAB_data <- AAB_data %>%
  mutate(
    edu_mother = case_when(
      fhq_b10 %in% c(7, 999) ~ NA_real_,
      TRUE                   ~ as.numeric(fhq_b10)
    ),
    edu_father = case_when(
      fhq_b25 %in% c(7, 999) ~ NA_real_,
      TRUE                   ~ as.numeric(fhq_b25)
    ),
    edu_max = pmax(edu_mother, edu_father, na.rm = TRUE)
  )

AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  summarise(
    n_mother_edu = sum(!is.na(edu_mother)),
    n_father_edu = sum(!is.na(edu_father)),
    n_edu_max    = sum(!is.na(edu_max)),
    n_income     = sum(!is.na(income_raw))
  )

## FIX: Build family-level SES lookup AND join it back before rm()
family_ses_raw <- AAB_data %>%
  filter(participant_type %in% c(1, 2), !is.na(family_id)) %>%
  arrange(family_id, participant_type) %>%   # proband (type 1) first
  group_by(family_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(family_id, income_raw, edu_max) %>%
  mutate(
    income_z  = as.numeric(scale(income_raw)),
    edu_max_z = as.numeric(scale(edu_max)),
    
    # Complete-case composite (NA if either component missing)
    ses_composite_complete = rowMeans(cbind(income_z, edu_max_z), na.rm = FALSE),
    
    # Partial composite (allows one missing component — sensitivity only)
    ses_composite_partial  = rowMeans(cbind(income_z, edu_max_z), na.rm = TRUE),
    
    # How many components contributed to the partial composite
    ses_n_components = as.integer(!is.na(income_z)) + as.integer(!is.na(edu_max_z))
  ) %>%
  select(family_id, ses_composite_complete, ses_composite_partial, ses_n_components)

## Check coverage at family level
family_ses_raw %>%
  summarise(
    n_families = n(),
    n_complete = sum(!is.na(ses_composite_complete)),
    n_partial  = sum(!is.na(ses_composite_partial)),
    n_single   = sum(ses_n_components == 1, na.rm = TRUE),
    mean_c     = mean(ses_composite_complete, na.rm = TRUE),
    sd_c       = sd(ses_composite_complete,   na.rm = TRUE)
  )

## Join SES variables to all participants via family_id
AAB_data <- AAB_data %>%
  left_join(family_ses_raw, by = "family_id")

## Verify propagation
AAB_data %>%
  filter(participant_type %in% c(1, 3, 4)) %>%
  summarise(
    n_ses_complete   = sum(!is.na(ses_composite_complete)),
    n_ses_partial    = sum(!is.na(ses_composite_partial)),
    n_ses_missing    = sum(is.na(ses_composite_partial)),
    pct_ses_complete = round(mean(!is.na(ses_composite_complete)) * 100, 1)
  )

## Clean up
rm(family_ses_raw)

AAB_data <- AAB_data %>%
  select(-edu_mother, -edu_father)


# ==============================================================================
# FINAL CHECK
# ==============================================================================

cat("\n── New variables added to AAB_data:\n")
print(setdiff(names(AAB_data), names(AAB_safe_copy)))

cat("\n── Dimensions:", nrow(AAB_data), "rows ×", ncol(AAB_data), "columns\n")

cat("\n── Participant type summary:\n")
print(table(AAB_data$participant_type_f, useNA = "always"))

cat("\n── Key derived variables present:\n")
key_vars <- c("age", "sex", "site", "diagnosis_group", "family_id",
              "pedigree_linked", "cshq_total_prorated",
              "melatonin_ever", "tanner_genital",
              "ethnicity_collapsed", "ses_composite_complete",
              "ses_composite_partial", "ses_n_components")
print(sapply(key_vars, function(v) v %in% names(AAB_data)))
