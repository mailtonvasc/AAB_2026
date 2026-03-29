# title:          "Sleep disruption as an endophenotype of Autism"
# purpose:        "Output for FamilySLeeps"
# date:           "20/01/2026"
# author:         "Mailton Vasconcelos"

# --- Packages ----
library(tidyr)
library(dplyr)
library(readxl) #for reading .csv file
library(visdat) #for inspecting missingness
library(lubridate)
library(stringr) #for trimming

# ---- Load the data ----
AAB_data <- read.csv("AustralianAutismBiob-AccessRequest2024047_DATA_2025-05-14_1023.csv",  stringsAsFactors = FALSE)
AAB_safe_copy <- AAB_data

# ---- Retrieve and clean demographic variables ----

## Participant type
table(AAB_data$participant_type, useNA = "always")
unique(AAB_data$participant_type) #check variable for placeholders like 999
summary(AAB_data$participant_type)
str(AAB_data$participant_type)

### Recode participant type to factor
AAB_data <- AAB_data %>%
  filter(!is.na(id), !is.na(participant_type)) %>%
  mutate(
    participant_type_f = factor(participant_type,
                                levels = 1:6,
                                labels = c("Proband", "ASD-Q", "Sibling", "Control", "Mother", "Father"))
  )

table(AAB_data$participant_type, useNA = "always")

### Age
#### Children (missing = 6)
str(AAB_data$age_at_assessment)
summary(AAB_data$age_at_assessment, useNA = "always") # checked against the age calculated from proforma's dob and doa

names(AAB_data) %>% 
  grep("date|dob|doa|doe|birth|assess", ., value = TRUE, ignore.case = TRUE) #look for variables containing dates

#### Calculate age from date of birth and date of assessments
# Sources used in order of priority:
# 1. proforma_dob + proforma_doa (original REDCap calculated field)
# 2. ados_dob + ados_doe
# 3. ados_dob + vabs_testdate
# 4. vabs_dob + acl_childsample_date
# 5. vabs_dob + vabs_testdate
# 6. acl_child_dob + acl_proforma_date
# 7. acl_child_dob + acl_cog_date
# 8. acl_child_dob + acl_childsample_date


AAB_data <- AAB_data %>%
  mutate(
    # Parse all date fields
    proforma_dob_d          = ymd(proforma_dob),
    proforma_doa_d          = ymd(proforma_doa),
    ados_dob_d              = ymd(ados_dob),
    ados_doe_d              = ymd(ados_doe),
    vabs_dob_d              = ymd(vabs_dob),
    vabs_testdate_d         = ymd(vabs_testdate),
    acl_child_dob_d         = ymd(acl_child_dob),
    acl_childsample_date_d  = ymd(acl_childsample_date),
    acl_proforma_date_d     = ymd(acl_proforma_date),
    acl_cog_date_d          = ymd(acl_cog_date),
    
    # Recover age in priority order
    age_at_assessment_clean = case_when(
      !is.na(proforma_dob_d)   & !is.na(proforma_doa_d)          ~
        as.numeric(interval(proforma_dob_d,  proforma_doa_d)         / years(1)),
      !is.na(ados_dob_d)       & !is.na(ados_doe_d)              ~
        as.numeric(interval(ados_dob_d,      ados_doe_d)             / years(1)),
      !is.na(ados_dob_d)       & !is.na(vabs_testdate_d)         ~
        as.numeric(interval(ados_dob_d,      vabs_testdate_d)        / years(1)),
      !is.na(vabs_dob_d)       & !is.na(acl_childsample_date_d)  ~
        as.numeric(interval(vabs_dob_d,      acl_childsample_date_d) / years(1)),
      !is.na(vabs_dob_d)       & !is.na(vabs_testdate_d)         ~
        as.numeric(interval(vabs_dob_d,      vabs_testdate_d)        / years(1)),
      !is.na(acl_child_dob_d)  & !is.na(acl_proforma_date_d)    ~
        as.numeric(interval(acl_child_dob_d, acl_proforma_date_d)    / years(1)),
      !is.na(acl_child_dob_d)  & !is.na(acl_cog_date_d)         ~
        as.numeric(interval(acl_child_dob_d, acl_cog_date_d)         / years(1)),
      !is.na(acl_child_dob_d)  & !is.na(acl_childsample_date_d) ~
        as.numeric(interval(acl_child_dob_d, acl_childsample_date_d) / years(1)),
      TRUE ~ NA_real_
    ),
    
    # Remove intermediate parsed date columns
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

#### Verification
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  summarise(
    n_total         = n(),
    n_recovered     = sum(!is.na(age_at_assessment_clean)),
    n_still_missing = sum(is.na(age_at_assessment_clean)),
    min             = min(age_at_assessment_clean, na.rm = TRUE),
    max             = max(age_at_assessment_clean, na.rm = TRUE),
    mean            = mean(age_at_assessment_clean, na.rm = TRUE),
    sd              = sd(age_at_assessment_clean, na.rm = TRUE)
  )

### Calculate age for parents from children data
parent_dob <- AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  select(acl_mother_id, acl_father_id, fhq_b9, fhq_b24_dob, proforma_doa) %>%
  pivot_longer(cols = c(acl_mother_id, acl_father_id), # pile up all ids (mother and father) and creates a parent type variable
               names_to = "parent_type", values_to = "parent_id") %>% # values populate the id, the original col names become a factor
  mutate(
    parent_dob = case_when( # creates more readable "if-else" conditions
      parent_type == "acl_mother_id" ~ ymd(fhq_b9), # if the id comes from mother use parent_dob from fhq_b9
      parent_type == "acl_father_id" ~ ymd(fhq_b24_dob) # if the id comes from father use parent_dob from fhq_b24_dob
    )
  ) %>%
  filter(!is.na(parent_id)) %>% # drop cases where parent id was absent
  distinct(parent_id, .keep_all = TRUE) %>% # produce one row per parent per child since we have siblings in the dataset
  select(parent_id, parent_dob, assessment_date = proforma_doa) # creates the data subset parent_dob only with relevant variables

#### Join and compute age of parents using renamed column
AAB_data <- AAB_data %>%
  left_join(parent_dob, by = c("id" = "parent_id")) %>%
  mutate(
    age_parent = as.numeric(interval(parent_dob, assessment_date) / years(1))
  )

### Single unified age variable across all participant types
AAB_data <- AAB_data %>%
  mutate(
    age = case_when(
      participant_type %in% c(1, 2, 3, 4) ~ age_at_assessment_clean,
      participant_type %in% c(5, 6)        ~ age_parent
    )
  )

#### Final check across all groups
AAB_data %>%
  group_by(participant_type) %>%
  summarise(
    n         = n(),
    n_age     = sum(!is.na(age)),
    n_missing = sum(is.na(age)),
    min       = min(age, na.rm = TRUE),
    max       = max(age, na.rm = TRUE),
    mean      = mean(age, na.rm = TRUE),
    sd        = sd(age, na.rm = TRUE)
  )

#### Get rid of the intermediate data set created for parent age calculation 
rm(parent_dob)

### Sex 
AAB_data %>%
    group_by(participant_type) %>%
    count(proforma_sex)

# Check for other significant variables to recover more sex/gender information
names(AAB_data) %>% grep("sex|gender", ., value = TRUE, ignore.case = TRUE)

# The coding is reversed — proforma_sex 1 = Male, ados_gender 2 = Male
AAB_data <- AAB_data %>%
  mutate(
    ados_gender_recoded = case_when(
      ados_gender == 1 ~ 2,  # ADOS 1 (Male) → proforma 2 (Female)
      ados_gender == 2 ~ 1,  # ADOS 2 (Female) → proforma 1 (Male)
      TRUE             ~ NA_real_ # if nothing is matched, assign NA to it
    )
  )

#### Recovering children sex from other questionnaires - down to 20 missing across children (15 probands, 4 siblings, 1 control)
AAB_data <- AAB_data %>%
  mutate(
    sex = case_when(
      !is.na(proforma_sex)                ~ proforma_sex,
      !is.na(fhq_b2) & fhq_b2 != 999      ~ fhq_b2,
      !is.na(ados_gender_recoded)         ~ ados_gender_recoded,
      TRUE                                ~ NA_real_ # if nothing is matched, assign NA to it
    ),
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))
  )

#### add sex for parents
AAB_data <- AAB_data %>%
  mutate(
    sex = case_when(
      !is.na(sex)              ~ sex,
      participant_type == 5    ~ factor("Female", levels = c("Male", "Female")),
      participant_type == 6    ~ factor("Male",   levels = c("Male", "Female")),
      TRUE                     ~ NA
    )
  )

# Final check all groups
AAB_data %>%
  group_by(participant_type) %>%
  summarise(
    n_male    = sum(sex == "Male",   na.rm = TRUE),
    n_female  = sum(sex == "Female", na.rm = TRUE),
    n_missing = sum(is.na(sex))
  )

# Remove intermediate variable
AAB_data$ados_gender_recoded = NULL

# Check all obsolete intermediate variables and removem them
setdiff(names(AAB_data), names(AAB_safe_copy))

AAB_data <- AAB_data %>%
  select(-parent_dob, -assessment_date)

### Site ID
AAB_data <- AAB_data %>%
  mutate(
    site = case_when(
      substr(as.character(id), 1, 2) == "11" ~ 1,  # WA
      substr(as.character(id), 1, 2) == "22" ~ 2,  # VIC
      substr(as.character(id), 1, 2) == "33" ~ 3,  # NSW
      substr(as.character(id), 1, 2) == "44" ~ 4   # QLD
    ),
    site = factor(site, levels = 1:4, labels = c("WA", "VIC", "NSW", "QLD"))
  )

# Check
AAB_data %>%
  group_by(site, participant_type) %>%
  count() %>%
  pivot_wider(names_from = participant_type, values_from = n)

# ---- Retrieve and clean diagnosis confirmation ----
## fhq_h1 codes: 1=Autism, 2=PDD-NOS, 3=Asperger, 4=Other, 5=999 (entry error)

## Inspect
AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  count(fhq_h1, useNA = "always")

## IDs with code 4 (Other) but no valid ASD diagnosis on review of fhq_h1_detail
no_asd_ids <- c(1101589, 2202623, 2202629, 2202630, 2202631,
                2202641, 2202651, 2202664, 2202673, 4406384)

## Recode
AAB_data <- AAB_data %>%
  mutate(
    diagnosis_type = case_when(
      fhq_h1 == 1                        ~ "Autism",
      fhq_h1 == 2                        ~ "PDD-NOS",
      fhq_h1 == 3                        ~ "Asperger",
      fhq_h1 == 4 & !id %in% no_asd_ids ~ "Other/Multiple",
      fhq_h1 == 4 &  id %in% no_asd_ids ~ NA_character_,  # no valid ASD diagnosis
      fhq_h1 == 5                        ~ NA_character_,  # 999 data entry error
      TRUE                               ~ NA_character_
    ),
    diagnosis_type = factor(diagnosis_type,
                            levels = c("Autism", "PDD-NOS", "Asperger", "Other/Multiple"))
  )

## Verify
AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  count(diagnosis_type, useNA = "always")

## Create the diagnosis group
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
  count(participant_type, diagnosis_group, useNA = "always")



# ---- Retrieve and clean ADOS variables ----
# Source: 'ados' form
## The AAB data team pre-computed a unified Calibrated Severity Score (CSS)
# in `ados2_comparisonscore`, covering all ADOS versions and modules:
#   ADOS-2 Modules 1–4  (ados_mod 1–4)
#   ADOS-G Modules 1–4  (ados_mod 5–8, legacy WA files pre-2014)
# The field is scored 1–10, consistent with the CSS scale (Gotham et al., 2009).

### Derive CSS from the AAB pre-computed unified comparison score
AAB_data <- AAB_data %>%
  mutate(
    ados_css = case_when(
      ados2_comparisonscore >= 1 &
        ados2_comparisonscore <= 10 ~ as.numeric(ados2_comparisonscore),
      TRUE ~ NA_real_
    )
  )

### Flag instrument version and CSS availability

AAB_data <- AAB_data %>%
  mutate(
    ados_instrument = case_when(
      ados_mod %in% 1:4 ~ "ADOS-2",
      ados_mod %in% 5:8 ~ "ADOS-G",
      TRUE              ~ NA_character_
    ),
    ados_version_flag = case_when(
      !is.na(ados_css)                    ~ "ados_css",     # primary — CSS available
      ados_mod %in% 1:8 & is.na(ados_css) ~ "ados_no_css", # administered but score absent
      TRUE                                ~ "no_ados"       # no ADOS recorded
    )
  )

### Verification
AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  group_by(ados_instrument, ados_version_flag) %>%
  summarise(
    n             = n(),
    n_css         = sum(!is.na(ados_css)),
    n_missing_css = sum(is.na(ados_css)),
    css_mean      = mean(ados_css, na.rm = TRUE),
    css_sd        = sd(ados_css, na.rm = TRUE),
    css_min       = min(ados_css, na.rm = TRUE),
    css_max       = max(ados_css, na.rm = TRUE),
    .groups       = "drop"
  )

# Site distribution of ADOS-G cases (expect WA only)
AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  count(site, ados_instrument, useNA = "always") %>%
  pivot_wider(names_from = ados_instrument, values_from = n, values_fill = 0)

# ---- Retrieve and clean CSHQ variables ----

### Vector of all CSHQ item variables
cshq_items <- c(
  "fhq_h16_a","fhq_h16_b","fhq_h16_c","fhq_h16_d","fhq_h16_e","fhq_h16_f",
  "fhq_h16_g","fhq_h16_h","fhq_h16_i","fhq_h16_j","fhq_h16_k","fhq_h16_l",
  "fhq_h16_m","fhq_h16_n","fhq_h16_o","fhq_h16_p","fhq_h16_q","fhq_h16_r",
  "fhq_h16_s","fhq_h16_t","fhq_h16_u","fhq_h16_v","fhq_h16_w","fhq_h16_x",
  "fhq_h16_y","fhq_h16_z","fhq_h16_aa","fhq_h16_ab","fhq_h16_ac","fhq_h16_ad",
  "fhq_h16_ae","fhq_h16_af","fhq_h16_ag","fhq_h16_ah"
)

### Recode 999 as NA
AAB_data <- AAB_data %>%
  mutate(across(     #across() applies a transformation to multiple columns; 
    all_of(cshq_items), # all_of() tells across() to apply to exactly the variables listed in the vector cshq_items;
                ~ ifelse(   # ifelse(condition, value_if_true, value_if_false)
                  .x == 999, NA, .x)))  # .x = the current column being processed; 

### Reverse-score the positively worded items: 1–3 becomes 3–1
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
    ~ ifelse(is.na(.x), NA, 4 - .x),   # 1→3, 2→2, 3→1
    .names = "{.col}_r" # .names = "{.col}_r" Creates new variables instead of overwriting the dataset it also appends _r suffix to each reversed item.
  )) 

### Create vectors with variables names defining subscales 

#### Bedtime Resistance
cshq_bedtime_resistance <- c(
  "fhq_h16_a_r",  # Goes to bed at same time
  "fhq_h16_b_r",  # Falls asleep in own bed
  "fhq_h16_c",  # Falls asleep in other's bed
  "fhq_h16_d",  # Needs parent in room to sleep
  "fhq_h16_e",  # Struggles at bedtime
  "fhq_h16_f"   # Afraid / shows fear of sleeping alone
)

#### Sleep Onset Delay
cshq_sleep_onset_delay <- c(
  "fhq_h16_g_r"   # Falls asleep within 20 minutes
)

#### Sleep Duration
cshq_sleep_duration <- c(
  "fhq_h16_h",  # Sleeps too little
  "fhq_h16_i_r",  # Sleeps the right amount
  "fhq_h16_j_r"   # Sleeps same amount each day
)

#### Sleep Anxiety
cshq_sleep_anxiety <- c(
  "fhq_h16_d",  # Needs parent in room to sleep (duplicate item)
  "fhq_h16_k",  # Afraid of sleeping in the dark
  "fhq_h16_l",  # Afraid of sleeping alone (duplicate concept)
  "fhq_h16_m"   # Trouble sleeping away from home
)

#### Night Wakings
cshq_night_wakings <- c(
  "fhq_h16_n",  # Moves to other's bed at night
  "fhq_h16_o",  # Awakes once during night
  "fhq_h16_p"   # Awakes more than once during the night
)

#### Parasomnias
cshq_parasomnias <- c(
  "fhq_h16_q",  # Wets the bed at night
  "fhq_h16_r",  # Talks during sleep
  "fhq_h16_s",  # Becomes restless and moves a lot during sleep
  "fhq_h16_t",  # Sleepwalks
  "fhq_h16_u",  # Grinds teeth
  "fhq_h16_v",  # Awakens screaming and/or sweating
  "fhq_h16_w"   # Express alarm at a scary dream
)

#### Sleep-Disordered Breathing
cshq_sdb <- c(
  "fhq_h16_x",  # Snores loudly
  "fhq_h16_y",  # Stops breathing
  "fhq_h16_z"   # Snorts and gasps
)

#### Daytime Sleepiness
cshq_daytime_sleepiness <- c(
  "fhq_h16_aa_r", # Wakes by himself
  "fhq_h16_ab", # Wakes up in negative mood
  "fhq_h16_ac", # Gets woken up by others
  "fhq_h16_ad", # Hard time getting out of bed
  "fhq_h16_ae", # Takes a long time to be alert
  "fhq_h16_af", # Seems tired
  "fhq_h16_ag", # Falls asleep while watching TV
  "fhq_h16_ah"  # Falls asleep while riding in a car
)

# fhq_h16_d appears in both Bedtime Resistance AND Sleep Anxiety — include once
# fhq_h16_l is conceptually identical to fhq_h16_f. fhq_h16_l is the duplicate that should be excluded
# Total = 33 unique items (not sum of subscales)

cshq_total_items <- c(
  # Bedtime Resistance (6 items)
  "fhq_h16_a_r", "fhq_h16_b_r", "fhq_h16_c", "fhq_h16_d", 
  "fhq_h16_e", "fhq_h16_f",
  # Sleep Onset Delay (1 item)
  "fhq_h16_g_r",
  # Sleep Duration (3 items)
  "fhq_h16_h", "fhq_h16_i_r", "fhq_h16_j_r",
  # Sleep Anxiety (3 items — fhq_h16_d already counted above and fhq_h16_l conceptually counted as fhq_h16_f)
  "fhq_h16_k", "fhq_h16_m",
  # Night Wakings (3 items)
  "fhq_h16_n", "fhq_h16_o", "fhq_h16_p",
  # Parasomnias (7 items)
  "fhq_h16_q", "fhq_h16_r", "fhq_h16_s", "fhq_h16_t",
  "fhq_h16_u", "fhq_h16_v", "fhq_h16_w",
  # Sleep-Disordered Breathing (3 items)
  "fhq_h16_x", "fhq_h16_y", "fhq_h16_z",
  # Daytime Sleepiness (8 items)
  "fhq_h16_aa_r", "fhq_h16_ab", "fhq_h16_ac", "fhq_h16_ad",
  "fhq_h16_ae", "fhq_h16_af", "fhq_h16_ag", "fhq_h16_ah"
)

#### Verify count
length(cshq_total_items) # should be 33

#### Compute sub and total scores
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
    
    # Total from 33 unique items directly
    cshq_total = rowSums(across(all_of(cshq_total_items)), na.rm = FALSE)
  )

#### Check
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  group_by(participant_type) %>%
  summarise(
    n         = sum(!is.na(cshq_total)),
    n_missing = sum(is.na(cshq_total)),
    min       = min(cshq_total, na.rm = TRUE),
    max       = max(cshq_total, na.rm = TRUE),
    mean      = mean(cshq_total, na.rm = TRUE),
    sd        = sd(cshq_total, na.rm = TRUE)
  )

#### Owens et al. suggest a prorating approach when ≤3 items are missing.
#### The missingness is spread fairly evenly across all 33 items there's no pattern of specific items being skipped. 

AAB_data <- AAB_data %>%
  mutate(
    n_items_present = rowSums(!is.na(across(all_of(cshq_total_items)))),
    n_items_missing = 33 - n_items_present,
    
    cshq_total_prorated = case_when(
      !is.na(cshq_total) ~ cshq_total, # catches the ones already complete
      n_items_missing > 0 & n_items_missing <= 3 ~ # Prorate if <= 3 items missing
        round(rowSums(across(all_of(cshq_total_items)), na.rm = TRUE) * 
                (33 / n_items_present), 1), # More than 3 missing — keep NA
      TRUE ~ NA_real_ # If nothing from above is fulfilled, than NA
    )
  )

# Check recovery
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  summarise(
    n_complete  = sum(!is.na(cshq_total)),
    n_prorated  = sum(!is.na(cshq_total_prorated)) - sum(!is.na(cshq_total)),
    n_missing   = sum(is.na(cshq_total_prorated)),
    mean        = mean(cshq_total_prorated, na.rm = TRUE),
    sd          = sd(cshq_total_prorated, na.rm = TRUE)
  )

AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4),
         is.na(cshq_total),
         n_items_missing > 0 & n_items_missing <= 33) %>%
  summarise(across(all_of(cshq_total_items), 
                   ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), 
               names_to = "item", 
               values_to = "n_missing") %>%
  arrange(desc(n_missing))

### Remove obsolete items
AAB_data <- AAB_data %>%
  select(-n_items_present, -n_items_missing)
rm(cshq_reverse_items, cshq_vars, cshq_total_items_33)

# ---- Family ID derivation ----
# Strategy: resolve family clusters from probands outward using
# acl_mother_id / acl_father_id (linking children → parents) and
# participant_multiplex_1:5 (linking probands → ASD siblings).
# Controls are unrelated — they receive singleton family IDs.

## Step 1: Gather child id and parent linkage from probands and ASD-Q and siblings
# (these are the rows that carry acl_mother_id / acl_father_id)
child_parent_links <- AAB_data %>%
  filter(participant_type %in% c(1, 2, 3)) %>%   # Proband, ASD-Q, Sibling
  select(id, acl_mother_id, acl_father_id) %>%
  filter(!is.na(acl_mother_id) | !is.na(acl_father_id))

## Step 2: Build an anchor key — one row per family — using the
# mother ID as the primary family anchor (most reliably present),
# falling back to father ID when mother is absent.
family_anchors <- child_parent_links %>%
  mutate(
    family_anchor = case_when(
      !is.na(acl_mother_id) ~ as.character(acl_mother_id),
      !is.na(acl_father_id) ~ as.character(acl_father_id),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(family_anchor)) %>%
  select(id, family_anchor)

## Step 3: Map every participant to its family anchor
# Children: look up their own anchor
child_family_map <- family_anchors %>%
  rename(participant_id = id)

# Parents: their own ID is the anchor when it appears as mother/father
mother_map <- child_parent_links %>%
  filter(!is.na(acl_mother_id)) %>%
  distinct(acl_mother_id) %>%
  mutate(
    participant_id = as.character(acl_mother_id),
    family_anchor  = as.character(acl_mother_id)
  ) %>%
  select(participant_id, family_anchor)

father_map <- child_parent_links %>%
  filter(!is.na(acl_father_id)) %>%
  group_by(acl_father_id) %>%
  filter(all(is.na(acl_mother_id))) %>%   # no mother ID exists for this father across any child row
  ungroup() %>%
  distinct(acl_father_id) %>%
  mutate(
    participant_id = as.character(acl_father_id),
    family_anchor  = as.character(acl_father_id)
  ) %>%
  select(participant_id, family_anchor)

# Fathers in two-parent families: inherit the mother anchor
father_in_complete_family <- child_parent_links %>%
  filter(!is.na(acl_mother_id), !is.na(acl_father_id)) %>%
  distinct(acl_mother_id, acl_father_id) %>%
  mutate(
    participant_id = as.character(acl_father_id),
    family_anchor  = as.character(acl_mother_id)
  ) %>%
  select(participant_id, family_anchor)

## Step 4: Stack all maps and deduplicate
all_family_map <- bind_rows(
  child_family_map %>% mutate(participant_id = as.character(participant_id)),
  mother_map,
  father_map,
  father_in_complete_family
) %>%
  distinct(participant_id, .keep_all = TRUE)

## Step 5: Convert family anchor to an integer family_id (cleaner for modelling)
anchor_to_fid <- all_family_map %>%
  distinct(family_anchor) %>%
  arrange(family_anchor) %>%
  mutate(family_id = row_number())

all_family_map <- all_family_map %>%
  left_join(anchor_to_fid, by = "family_anchor")

## Step 6: Join back to the main dataset
AAB_data <- AAB_data %>%
  left_join(
    all_family_map %>% 
      select(participant_id, family_id) %>%
      mutate(participant_id = as.integer(participant_id)),
    by = c("id" = "participant_id")
  )

## Step 7: Assign singleton family IDs to Comparison children (unrelated)
# They need a family_id for mixed-effects models but must not cluster
max_fid <- max(AAB_data$family_id, na.rm = TRUE) #calculate how many IDs were already attributed

AAB_data <- AAB_data %>%
  mutate(
    family_id = case_when(
      participant_type == 4 & is.na(family_id) ~
        max_fid + row_number(),   # unique ID per comparison child
      TRUE ~ family_id # Catch-all fallback — equivalent to the else in an if-else chain
    )
  )

## Approximate multiplex families: families with more than one proband or ASD-Q for sensitivity analyses
multiplex_approx <- AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  count(family_id, name = "n_asd_children") %>%
  mutate(multiplex_approx = n_asd_children > 1)

AAB_data <- AAB_data %>%
  left_join(multiplex_approx, by = "family_id")

## Flag participants with no parent IDs recorded — excluded from pedigree-based analyses
AAB_data <- AAB_data %>%
  mutate(
    pedigree_linked = case_when(
      participant_type %in% c(1, 2, 3) & is.na(acl_mother_id) & is.na(acl_father_id) ~ FALSE,
      participant_type %in% c(1, 2, 3) ~ TRUE,
      TRUE ~ NA
    )
  )

## Clean up intermediate objects
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

# Generate list of parent IDs to request from AAB data team
missing_parent_ids <- AAB_data %>%
  filter(participant_type %in% c(1, 2, 3), is.na(family_id)) %>%
  select(id, acl_mother_id, acl_father_id) %>%
  pivot_longer(cols = c(acl_mother_id, acl_father_id),
               names_to = "parent_type",
               values_to = "parent_id") %>%
  filter(!is.na(parent_id)) %>%
  distinct(parent_id) %>%
  arrange(parent_id)

# Count and inspect
nrow(missing_parent_ids)
print(missing_parent_ids)

# ---- Retrieve and clean melatonin use variables ----
# Source: fhq_section_h — "Prescribed Medications" (fhq_h13_[a-f]) and
#         "Natural Therapies" (fhq_h14_[a-f])
# Strategy: scan all free-text name fields across both sections for any
# mention of melatonin, then derive summary flags and ancillary variables.

### Identify ways that melatonin and other formulations were written
#### Select all medication-name columns (h13 & h14, a–f)
get_unique_meds <- function(df, pattern = "^fhq_h1[34]_[a-f]_name$") { # the function gets a dataframe and a regex pattern
  # The pattern itself: ^fhq_h1[34]_[a-f]_name$
    #^ and $ anchor to the full column name (no partial matches)
    #[34] matches either 3 or 4 — so both fhq_h13 and fhq_h14
    #[a-f] matches any slot letter a through f
  cols <- names(df)[grepl(pattern, names(df))] # grepl() returns a logical vector for every column name that matches the pattern
  lapply(df[cols], unique)  # lapply then applies unique() to each column individually
}

med_uniques <- get_unique_meds(AAB_data)
names(med_uniques)

#### Collapse all into one long vector + get uniques
all_unique_meds <- AAB_data[names(get_unique_meds(AAB_data))] %>%
  unlist( # unlist() collapses all columns into a single vector, so every medication name from every slot ends up in one flat structure
    use.names = FALSE) %>%  # use.names = FALSE just drops the column-derived names that unlist would create
  as.character() %>%
  trimws() %>% # removes leading whitr spaces and trailing spaces
  unique()

#### Inspect (print)
old <- getOption("max.print")
options(max.print = 999999)
all_unique_meds

#### Helper: Detect melatonin mentions in a character vector (case-insensitive)
is_melatonin <- function(x) {
  grepl(
    paste(
      "melat",          # melatonin, melatonon, melaton…, malatonin, melatorin, melatronin
      "melot",          # melotonin
      "mellot",         # mellotonin
      "circadi",        # Circadin, circadian (melatonin)
      "cirdadi",        # Cirdadin
      "melatone",       # Melatone (brand-like)
      "sleep.?tone",    # Sleep-tone, Sleeptone
      sep = "|"
    ),
    x, ignore.case = TRUE, perl = TRUE
  )
}

#### Helper: clean free-text age-at-first-use field
## Rejects calendar years (>= 1900), negatives, implausible ages (> 30)
clean_age_field <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  case_when(
    is.na(x_num)  ~ NA_real_,
    x_num >= 1900 ~ NA_real_,
    x_num < 0     ~ NA_real_,
    x_num > 30    ~ NA_real_,
    TRUE          ~ x_num
  )
}

### Step 1: Build long-format medication table (prescribed (h13) + natural (h14))

## Prescribed medications (fhq_h13) — slots a–f
prescribed_slots <- letters[1:6]

prescribed_long <- purrr::map_dfr(prescribed_slots, function(slot) {
  name_col    <- paste0("fhq_h13_", slot, "_name")
  age_col     <- paste0("fhq_h13_", slot, "_age")
  dur_col     <- paste0("fhq_h13_", slot, "_duration")
  current_col <- paste0("fhq_h13_", slot, "_current")
  reason_col  <- paste0("fhq_h13_", slot, "_reason")
  improve_col <- paste0("fhq_h13_", slot, "_improve")
  
  # Only proceed if the column exists in the dataset
  if (!name_col %in% names(AAB_data)) return(NULL)
  
  AAB_data %>%
    select(id,
           med_name    = all_of(name_col),
           med_age     = all_of(age_col),
           med_dur     = all_of(dur_col),
           med_current = all_of(current_col),
           med_reason  = all_of(reason_col),
           med_improve = all_of(improve_col)) %>%
    mutate(slot        = slot,
           med_section = "prescribed")
}) %>%
  filter(!is.na(med_name), str_trim(med_name) != "")

## Natural therapies (fhq_h14) — slots a–f
## Each slot has: _name, _age, _reason, _improve  (no _duration or _current)
natural_long <- purrr::map_dfr(prescribed_slots, function(slot) {
  name_col    <- paste0("fhq_h14_", slot, "_name")
  age_col     <- paste0("fhq_h14_", slot, "_age")
  reason_col  <- paste0("fhq_h14_", slot, "_reason")
  improve_col <- paste0("fhq_h14_", slot, "_improve")
  
  if (!name_col %in% names(AAB_data)) return(NULL)
  
  AAB_data %>%
    select(id,
           med_name    = all_of(name_col),
           med_age     = all_of(age_col),
           med_reason  = all_of(reason_col),
           med_improve = all_of(improve_col)) %>%
    mutate(slot        = slot,
           med_section = "natural",
           med_dur     = NA_character_,
           med_current = NA_real_)
}) %>%
  filter(!is.na(med_name), str_trim(med_name) != "")

## Also scan fhq_h13_other (free-text notes field) for any melatonin mentions
other_mentions <- AAB_data %>%
  filter(!is.na(fhq_h13_other),
         is_melatonin(fhq_h13_other)) %>%
  select(id, med_name = fhq_h13_other) %>%
  mutate(slot        = "other",
         med_section = "prescribed_other",
         med_age     = NA_character_,
         med_dur     = NA_character_,
         med_current = NA_real_,
         med_reason  = NA_character_,
         med_improve = NA_real_)

## Combine all sections
all_meds_long <- bind_rows(prescribed_long, natural_long, other_mentions)

#### Step 2: Flag melatonin rows

all_meds_long <- all_meds_long %>%
  mutate(is_mel = is_melatonin(med_name))

# Quick check: what did we capture?
all_meds_long %>%
  filter(is_mel) %>%
  count(med_section, med_name) %>%
  arrange(desc(n))

#### Step 3: Derive participant-level melatonin summary variables

melatonin_summary <- all_meds_long %>%
  group_by(id) %>%
  summarise(
    # Binary: ever mentioned melatonin in any medication slot
    melatonin_ever = any(is_mel, na.rm = TRUE),
    
    # Current use: 1 = Yes, 3 = Only when needed (both count as current)
    # med_current coding: 1 = Yes | 2 = No | 3 = Only when needed | 999 = missing
    melatonin_current = any(
      is_mel & med_current %in% c(1, 3),
      na.rm = TRUE
    ),
    
    # Age at first use — take the minimum across all matching slots
    # (free-text field; convert to numeric, coerce invalid strings to NA)
    melatonin_age_first = suppressWarnings(
      min(clean_age_field(med_age[is_mel]), na.rm = TRUE)
    ),
    
    # Duration string — concatenate non-missing durations (free-text)
    melatonin_duration_raw = paste(
      na.omit(med_dur[is_mel & !is.na(med_dur)]),
      collapse = "; "
    ),
    
    # Perceived improvement: 1 = No | 2 = Yes | 999 = missing
    melatonin_improved = case_when(
      any(is_mel & med_improve == 2, na.rm = TRUE) ~ 1L,  # at least one "Yes"
      any(is_mel & med_improve == 1, na.rm = TRUE) ~ 0L,  # at least one "No"
      TRUE                                          ~ NA_integer_
    ),
    
    # Source section (prescribed / natural / both)
    melatonin_source = case_when(
      any(is_mel & med_section == "prescribed", na.rm = TRUE) &
        any(is_mel & med_section == "natural",    na.rm = TRUE) ~ "both",
      any(is_mel & med_section == "prescribed", na.rm = TRUE)   ~ "prescribed",
      any(is_mel & med_section == "natural",    na.rm = TRUE)   ~ "natural",
      TRUE                                                       ~ NA_character_
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Recode Inf (from min() on all-NA) back to NA
    melatonin_age_first    = if_else(is.infinite(melatonin_age_first),
                                     NA_real_, melatonin_age_first),
    # Empty string to NA for duration
    melatonin_duration_raw = if_else(melatonin_duration_raw == "",
                                     NA_character_, melatonin_duration_raw),
    # Convert logicals to integer flags (0/1)
    melatonin_ever    = as.integer(melatonin_ever),
    melatonin_current = as.integer(melatonin_current),
    # Factor labels
    melatonin_current_f = factor(melatonin_current,
                                 levels = c(0, 1),
                                 labels = c("No", "Yes"))
  )

# Some participants (n = 3) typed their full medication history into a single name field
# rather than using separate slots — melatonin_ever is valid for these participants
# but melatonin_age_first, melatonin_current, and melatonin_duration_raw are not

multi_entry_ids <- all_meds_long %>%
  filter(is_mel, str_count(med_name, "[,;]") >= 2) %>%
  pull(id) %>%
  unique()

#### Step 4: Join back to main dataset
AAB_data <- AAB_data %>%
  left_join(melatonin_summary, by = "id") %>%
  mutate(
    # Participants with no melatonin row default to 0/No
    melatonin_ever    = replace_na(melatonin_ever, 0L),
    melatonin_current = replace_na(melatonin_current, 0L),
    melatonin_current_f = factor(melatonin_current,
                                 levels = c(0, 1),
                                 labels = c("No", "Yes")),
    melatonin_entry_concat = as.integer(id %in% multi_entry_ids)
  )

#### Step 5: Verification 
##### Counts by participant type and melatonin use status
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  group_by(participant_type_f, melatonin_ever, melatonin_current) %>%
  count() %>%
  print(n = 50)

##### Age at first use distribution among users
AAB_data %>%
  filter(melatonin_ever == 1) %>%
  summarise(
    n         = n(),
    n_age     = sum(!is.na(melatonin_age_first)),
    min_age   = min(melatonin_age_first, na.rm = TRUE),
    max_age   = max(melatonin_age_first, na.rm = TRUE),
    mean_age  = mean(melatonin_age_first, na.rm = TRUE),
    sd_age    = sd(melatonin_age_first, na.rm = TRUE)
  )

## Cross-tabulate melatonin use with diagnosis group (children only)
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4), !is.na(diagnosis_group)) %>%
  count(diagnosis_group, melatonin_ever) %>%
  pivot_wider(names_from = melatonin_ever, values_from = n,
              names_prefix = "mel_ever_")

## Clean up intermediate objects
rm(prescribed_long, natural_long, other_mentions, all_meds_long, melatonin_summary,
   prescribed_slots, is_melatonin)

## Concatenated entry flag
AAB_data %>%
  filter(melatonin_entry_concat == 1) %>%
  count(participant_type_f, melatonin_current)

### Clean up intermediate objects
rm(prescribed_long, natural_long, other_mentions, all_meds_long,
   melatonin_summary, prescribed_slots, multi_entry_ids,
   is_melatonin, clean_age_field)

# ---- Retrieve and clean Tanner stage variables ----
# Source: 'tanner' form — tanner_sex, tanner_boy_genital, tanner_boy_pubichair,
#         tanner_girl_genital, tanner_girl_pubichair
# Admin flag: acl_tanner_completed (assessment_checklist form)
# Supplementary: 3Di interview items q1044, q1046, q1049, q1050
#
# Strategy: derive a sex-unified composite Tanner stage for each child
# participant, using the two domain ratings (genital/breast + pubic hair)
# that are sex-specific. Flag missingness sources for sensitivity analyses.


### Inspect completion flag
# acl_tanner_completed: 1=Yes | 0=No | 2=N/A | 999=missing/error
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(acl_tanner_completed, useNA = "always")

### Recode 999 to NA across all Tanner item variables
tanner_items <- c(
  "tanner_sex",
  "tanner_boy_genital",
  "tanner_boy_pubichair",
  "tanner_girl_genital",
  "tanner_girl_pubichair"
)

AAB_data <- AAB_data %>%
  mutate(across(
    all_of(tanner_items),
    ~ ifelse(.x == 999, NA, .x)
  ))

# Also clean the completion flag
AAB_data <- AAB_data %>%
  mutate(
    acl_tanner_completed = ifelse(acl_tanner_completed == 999, NA, acl_tanner_completed)
  )

### Check sex concordance between tanner_sex and sex
# tanner_sex coding: 1 = Boys, 2 = Girls
# sex (derived earlier): 1 = Male, 2 = Female  — same numeric coding
# Flagging mismatches allows for manual review before computing composites.

AAB_data <- AAB_data %>%
  mutate(
    tanner_sex_mismatch = case_when(
      participant_type %in% c(5, 6)     ~ NA,              # parents: not applicable
      is.na(tanner_sex)                 ~ NA,              # no tanner sex recorded
      is.na(as.integer(sex))            ~ NA,              # no derived sex
      tanner_sex != as.integer(sex)     ~ TRUE,
      TRUE                              ~ FALSE
    )
  )

# Inspect mismatches (there's 1 case that can be solved with PLINK later)
AAB_data %>%
  filter(tanner_sex_mismatch == TRUE) %>%
  select(id, participant_type_f, sex, tanner_sex)

# Mismatched participant id == 1131608

### Tanner genital score: single unified variable
# Rationale: genital/breast development reflects HPG axis maturation and is
# preferred over pubic hair, which depends on the independent adrenal axis
# (Emmanuel & Bokor, 2022; StatPearls). Consistent with Yap et al. (2023,
# Nat Med) who used tanner_genital as the pubertal covariate in the AAB.

AAB_data <- AAB_data %>%
  mutate(
    tanner_genital = case_when(
      participant_type %in% c(5, 6)     ~ NA_real_,  # parents: not applicable
      tanner_sex_resolved == 1          ~ as.numeric(tanner_boy_genital),   # boys: penis/testicular
      tanner_sex_resolved == 2          ~ as.numeric(tanner_girl_genital),  # girls: breast
      TRUE                              ~ NA_real_
    ),
    # Ordered factor version for modelling
    tanner_genital_f = factor(
      tanner_genital,
      levels = 1:5,
      labels = c("Stage 1", "Stage 2", "Stage 3", "Stage 4", "Stage 5"),
      ordered = TRUE
    )
  )

# ---- Verification
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  group_by(participant_type_f, sex) %>%
  summarise(
    n          = n(),
    n_complete = sum(!is.na(tanner_genital)),
    n_missing  = sum(is.na(tanner_genital)),
    mean       = mean(tanner_genital, na.rm = TRUE),
    sd         = sd(tanner_genital, na.rm = TRUE),
    .groups    = "drop"
  )

AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(tanner_genital_f, useNA = "always")


### Create tanner_missing_reason from existing variables
AAB_data <- AAB_data %>%
  mutate(
    tanner_missing_reason = case_when(
      participant_type %in% c(5, 6)                                          ~ "not_applicable",
      !is.na(tanner_genital)                                                 ~ "complete",
      id == 1131608                                                           ~ "sex_scale_mismatch_genital_not_mappable",
      acl_tanner_completed == 0                                              ~ "assessment_not_done",
      acl_tanner_completed == 2                                              ~ "not_applicable_na",
      acl_tanner_completed == 1 & is.na(tanner_genital)                     ~ "done_but_items_missing",
      is.na(acl_tanner_completed)                                            ~ "no_checklist_record",
      TRUE                                                                   ~ "unknown"
    )
  )

# Drop intermediate variables
AAB_data <- AAB_data %>%
  select(-age_at_assessment_clean, -age_parent,
         -tanner_sex_mismatch, -tanner_sex_resolved)

# ---- Retrieve and clean ethnicity variables ----
# Source: fhq_sections_a_b — Biological Mother (fhq_b8) and Biological Father (fhq_b23)
# Strategy: assign maternal ethnicity as primary proxy for child ethnicity,
# falling back to paternal ethnicity when maternal is missing.
# Rationale: consistent with established convention in paediatric cohort research
# (Vedelli et al., 2024). Genomic ancestry PCs will replace self-reported
# ethnicity in all genetic analyses.

## Inspect raw variables
AAB_data %>%
  count(fhq_b8,  useNA = "always")   # maternal ethnicity
AAB_data %>%
  count(fhq_b23, useNA = "always")   # paternal ethnicity

## Recode 999 as NA for both parent ethnicity fields
AAB_data <- AAB_data %>%
  mutate(
    fhq_b8_clean  = if_else(fhq_b8  == 999, NA_real_, as.numeric(fhq_b8)),
    fhq_b23_clean = if_else(fhq_b23 == 999, NA_real_, as.numeric(fhq_b23))
  )

## Derive proxy ethnicity: maternal first, paternal fallback
# Coding: 1=Caucasian | 2=Aboriginal | 3=Asian | 4=Maori/Pacific Islander | 5=Other
AAB_data <- AAB_data %>%
  mutate(
    ethnicity_source = case_when(
      !is.na(fhq_b8_clean)  ~ "mother",   # maternal available → use it
      !is.na(fhq_b23_clean) ~ "father",   # maternal missing, paternal available
      TRUE                  ~ NA_character_
    ),
    ethnicity_raw = case_when(
      !is.na(fhq_b8_clean)  ~ fhq_b8_clean,   # maternal first
      !is.na(fhq_b23_clean) ~ fhq_b23_clean,  # paternal fallback
      TRUE                  ~ NA_real_
    ),
    ethnicity_f = factor(
      ethnicity_raw,
      levels = 1:5,
      labels = c("Caucasian", "Aboriginal", "Asian", "Maori/Pacific Islander", "Other")
    )
  )

## Check: how many children received maternal vs paternal vs no ethnicity
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(ethnicity_source, useNA = "always")

## Check: distribution of ethnicity by participant type
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(participant_type_f, ethnicity_f, useNA = "always") %>%
  pivot_wider(names_from = ethnicity_f, values_from = n, values_fill = 0)

## Flag discordant parental ethnicity (both present but different)
# Useful for sensitivity analyses and to document in methods
AAB_data <- AAB_data %>%
  mutate(
    ethnicity_discordant = case_when(
      !is.na(fhq_b8_clean) & !is.na(fhq_b23_clean) &
        fhq_b8_clean != fhq_b23_clean ~ TRUE,
      !is.na(fhq_b8_clean) & !is.na(fhq_b23_clean) &
        fhq_b8_clean == fhq_b23_clean ~ FALSE,
      TRUE ~ NA  # one or both parents missing — cannot assess concordance
    )
  )

## How many discordant families?
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(ethnicity_discordant, useNA = "always")

## Remove intermediate cleaning columns
AAB_data <- AAB_data %>%
  select(-fhq_b8_clean, -fhq_b23_clean)

## Propagate ethnicity within families
# The FHQ is completed once per family (typically by the proband's parent).
# Siblings and some controls therefore have NA on fhq_b8/fhq_b23 in their
# own row. Strategy: derive ethnicity from each child's own row first;
# if still missing, inherit from another child in the same family who has it.

### Build a family-level ethnicity lookup from all available child rows
family_ethnicity <- AAB_data %>%
  filter(
    participant_type %in% c(1, 2, 3, 4),
    !is.na(family_id),
    !is.na(ethnicity_raw)             # only rows that already have ethnicity
  ) %>%
  group_by(family_id) %>%
  slice(1) %>%                        # one row per family (all members share same FHQ)
  ungroup() %>%
  select(
    family_id,
    ethnicity_raw_fam    = ethnicity_raw,
    ethnicity_f_fam      = ethnicity_f,
    ethnicity_source_fam = ethnicity_source,
    ethnicity_discordant_fam = ethnicity_discordant
  )

### Join family-level ethnicity back and fill gaps
AAB_data <- AAB_data %>%
  left_join(family_ethnicity, by = "family_id") %>%
  mutate(
    # Track the resolution path
    ethnicity_resolution = case_when(
      !is.na(ethnicity_raw)     ~ "own_row",     # child's own FHQ had it
      !is.na(ethnicity_raw_fam) ~ "family_propagated", # inherited from family
      TRUE                      ~ NA_character_
    ),
    # Fill ethnicity variables where missing
    ethnicity_raw         = coalesce(ethnicity_raw,     ethnicity_raw_fam),
    ethnicity_f           = coalesce(ethnicity_f,       ethnicity_f_fam),
    ethnicity_source      = coalesce(ethnicity_source,  ethnicity_source_fam),
    ethnicity_discordant  = coalesce(ethnicity_discordant, ethnicity_discordant_fam)
  ) %>%
  select(-ethnicity_raw_fam, -ethnicity_f_fam,
         -ethnicity_source_fam, -ethnicity_discordant_fam)

### Verify recovery
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(ethnicity_resolution, useNA = "always")

### Check final distribution by participant type
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(participant_type_f, ethnicity_f, useNA = "always") %>%
  pivot_wider(names_from = ethnicity_f, values_from = n, values_fill = 0)

### Clean up
rm(family_ethnicity)

## Collapse ethnicity for regression analyses
# Rationale: Aboriginal (n≈11) and Maori/Pacific Islander (n≈14) are too
# small for stable regression estimates. Caucasian is the clear reference
# group. Asian (n≈116) and Other (n≈103) are retained as separate categories.
# Aboriginal and Maori/Pacific Islander are combined into a single
# "Indigenous/Pacific" category to preserve their distinction from the
# generic Other category while achieving minimal cell sizes.
#
# A binary sensitivity variable (Caucasian vs non-Caucasian) is also
# created for robustness checks in models where even the collapsed
# groups are too small (e.g., subgroup analyses).
#
# NOTE: Collapsing Aboriginal Australians with any other group requires
# explicit acknowledgement in methods and limitations per NHMRC guidelines
# on research involving Aboriginal and Torres Strait Islander peoples.

## Verify current counts before collapsing
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(ethnicity_f, useNA = "always") %>%
  arrange(desc(n))

## Collapsed variable for regression (4 levels)
AAB_data <- AAB_data %>%
  mutate(
    ethnicity_collapsed = case_when(
      ethnicity_f == "Caucasian"               ~ "Caucasian",
      ethnicity_f == "Asian"                   ~ "Asian",
      ethnicity_f %in% c("Aboriginal",
                         "Maori/Pacific Islander") ~ "Indigenous/Pacific",
      ethnicity_f == "Other"                   ~ "Other",
      TRUE                                     ~ NA_character_
    ),
    ethnicity_collapsed = factor(
      ethnicity_collapsed,
      levels = c("Caucasian",          # reference group
                 "Asian",
                 "Indigenous/Pacific",
                 "Other")
    )
  )

## Binary sensitivity variable (Caucasian vs non-Caucasian)
AAB_data <- AAB_data %>%
  mutate(
    ethnicity_binary = case_when(
      ethnicity_f == "Caucasian" ~ "Caucasian",
      !is.na(ethnicity_f)        ~ "Non-Caucasian",
      TRUE                       ~ NA_character_
    ),
    ethnicity_binary = factor(
      ethnicity_binary,
      levels = c("Caucasian", "Non-Caucasian")  # Caucasian as reference
    )
  )

## Verify final distributions
AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(ethnicity_collapsed, useNA = "always")

AAB_data %>%
  filter(participant_type %in% c(1, 2, 3, 4)) %>%
  count(ethnicity_binary, useNA = "always")

# ---- SES variable derivation ----

# Sources: fhq_b36 (income), fhq_b10 (maternal edu), fhq_b25 (paternal edu)
# Occupation (fhq_b12, fhq_b27) deferred — requires ANZSCO coding
# Unit: family level (derived from proband row, propagated via family_id)

### Clean income
# fhq_b36: 12-point ordinal (1 = $1–$8k, 12 = >$104k)
# Code 13 = "prefer not to say"; 999 = missing/error → both to NA

AAB_data <- AAB_data %>%
  mutate(
    income_raw = case_when(
      fhq_b36 %in% c(13, 999) ~ NA_real_,
      TRUE                    ~ as.numeric(fhq_b36)
    )
  )

# Inspect
AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  count(income_raw, useNA = "always") %>%
  arrange(income_raw)

### Clean education
# fhq_b10 (mother) and fhq_b25 (father): 7-point ordinal
# 1=<10yrs school, 2=10yrs, 3=11yrs, 4=12yrs, 5=Trade/Cert, 6=Degree, 7=Other
# 999 → NA; code 7 (Other) → NA (too ambiguous without detail)

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
    # Take the higher of the two parents as household education level
    # (standard in family-based studies; reflects maximum resource available to the child)
    edu_max = pmax(edu_mother, edu_father, na.rm = TRUE)
  )

# Inspect
AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  summarise(
    n_mother_edu  = sum(!is.na(edu_mother)),
    n_father_edu  = sum(!is.na(edu_father)),
    n_edu_max     = sum(!is.na(edu_max)),
    n_income      = sum(!is.na(income_raw))
  )

### Derive family-level SES variables from proband row
# All SES components are reported by parent on proband/ASD-Q forms only
# → derive once per family and propagate to siblings and parents via family_id

family_ses_raw <- AAB_data %>%
  filter(participant_type %in% c(1, 2),  # proband/ASD-Q rows carry the FHQ
         !is.na(family_id)) %>%
  arrange(family_id, participant_type) %>%  # type 1 (proband) first
  group_by(family_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(family_id, income_raw, edu_mother, edu_father, edu_max)

# Check coverage at family level
family_ses_raw %>%
  summarise(
    n_families     = n(),
    n_income       = sum(!is.na(income_raw)),
    n_edu_max      = sum(!is.na(edu_max)),
    n_both         = sum(!is.na(income_raw) & !is.na(edu_max)),
    pct_income     = round(mean(!is.na(income_raw)) * 100, 1),
    pct_edu        = round(mean(!is.na(edu_max)) * 100, 1),
    pct_both       = round(mean(!is.na(income_raw) & !is.na(edu_max)) * 100, 1)
  )

### Build composite SES
# Method: Equal-weight z-score average (income + edu_max)
# Transparent, easily reported, robust to small N
# Reference: Avvisati et al. 2020 PISA ESCS equal-weighting rationale

family_ses_raw <- family_ses_raw %>%
  mutate(
    income_z  = as.numeric(scale(income_raw)),
    edu_max_z = as.numeric(scale(edu_max)),
    
    # Composite: mean of available z-scores
    # na.rm = FALSE → NA if ANY component missing (complete-case)
    ses_composite_complete = rowMeans(
      cbind(income_z, edu_max_z),
      na.rm = FALSE
    ),
    
    # Composite: mean of available z-scores allowing partial
    # (use only when missingness <50% of components — flag for sensitivity)
    ses_composite_partial = rowMeans(
      cbind(income_z, edu_max_z),
      na.rm = TRUE
    ),
    
    # Flag: how many components went into the partial composite
    ses_n_components = (!is.na(income_z)) + (!is.na(edu_max_z))
  )

# Check distribution
family_ses_raw %>%
  summarise(
    n_complete = sum(!is.na(ses_composite_complete)),
    n_partial  = sum(!is.na(ses_composite_partial)),
    n_single   = sum(ses_n_components == 1, na.rm = TRUE),
    mean_c     = mean(ses_composite_complete, na.rm = TRUE),
    sd_c       = sd(ses_composite_complete, na.rm = TRUE)
  )

### Remove intermediate SES objects and variables
rm(family_ses_raw)

AAB_data <- AAB_data %>%
  select(-edu_mother, -edu_father)
