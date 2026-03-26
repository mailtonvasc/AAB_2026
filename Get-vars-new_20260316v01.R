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
get_unique_meds <- function(df, pattern = "^fhq_h1[34]_[a-f]_name$") {
  cols <- names(df)[grepl(pattern, names(df))]
  lapply(df[cols], unique)
}

med_uniques <- get_unique_meds(AAB_data)
names(med_uniques)

#### Collapse all into one long vector + get uniques
all_unique_meds <- AAB_data[med_name_cols] |>
  unlist(use.names = FALSE) |>
  as.character() |>
  trimws() |>
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
