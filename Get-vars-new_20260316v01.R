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
      # Already complete
      !is.na(cshq_total) ~ cshq_total,
      # Prorate if <= 3 items missing
      n_items_missing > 0 & n_items_missing <= 3 ~
        round(rowSums(across(all_of(cshq_total_items)), na.rm = TRUE) * 
                (33 / n_items_present), 1),
      # Too many missing — keep NA
      TRUE ~ NA_real_
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







