# title:          "Sleep disruption as an endophenotype of Autism"
# purpose:        "Output for FamilySLeeps"
# date:           "20/01/2026"
# author:         "Mailton Vasconcelos"

# ---- Packages ----
library(tidyr)
library(dplyr)
library(readxl) #for reading .csv file
library(visdat) #for inspecting missingness
library(lubridate)
library(stringr) #for trimming

# ---- Load the data ----
#AAB_data <- read.csv("AustralianAutismBiob-AccessRequest2024047_DATA_2025-05-14_1023.csv",  stringsAsFactors = FALSE)
AAB_safe_copy <- AAB_data

## ---- Retrieve and clean demographic variables ----
### Participant type
table(AAB_data$participant_type, useNA = "always")
unique(AAB_data$participant_type) #check variable for placeholders like 999
summary(AAB_data$participant_type)
str(AAB_data$participant_type)

#### Recode participant type to factor
AAB_data <- AAB_data %>%
  filter(!is.na(id), !is.na(participant_type)) %>%
  mutate(
    participant_type_f = factor(participant_type,
                                levels = 1:6,
                                labels = c("Proband", "ASD-Q", "Sibling", "Control", "Mother", "Father"))
  )

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

# Verification
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

#### Calculate age for parents from children data
parent_dob <- AAB_data %>%
  filter(participant_type %in% c(1, 2)) %>%
  select(acl_mother_id, acl_father_id, fhq_b9, fhq_b24_dob, proforma_doa_d) %>%
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
  select(parent_id, parent_dob, assessment_date = proforma_doa_d) # creates the data subset parent_dob only with relevant variables

#### Join and compute age of parents using renamed column
AAB_data <- AAB_data %>%
  left_join(parent_dob, by = c("id" = "parent_id")) %>%
  mutate(
    age_parent = as.numeric(interval(parent_dob, assessment_date) / years(1))
  )

#### Single unified age variable across all participant types
AAB_data <- AAB_data %>%
  mutate(
    age = case_when(
      participant_type %in% c(1, 2, 3, 4) ~ age_at_assessment,
      participant_type %in% c(5, 6)        ~ age_parent
    )
  )

# Final check across all groups
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

### Sex 
AAB_data %>%
    group_by(participant_type) %>%
    count(proforma_sex)

#### Recovering children sex from other questionnaires - down to 20 missing across children (15 probands, 4 siblings, 1 control)
AAB_data <- AAB_data %>%
  mutate(
    sex = case_when(
      !is.na(proforma_sex) ~ proforma_sex,
      !is.na(fhq_b2)       ~ fhq_b2,
      TRUE                 ~ NA_real_
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

## ---- Retrieve and clean CSHQ variables for Step 1 ----

### 1. Vector of all CSHQ item variables
cshq_items <- c(
  "fhq_h16_a","fhq_h16_b","fhq_h16_c","fhq_h16_d","fhq_h16_e","fhq_h16_f",
  "fhq_h16_g","fhq_h16_h","fhq_h16_i","fhq_h16_j","fhq_h16_k","fhq_h16_l",
  "fhq_h16_m","fhq_h16_n","fhq_h16_o","fhq_h16_p","fhq_h16_q","fhq_h16_r",
  "fhq_h16_s","fhq_h16_t","fhq_h16_u","fhq_h16_v","fhq_h16_w","fhq_h16_x",
  "fhq_h16_y","fhq_h16_z","fhq_h16_aa","fhq_h16_ab","fhq_h16_ac","fhq_h16_ad",
  "fhq_h16_ae","fhq_h16_af","fhq_h16_ag","fhq_h16_ah"
)

### 2, Recode 999 as NA
AAB_data_recoded <- AAB_data %>%
  mutate(across(
    all_of(cshq_items),
                ~ ifelse(.x == 999, NA, .x))) # across() applies a transformation to multiple columns; all_of() tells across() to apply to exactly the variables listed in the vector cshq_items; .x = the current column being processed; ifelse(condition, value_if_true, value_if_false)

### 3. Reverse-score the positively worded items: 1–3 becomes 3–1
cshq_reverse_items <- c(
  "fhq_h16_a",   # Goes to bed at same time
  "fhq_h16_b",   # Falls asleep in own bed
  "fhq_h16_g",   # Falls asleep within 20 minutes
  "fhq_h16_i",   # Sleeps the right amount
  "fhq_h16_j",   # Sleeps same amount each day
  "fhq_h16_aa"   # Wakes by himself
)

AAB_data_recoded <- AAB_data_recoded %>%
  mutate(across(
    all_of(cshq_reverse_items),
    ~ ifelse(is.na(.x), NA, 4 - .x),   # 1→3, 2→2, 3→1
    .names = "{.col}_r"
  )) # .names = "{.col}_r" Creates new variables instead of overwriting the dataset. Appends _r suffix to each reversed item.

### 4. Create vectors with variables names defining subscales 

#### 4.1 Bedtime Resistance
cshq_bedtime_resistance <- c(
  "fhq_h16_a_r",  # Goes to bed at same time
  "fhq_h16_b_r",  # Falls asleep in own bed
  "fhq_h16_c",  # Falls asleep in other's bed
  "fhq_h16_d",  # Needs parent in room to sleep
  "fhq_h16_e",  # Struggles at bedtime
  "fhq_h16_f"   # Afraid / shows fear of sleeping alone
)

#### 4.2 Sleep Onset Delay
cshq_sleep_onset_delay <- c(
  "fhq_h16_g_r"   # Falls asleep within 20 minutes
)

#### 4.3 Sleep Duration
cshq_sleep_duration <- c(
  "fhq_h16_h",  # Sleeps too little
  "fhq_h16_i_r",  # Sleeps the right amount
  "fhq_h16_j_r"   # Sleeps same amount each day
)

#### 4.4 Sleep Anxiety
cshq_sleep_anxiety <- c(
  "fhq_h16_d",  # Needs parent in room to sleep (duplicate item)
  "fhq_h16_k",  # Afraid of sleeping in the dark
  "fhq_h16_l",  # Afraid of sleeping alone (duplicate concept)
  "fhq_h16_m"   # Trouble sleeping away from home
)

#### 4.5 Night Wakings
cshq_night_wakings <- c(
  "fhq_h16_n",  # Moves to other's bed at night
  "fhq_h16_o",  # Awakes once during night
  "fhq_h16_p"   # Awakes more than once during the night
)

#### 4.6 Parasomnias
cshq_parasomnias <- c(
  "fhq_h16_q",  # Wets the bed at night
  "fhq_h16_r",  # Talks during sleep
  "fhq_h16_s",  # Becomes restless and moves a lot during sleep
  "fhq_h16_t",  # Sleepwalks
  "fhq_h16_u",  # Grinds teeth
  "fhq_h16_v",  # Awakens screaming and/or sweating
  "fhq_h16_w"   # Express alarm at a scary dream
)

#### 4.7 Sleep-Disordered Breathing
cshq_sdb <- c(
  "fhq_h16_x",  # Snores loudly
  "fhq_h16_y",  # Stops breathing
  "fhq_h16_z"   # Snorts and gasps
)

#### 4.8 Daytime Sleepiness
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

## ---- Retrieve and clean Demographic variables for Step 1 ----
### 1 Participant type

table(AAB_data$participant_type, useNA = "always") #check variable
unique(AAB_data$participant_type) #check variable for placeholders like 999
sum(is.na(AAB_data$participant_type))
AAB_data %>%
  filter(is.na(participant_type)) %>%
  View()

AAB_data_recoded <- AAB_data %>%
  filter(!is.na(participant_type)) %>%
  mutate(participant_type = factor(participant_type,
                              levels = c(1, 2, 3, 4, 5, 6),
                              labels = c("Proband", "ASD-Q", "Sibling", 
                                         "Comparison", "Mother", "Father")))

### 2. Clean and parse date variables for age
#Develop code and inspect the variable first

### 3. Recode child's gender (fhq_b2) and find a parent one
#Inspect the variable first
table(AAB_data$fhq_b2, useNA = "always") #check variable
unique(AAB_data$fhq_b2) #check variable for placeholders like 999
table(AAB_data$tanner_sex, useNA = "always") #check variable
unique(AAB_data$tanner_sex) #check variable for placeholders like 999
table(AAB_data$ccamo_gender, useNA = "always") #gender for mother from CCA
table(AAB_data$ccafa_gender, useNA = "always") #gender for father from CCA




#Recode the variables and create a single sex one
AAB_data_recoded <- AAB_data_recoded %>%
  mutate(
    # Clean ccamo_gender (Mother)
    ccamo_gender_clean = case_when(
      tolower(str_trim(ccamo_gender)) %in% c("f", "female") ~ 2,
      tolower(str_trim(ccamo_gender)) %in% c("m", "male") ~ 1,
      str_trim(ccamo_gender) == "999" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    ccafa_gender_clean = case_when(
      tolower(str_trim(ccafa_gender)) %in% c("f", "female") ~ 2,
      tolower(str_trim(ccafa_gender)) %in% c("m", "male") ~ 1,
      str_trim(ccafa_gender) == "999" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # Recode ados_gender to match 1 = Male, 2 = Female since it is inverted
    ados_gender_recoded = case_when(
      ados_gender == 1 ~ 2,
      ados_gender == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Clean fhq_b2
    fhq_b2_clean = na_if(as.numeric(fhq_b2), 999),
    
    # Build unified sex variable
    sex = case_when(
      participant_type %in% c("Proband", "Sibling", "ASD-Q", "Comparison") ~
        coalesce(fhq_b2_clean, ados_gender_recoded, as.numeric(tanner_sex)),
      participant_type %in% c("Mother") ~
        ccamo_gender_clean,
      participant_type %in% c("Father") ~
        ccafa_gender_clean,
      TRUE ~ NA_real_ # for anything that didn't match the conditions above, assign NA
    ),
    
    # Convert to factor
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))
  ) %>%
  select(-fhq_b2_clean, -ados_gender_recoded, -ccamo_gender_clean, -ccafa_gender_clean)

table(AAB_data_recoded$sex, AAB_data_recoded$participant_type, useNA = "always")

Proband ASD-Q Sibling Comparison Mother Father <NA>
  Male       897     6      91         54      0      0    0
Female     248     9      98         63      0      0    0
<NA>         9     0      72         32    431    298    0

### -------------------------------------------------------------
#I still need to fix that there's 9 probands with NA, 72 siblings and father and mother

### 6) Compute subscales and total CHSQ scores

AAB_data_recoded <- AAB_data_recoded %>%
  mutate(
    s_cshq_bedtime_resistance = rowSums(across(all_of(cshq_bedtime_resistance)), na.rm = TRUE),
    s_cshq_sleep_onset_delay  = rowSums(across(all_of(cshq_sleep_onset_delay)),  na.rm = TRUE),
    s_cshq_sleep_duration     = rowSums(across(all_of(cshq_sleep_duration)),     na.rm = TRUE),
    s_cshq_sleep_anxiety      = rowSums(across(all_of(cshq_sleep_anxiety)),      na.rm = TRUE),
    s_cshq_night_wakings      = rowSums(across(all_of(cshq_night_wakings)),      na.rm = TRUE),
    s_cshq_parasomnias        = rowSums(across(all_of(cshq_parasomnias)),        na.rm = TRUE),
    s_cshq_sdb                = rowSums(across(all_of(cshq_sdb)),                na.rm = TRUE),
    s_cshq_daytime_sleepiness = rowSums(across(all_of(cshq_daytime_sleepiness)), na.rm = TRUE)
  )

cshq_total_items_33 <- c(
  
  # 1. Bedtime Resistance (6 items, keep fhq_h16_f but not fhq_h16_l here)
  "fhq_h16_a_r",
  "fhq_h16_b_r",
  "fhq_h16_c",
  "fhq_h16_d",   # included ONCE in total score
  "fhq_h16_e",
  "fhq_h16_f",   # keep this version of "afraid sleeping alone"
  
  # 2. Sleep Onset Delay (1 item)
  "fhq_h16_g_r",
  
  # 3. Sleep Duration (3 items)
  "fhq_h16_h",
  "fhq_h16_i_r",
  "fhq_h16_j_r",
  
  # 4. Sleep Anxiety (exclude duplicate d + duplicate l)
  "fhq_h16_k",
  "fhq_h16_m",
  
  # 5. Night Wakings (3 items)
  "fhq_h16_n",
  "fhq_h16_o",
  "fhq_h16_p",
  
  # 6. Parasomnias (7 items)
  "fhq_h16_q",
  "fhq_h16_r",
  "fhq_h16_s",
  "fhq_h16_t",
  "fhq_h16_u",
  "fhq_h16_v",
  "fhq_h16_w",
  
  # 7. Sleep‑Disordered Breathing (3 items)
  "fhq_h16_x",
  "fhq_h16_y",
  "fhq_h16_z",
  
  # 8. Daytime Sleepiness (8 items)
  "fhq_h16_aa_r",
  "fhq_h16_ab",
  "fhq_h16_ac",
  "fhq_h16_ad",
  "fhq_h16_ae",
  "fhq_h16_af",
  "fhq_h16_ag",
  "fhq_h16_ah"
)

#--- Define variables of interest for step 1a ---
step_1_vars <- c(
  "sleep_chsq_total",
  "sleep_good_poor_sleep",
  "sleep_bedtime_resistance",
  "sleep_onset_delay",
  "sleep_duration",
  "sleep_anxiety",
  "sleep_night_wakings",
  "sleep_parasomnias",
  "sleep_disordered_breathing",
  "sleep_daytime_sleepiness",
  "id",
  "participant_type",
#  "site", # cannot find site on dataset
  "fhq_b1", # date of birth
  "acl_fhq_date", #date of dob collection during Family History Questionnaire
  "fhq_b2", # participant gender
  ""
)


AAB_data_recoded <- AAB_data_recoded %>%
  mutate(
    cshq_total_33 = rowSums(across(all_of(cshq_total_items_33)), 
                            na.rm = TRUE)
  )


vars_to_extract <- c(
  # Grouping variables
  "id", "participant_type", "fhq_b1", "fhq_b2", "acl_fhq_date",
  "gender", "age_at_fhq",
  
  # Raw CSHQ items
  "fhq_h16_a","fhq_h16_b","fhq_h16_c","fhq_h16_d","fhq_h16_e",
  "fhq_h16_f","fhq_h16_g","fhq_h16_h","fhq_h16_i","fhq_h16_j",
  "fhq_h16_k","fhq_h16_l","fhq_h16_m","fhq_h16_n","fhq_h16_o",
  "fhq_h16_p","fhq_h16_q","fhq_h16_r","fhq_h16_s","fhq_h16_t",
  "fhq_h16_u","fhq_h16_v","fhq_h16_w","fhq_h16_x","fhq_h16_y",
  "fhq_h16_z","fhq_h16_aa","fhq_h16_ab","fhq_h16_ac","fhq_h16_ad",
  "fhq_h16_ae","fhq_h16_af","fhq_h16_ag","fhq_h16_ah",
  
  # Reverse-scored versions
  "fhq_h16_a_r","fhq_h16_b_r","fhq_h16_g_r",
  "fhq_h16_i_r","fhq_h16_j_r","fhq_h16_aa_r",
  
  # Subscale scores
  "s_cshq_bedtime_resistance", "s_cshq_sleep_onset_delay",
  "s_cshq_sleep_duration", "s_cshq_sleep_anxiety",
  "s_cshq_night_wakings", "s_cshq_parasomnias",
  "s_cshq_sdb", "s_cshq_daytime_sleepiness",
  
  # Total CSHQ scores
  "cshq_total_33"
)

AAB_step_1 <- AAB_data_recoded %>% select(all_of(vars_to_extract))

# --- Inspect data structure ---
str(AAB_step_1)