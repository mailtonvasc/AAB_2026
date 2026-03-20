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