# Get the app-based substance use questions from the checking in survey

library(readr)
library(dplyr)

checkin <- read_csv('data/raw/Michigan_LS_checking_in_1_22_17.csv')
surveys <- read_rds('data/raw/Michigan_DB_surveyentries_02_01_17.rds')
twin_ids <- read_csv('data/processed/id_mapping_long.csv',
                     col_types = cols(
                       SVID = col_character(),
                       alternate_id = col_character(),
                       bestzygos = col_character()
                     ))

# Join to the Michigan DB survey table to get completion times
# Select completed surveys where the ID corresponds to a twin
# Select relevant columns
checkin <- left_join(checkin, surveys, by = c('Token' = 'ls_token')) %>% 
  filter(!is.na(date_completed)) %>%
  filter(`First name` %in% twin_ids$alternate_id) %>%
  select(user_id, date_to_present, date_completed, 6:63)


# Rename the columns to be easier to work with
colnames(checkin) <- c('user_id', 'date_to_present', 'date_completed',
                       'any_substance_use', 'cig_use', 'ecig_use', 'other_tob_use', 
                       'alc_use', 'mar_use', 'other_drug_use', 'decline_use', 
                       'cig_freq_days_per_week', 'cig_quantity_per_day_1', 
                       'cig_quantity_per_day_2', 'cig_quantity_yesterday',
                       'ecig_freq_days_per_week', 'ecig_freq_times_per_day',
                       'ecig_quantity_puffs', 'ecig_liquid_concentration_1',
                       'ecig_liquid_concentration_2', 'smokeless_tob_use',
                       'cigar_use', 'pipe_use', 'hookah_use', 'other_tob_decline_use',
                       'other_other_tob_use', 'other_tob_freq_days_per_week', 
                       'alc_freq_days_per_week', 'alc_quantity_drinks_per_day',
                       'alc_quantity_drinks_yesterday', 'mar_freq_days_per_week',
                       'mar_freq_times_per_day', 'mar_freq_times_yesterday',
                       'mar_method', 'mar_method_other', 'psilocybin_use',
                       'lsd_use', 'mescaline_use', 'salvia_use', 'dxm_use',
                       'cocaine_use', 'crack_use', 'stimulant_med_use', 
                       'stimulant_use', 'opioid_use', 'anti_anxiety_use', 
                       'other_drug_decline_use', 'other_other_drug_use', 
                       'other_drug_freq_days_per_week', 'prescribed_use',
                       'prescribed_excess', 'prescribed_excess_pleasure', 
                       'prescribed_excess_energy', 'prescribed_excess_lack_of_effect',
                       'prescribed_excess_decline', 'prescribed_excess_other',
                       'prescribed_excess_which_stimulant', 'prescribed_excess_which_pain_killers',
                       'prescribed_excess_which_depressants', 'prescribed_excess_which_decline', 
                       'prescribed_excess_which_other')

checkin$any_substance_use <- checkin$any_substance_use == 'Yes'
