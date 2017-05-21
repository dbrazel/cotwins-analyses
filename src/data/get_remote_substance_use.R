# Get the app-based substance use questions from the checking in survey

library(readr)
library(dplyr)

checkin <- read_csv(
  'data/raw/Michigan_LS_checking_in_1_22_17.csv',
  na = c('NA', 'N/A', 'Would rather not answer', '', 'I don\'t know')
)
surveys <-
  read_rds('data/raw/Michigan_DB_surveyentries_02_01_17.rds')
twin_ids <- read_csv(
  'data/processed/id_mapping_long.csv',
  col_types = cols(
    SVID = col_character(),
    alternate_id = col_character(),
    bestzygos = col_character()
  )
)

# Join to the Michigan DB survey table to get completion times
# Select completed surveys where the ID corresponds to a twin
# Select relevant columns
checkin <-
  left_join(checkin, surveys, by = c('Token' = 'ls_token')) %>%
  filter(!is.na(date_completed)) %>%
  filter(`First name` %in% twin_ids$alternate_id) %>%
  select(user_id, date_to_present, date_completed, 6:63)


# Rename the columns to be easier to work with
colnames(checkin) <-
  c(
    'user_id',
    'date_to_present',
    'date_completed',
    'any_substance_use',
    'cig_use',
    'ecig_use',
    'other_tob_use',
    'alc_use',
    'mar_use',
    'other_drug_use',
    'decline_use',
    'cig_freq_days_per_week',
    'cig_quantity_per_day',
    'cig_quantity_per_day_2',
    'cig_quantity_yesterday',
    'ecig_freq_days_per_week',
    'ecig_freq_times_per_day',
    'ecig_quantity_puffs',
    'ecig_liquid_concentration_known',
    'ecig_liquid_concentration',
    'smokeless_tob_use',
    'cigar_use',
    'pipe_use',
    'hookah_use',
    'other_tob_decline_use',
    'other_other_tob_used',
    'other_tob_freq_days_per_week',
    'alc_freq_days_per_week',
    'alc_quantity_drinks_per_day',
    'alc_quantity_drinks_yesterday',
    'mar_freq_days_per_week',
    'mar_freq_times_per_day',
    'mar_freq_times_yesterday',
    'mar_method',
    'mar_method_other',
    'psilocybin_use',
    'lsd_use',
    'mescaline_use',
    'salvia_use',
    'dxm_use',
    'cocaine_use',
    'crack_use',
    'stimulant_med_use',
    'stimulant_use',
    'opioid_use',
    'anti_anxiety_use',
    'other_drug_decline_use',
    'other_other_drug_used',
    'other_drug_freq_days_per_week',
    'prescribed_use',
    'prescribed_excess',
    'prescribed_excess_pleasure',
    'prescribed_excess_energy',
    'prescribed_excess_lack_of_effect',
    'prescribed_excess_decline',
    'prescribed_excess_other',
    'prescribed_excess_which_stimulant',
    'prescribed_excess_which_pain_killers',
    'prescribed_excess_which_depressants',
    'prescribed_excess_which_decline',
    'prescribed_excess_which_other'
  )

# Convert "Yes"/"No" columns to boolean
to_do_cols <-
  c(colnames(checkin)[c(4:11, 19, 21:25, 36:47, 50:55, 57:60)])
for (i in to_do_cols) {
  checkin[, i] <- checkin[, i] == 'Yes'
}

# Merge the doubled cigarette quantity question
checkin$cig_quantity_per_day[is.na(checkin$cig_quantity_per_day)] <-
  checkin$cig_quantity_per_day_2[is.na(checkin$cig_quantity_per_day)]
checkin <- select(checkin,-cig_quantity_per_day_2)

# Remap binned answers to numbers, using the middle of a range when necessary
# "More than" answers are mapped to their lower bound.
# This is necessary because the in person surveys were not binned for
# equivalent questions.
checkin$cig_freq_days_per_week <-
  plyr::mapvalues(
    checkin$cig_freq_days_per_week,
    c(
      'Seven days',
      'Six days',
      'Four days',
      'Five days',
      'Two days',
      'Three days',
      'One day'
    ),
    c(7, 6, 4, 5, 2, 3, 1),
    warn_missing = T
  ) %>% as.numeric()

checkin$cig_quantity_per_day <-
  plyr::mapvalues(
    checkin$cig_quantity_per_day,
    c(
      "2 cigarettes",
      "5 cigarettes per day",
      "5 cigarettes",
      "3 cigarettes",
      "3 cigarettes per day",
      "2 cigarettes per day",
      "4 cigarettes per day",
      "6-10 cigarettes",
      "1 cigarette per day",
      "Only part of a cigarette per day",
      "1 cigarette",
      "6-10 cigarettes per day",
      "More than 30 cigarettes",
      "11-15 cigarettes",
      "4 cigarettes"
    ),
    c(2, 5, 5, 3, 3, 2, 4, 8, 1, 0.5, 1, 8, 30, 13, 4),
    warn_missing = T
  ) %>%
  as.numeric()

checkin$cig_quantity_yesterday <-
  plyr::mapvalues(
    checkin$cig_quantity_yesterday,
    c(
      "5 cigarettes",
      "1 cigarette",
      "2 cigarettes",
      "3 cigarettes",
      "0 cigarettes (I did not smoke at all yesterday)",
      "6-10 cigarettes",
      "Only part of a cigarette",
      "4 cigarettes",
      "11-15 cigarettes"
    ),
    c(5, 1, 2, 3, 0, 8, 0.5, 4, 13),
    warn_missing = T
  ) %>% as.numeric()

checkin$ecig_freq_days_per_week <-
  plyr::mapvalues(
    checkin$ecig_freq_days_per_week,
    c(
      'Seven days',
      'Six days',
      'Four days',
      'Five days',
      'Two days',
      'Three days',
      'One day'
    ),
    c(7, 6, 4, 5, 2, 3, 1),
    warn_missing = T
  ) %>% as.numeric()

checkin$ecig_freq_times_per_day <-
  plyr::mapvalues(
    checkin$ecig_freq_times_per_day,
    c(
      "Once per day",
      "2-3 times per day",
      "4-5 times per day",
      "10 or more times per day",
      "6-9 times per day"
    ),
    c(1, 2.5, 4.5, 10, 7.5),
    warn_missing = T
  ) %>% as.numeric()

checkin$ecig_quantity_puffs <-
  plyr::mapvalues(
    checkin$ecig_quantity_puffs,
    c("2-3 puffs", "One puff", "4-5 puffs", "10 or more puffs", "6-9 puffs"),
    c(2.5, 1, 4.5, 10, 7.5),
    warn_missing = T
  ) %>% as.numeric()

checkin$ecig_liquid_concentration <-
  plyr::mapvalues(
    checkin$ecig_liquid_concentration,
    c(
      "nicotine free (0)",
      "1-10 mg/ml",
      "31 or higher",
      "11-20 mg/ml",
      "21-30 mg/ml"
    ),
    c(0, 5.5, 31, 15.5, 25.5),
    warn_missing = T
  ) %>% as.numeric()
