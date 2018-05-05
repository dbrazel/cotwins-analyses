# Extract symptom counts and diagnoses from the remote PhenX followup
# see src/data/get_phenx_diagnoses.R for details on the measures.

library(readr)
library(dplyr)
library(lubridate)

phenx <- read_csv(
  'data/raw/Michigan_LS_substance_use_4_12_18.csv',
  col_types = paste0(rep('c', 142), collapse = ''),
  na = c(
    'NA',
    'N/A',
    'Would Rather Not Answer',
    '',
    'I don\'t know',
    'Would rather not answer'
  )
)

surveys <-
  read_rds('data/raw/Michigan_DB_surveyentries_04_12_18.rds')
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
phenx <-
  left_join(phenx, surveys, by = c('token' = 'ls_token')) %>%
  filter(!is.na(date_completed)) %>%
  filter(firstname %in% twin_ids$alternate_id) %>%
  select(user_id, date_to_present, date_completed, 6:140, ls_token = token) %>%
  mutate(
    date_to_present = ymd_hms(date_to_present),
    date_completed = ymd_hms(date_completed)
  )

# Convert 'Yes'/'No' columns to boolean
to_do_cols <-
  c(colnames(phenx)[c(4:6, 9, 12, 15, 18:23, 25, 29:69, 71:113, 115:138)])
for (i in to_do_cols) {
  phenx[, i] <- phenx[, i] == 'Yes'
}

results <- select(phenx, user_id:date_completed, ls_token)

# Usage info from the introduction section

# Convert to drinking days per week on average in past six months
results['alc_freq_half_year'] <-
  plyr::mapvalues(
    phenx$AlcFreq,
    c(
      'I had no alcohol at all in the past 6 months',
      'Less than one day per month',
      'About one day per month',
      '1-3 days per month',
      '1-2 days per week',
      '3-5 days per week',
      'daily or almost daily'
    ),
    c(0, 0.125, 0.25, 0.5, 1.5, 4, 7),
    warn_missing = T
  ) %>% as.numeric()

# Drinks per days on drinking days
results['alc_quant_half_year'] <-
  plyr::mapvalues(
    phenx$AlcQuant,
    c(
      'Less than 1 on each day that I drank',
      '1-3 on each day that I drank',
      '4-6 on each day that I drank',
      '7-10 on each day that I drank',
      '11-12 on each day that I drank',
      'More than 12 on each day that I drank'
    ),
    c(0.5, 2, 5, 8.5, 11.5, 12),
    warn_missing = T
  ) %>% as.numeric()

# Average drinks per week in past six months
results['alc_dpw_half_year'] <- results$alc_freq_half_year * results$alc_quant_half_year
results[results$alc_freq_half_year == 0, 'alc_dpw_half_year'] <- 0

# Any cigarette use in past six months?
results['cig_ever_half_year'] <- phenx$CigEver

# Convert to smoking days per week in past six months
results['cig_freq_half_year'] <- as.numeric(phenx$CigFreq) / 4

# Cigarettes smoked on smoking days in past six months
results['cig_quant_half_year'] <-
  plyr::mapvalues(
    phenx$CigQuant,
    c(
      'Less than one cigarette per day',
      '1 cigarette per day',
      '2 cigarettes per day',
      '3 to 5 cigarettes per day',
      '6 to 15 cigarettes per day',
      '16 to 25 cigarettes per day',
      'More than 25 cigarettes per day'
    ),
    c(0.5, 1, 2, 4, 10.5, 20.5, 25),
    warn_missing = T
  ) %>% as.numeric()

# Any ecig use in the past six months?
results['ecig_ever_half_year'] <- phenx$EcigEver

# Convert to vaping days per week in past month
results['ecig_freq_half_year'] <- as.numeric(phenx$EcigFreq) / 4

# Ecig uses on vaping days in past six months
results['ecig_quant_half_year'] <-
  plyr::mapvalues(
    phenx$EcigQuant,
    c(
      'Less than one time per day',
      '1 time per day',
      '2 times per day',
      '3 to 5 times per day',
      '6 to 10 times per day',
      '11 to 20 times per day',
      'More than 20 times per day'
    ),
    c(0.5, 1, 2, 4, 8, 15.5, 20),
    warn_missing = T
  ) %>% as.numeric()

# Average ecig uses per week in past six months
results['ecig_ppw_half_year'] <- results$ecig_freq_half_year * results$ecig_quant_half_year
results[!results$ecig_ever_half_year, 'ecig_ppw_half_year'] <- 0

# Any marijuana use in past six months?
results['mar_ever_half_year'] <- phenx$MarEver

# Convert to pot smoking days per week in past six months
results['mar_freq_half_year'] <-
  plyr::mapvalues(
    phenx$MarFreq,
    c(
      "I didn't use marijuana at all in the past 6 months",
      "Less than one day per month",
      "About one day per month",
      "1-3 days per month",
      "1-2 days per week",
      "3-5 days per week",
      "daily or almost daily"
    ),
    c(0, 0.125, 0.25, 0.5, 1.5, 4, 7),
    warn_missing = T
  ) %>% as.numeric()

# Pot uses on pot smoking days in past six months
results['mar_quant_half_year'] <-
  plyr::mapvalues(
    phenx$MarQuant,
    c(
      "I don't usually use enough marijuana to get high",
      "About once per day",
      "About twice per day",
      "More than twice per day"
    ),
    c(0.1, 1, 2, 3),
    warn_missing = T
  ) %>% as.numeric()

# Average marijuana uses per week in past six months
results['mar_mpw_half_year'] <- results$mar_freq_half_year * results$mar_quant_half_year
results[!results$mar_ever_half_year, 'mar_mpw_half_year'] <- 0

# Alcohol abuse symptoms
