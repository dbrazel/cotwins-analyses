# Extract symptom counts and diagnoses from the remote PhenX followup
# see src/data/get_phenx_diagnoses.R for details on the measures.

library(readr)
library(dplyr)
library(lubridate)

phenx <- read_csv(
  "data/raw/Michigan_LS_substance_use_4_12_18.csv",
  col_types = paste0(rep('c', 142), collapse = ''))

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
