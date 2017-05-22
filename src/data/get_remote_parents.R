# Extract the in person parental monitoring survey

library(readr)
library(dplyr)

parents <- read_csv(
  'data/raw/Michigan_LS_parents_1_22_17.csv',
  na = c('NA', 'N/A', ''),
  col_types = paste0(rep('c', 40), collapse = '')
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
parents <-
  left_join(parents, surveys, by = c('Token' = 'ls_token')) %>%
  filter(!is.na(date_completed)) %>%
  filter(`First name` %in% twin_ids$alternate_id) %>%
  select(user_id, date_to_present, date_completed, 6:38, ls_token = Token)
