# Get the app-based substance use questions from the checking in survey

library(readr)

checkin <- read_csv('data/raw/Michigan_LS_checking_in_1_22_17.csv')
surveys <- read_rds('data/raw/Michigan_DB_surveyentries_02_01_17.rds')
twin_ids <- read_csv('data/processed/id_mapping_long.csv',
                     col_types = cols(
                       SVID = col_character(),
                       alternate_id = col_character(),
                       bestzygos = col_character()
                     ))

# Join to the Michigan DB survey table to get completion times
# Select completed surveys whre the ID corresponds to a twin
checkin <- left_join(checkin, surveys, by = c('Token' = 'ls_token')) %>% 
  filter(!is.na(date_completed)) %>%
  filter(`First name` %in% twin_ids$alternate_id)
