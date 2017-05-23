# Extract the in person parental monitoring survey

library(readr)
library(dplyr)
library(lubridate)

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
# Select relevant columns and convert date columns to DateTimes
parents <-
  left_join(parents, surveys, by = c('Token' = 'ls_token')) %>%
  filter(!is.na(date_completed)) %>%
  filter(`First name` %in% twin_ids$alternate_id) %>%
  select(user_id, date_to_present, date_completed, 6:38, ls_token = Token) %>%
  mutate(
    date_to_present = ymd_hms(date_to_present),
    date_completed = ymd_hms(date_completed)
  )

# Rename the columns to be easier to work with
colnames(parents) <-
  c(
    'user_id',
    'date_to_present',
    'date_completed',
    'live_with_father',
    'live_with_stepfather',
    'live_with_mother',
    'live_with_stepmother',
    'live_with_other',
    'live_with_decline',
    'understood_instructions',
    'mother_who',
    'mother_money',
    'mother_after_school',
    'mother_night',
    'mother_free_time',
    'father_who',
    'father_money',
    'father_after_school',
    'father_night',
    'father_free_time',
    'stepfather_who',
    'stepfather_money',
    'stepfather_after_school',
    'stepfather_night',
    'stepfather_free_time',
    'stepmother_who',
    'stepmother_money',
    'stepmother_after_school',
    'stepmother_night',
    'stepmother_free_time',
    'other_adult',
    'other_who',
    'other_money',
    'other_after_school',
    'other_night',
    'other_free_time',
    'ls_token'
  )

# Convert "Yes"/"No" columns to boolean
to_do_cols <-
  c(colnames(parents)[c(4:9)])
for (i in to_do_cols) {
  parents[, i] <- parents[, i] == 'Yes'
}

parents$understood_instructions = parents$understood_instructions == 'Got it!'

# Convert from the multiple choice answers to integer scores
score_convert <- function(x) {
  as.numeric(plyr::mapvalues(
    x,
    c(
      "Once in a while",
      "Never",
      "Sometimes",
      "Don't know",
      "Always",
      "Often",
      "Prefer not to answer"
    ),
    c(1, 0, 2, NA, 4, 3, NA),
    warn_missing = T
  ))
}

to_do_cols <- 
  c(
    'mother_who',
    'mother_money',
    'mother_after_school',
    'mother_night',
    'mother_free_time',
    'father_who',
    'father_money',
    'father_after_school',
    'father_night',
    'father_free_time',
    'stepfather_who',
    'stepfather_money',
    'stepfather_after_school',
    'stepfather_night',
    'stepfather_free_time',
    'stepmother_who',
    'stepmother_money',
    'stepmother_after_school',
    'stepmother_night',
    'stepmother_free_time',
    'other_who',
    'other_money',
    'other_after_school',
    'other_night',
    'other_free_time'
  )

parents[to_do_cols] <- lapply(parents[to_do_cols], score_convert)

# Calculate the total number of parental figures and a 
# additive score, normalized to number of parental figures
parents$n_par_figs <-
  select(parents, live_with_father:live_with_other) %>% rowSums(na.rm = T)
parents$n_par_figs[parents$n_par_figs == 0] <- NA

parents$monitor_score <-
  select(parents,
         mother_who:stepmother_free_time,
         other_who:other_free_time) %>% 
  rowSums(na.rm = T) / parents$n_par_figs

# Get a monitoring score based on the maximum value for each questions
# across all parental figures.
max_per_category <-
  tibble(
    max_who = pmax(
      parents$mother_who,
      parents$father_who,
      parents$stepfather_who,
      parents$stepmother_who,
      parents$other_who,
      na.rm = T
    ),
    max_money = pmax(
      parents$mother_money,
      parents$father_money,
      parents$stepfather_money,
      parents$stepmother_money,
      parents$other_money,
      na.rm = T
    ),
    max_after_school = pmax(
      parents$mother_after_school,
      parents$father_after_school,
      parents$stepfather_after_school,
      parents$stepmother_after_school,
      parents$other_after_school,
      na.rm = T
    ),
    max_night = pmax(
      parents$mother_night,
      parents$father_night,
      parents$stepfather_night,
      parents$stepmother_night,
      parents$other_night,
      na.rm = T
    ),
    max_free_time = pmax(
      parents$mother_free_time,
      parents$father_free_time,
      parents$stepfather_free_time,
      parents$stepmother_free_time,
      parents$other_free_time,
      na.rm = T
    )
  )

# We don't use na.rm = T here because if there were no
# non-missing answers for a question, we can't calculate
# a valid score
parents$max_monitor_score <- rowSums(max_per_category)

write_rds(parents, 'data/processed/remote_parents.rds')
write_csv(parents, 'data/processed/remote_parents.csv')
