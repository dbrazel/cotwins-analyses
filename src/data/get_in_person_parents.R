# Extract the in person parental monitoring survey
# Note that whereas the remote survey allows answers for more than two
# parental figures, this survey asks only about the two "primary" figures,
# as selected by the child

# Also, the remote survey only asks whether each parental figure "knows"
# the information in question. The in person survey asks three sets of questions,
# one with "asks", one about whether the twin "tells", and one about whether
# the parent "knows". For the sake of comparability, this script will calculate
# the scores from the "knows" questions

library(readr)
library(dplyr)
library(lubridate)

parents <- read_rds('data/raw/Robin_kid-Q-about-parent_12-1-16.rds')

# Select relevant columns and convert to DateTime,
# and merge the two tester ID columns
parents <-
  parents %>% select(startdate,
                     submitdate,
                     SVID = ID,
                     KPQTESTER,
                     tester,
                     KPQ00003_SQ001:`KPQ00011_4#1`)

parents <-
  parents %>% mutate(
    startdate = ymd_hms(startdate),
    submitdate = ymd_hms(submitdate),
    tester_id = ifelse(
      is.na(parents$KPQTESTER),
      parents$tester,
      parents$KPQTESTER)
  ) %>%
  select(startdate,
         submitdate,
         SVID,
         tester_id,
         KPQ00003_SQ001:`KPQ00011_4#1`)

# Convert the SPSS missing codes to R's NA
spss_missing <- function(x) {
  plyr::mapvalues(
    x,
    c(
      777,
      888,
      999,
      66
    ),
    c(NA, NA, NA, NA)
  )
}

to_do_cols <- c(colnames(parents)[c(5:38)])

parents[to_do_cols] <- lapply(parents[to_do_cols], spss_missing)

# Rename the columns to be more convenient to work with
colnames(parents) <- c(
  "startdate",
  "submitdate",
  "SVID",
  "tester_id",
  "par1",
  "par2",
  "par_other1",
  "par_other2",
  "par1_who_asks",
  "par2_who_asks",
  "par1_money_asks",
  "par2_money_asks",
  "par1_after_school_asks",
  "par2_after_school_asks",
  "par1_free_time_asks",
  "par2_free_time_asks",
  "par1_night_asks",
  "par2_night_asks",
  "par1_who_tells",
  "par2_who_tells",
  "par1_money_tells",
  "par2_money_tells",
  "par1_after_school_tells",
  "par2_after_school_tells",
  "par1_free_time_tells",
  "par2_free_time_tells",
  "par1_night_tells",
  "par2_night_tells",
  "par1_who_knows",
  "par2_who_knows",
  "par1_money_knows",
  "par2_money_knows",
  "par1_after_school_knows",
  "par2_after_school_knows",
  "par1_free_time_knows",
  "par2_free_time_knows",
  "par1_night_knows",
  "par2_night_knows"
)

# Get a monitoring score based on the maximum value for each questions
# across all parental figures for the "knows" questions only
max_per_category <-
  tibble(
    max_who = pmax(
      parents$par1_who_knows,
      parents$par2_who_knows,
      na.rm = T
    ),
    max_money = pmax(
      parents$par1_money_knows,
      parents$par2_money_knows,
      na.rm = T
    ),
    max_after_school = pmax(
      parents$par1_after_school_knows,
      parents$par2_after_school_knows,
      na.rm = T
    ),
    max_free_time = pmax(
      parents$par1_free_time_knows,
      parents$par2_free_time_knows,
      na.rm = T
    ),
    max_night = pmax(
      parents$par1_night_knows,
      parents$par2_night_knows,
      na.rm = T
    )
  )

# We don't use na.rm = T here because if there were no
# non-missing answers for a question, we can't calculate
# a valid score
parents$max_monitor_score <- rowSums(max_per_category)

write_rds(parents, "data/processed/in_person_parents.rds")
write_csv(parents, "data/processed/in_person_parents.csv")
