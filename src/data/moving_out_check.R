# In the life events survey, administered every 30±10 days, we ask if the twin has
# "moved out to live on your own" in the past two months (this time period seems
# to be an error). We can check the consistency of this with the moving out
# variable inferred from the time at home data.

library(readr)
library(dplyr)
library(lubridate)

twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

knot_points <- read_csv("data/raw/at_home_knot_points.csv", col_types = "ccnnn")
knot_points <- select(knot_points, user_id, KP)
knot_points <- na.omit(knot_points)

surveys <- read_rds("data/raw/Michigan_DB_surveyentries_11_11_18.rds")
life_events <-
  read_csv("data/raw/Michigan_LS_life_events_12_15_18.csv",
           col_types = paste(rep("c", 44), collapse = ""),
           na = "N/A") %>%
  select(5, 24, 33, 43)

names(life_events) <- c("Token", "move1", "move2", "user_id")

life_events[is.na(life_events$move1), "move1"] <- life_events[is.na(life_events$move1), "move2"]
life_events <- select(life_events, Token, move = move1, user_id)

# Join to the Michigan DB survey table to get completion times
life_events <- left_join(life_events, surveys, by = c("Token" = "ls_token")) %>%
  select(user_id = user_id.x, move, date_completed)

# Select responses where the twin had moved out
life_events <- filter(life_events, move == "Yes")

# Get ages
life_events <-
  left_join(life_events, id_mapping_long, by = c("user_id" = "alternate_id")) %>%
  left_join(twin_info, by = c("SVID" = "ID1")) %>%
  select(user_id:date_completed, Birth_Date) %>%
  mutate(
    date_completed = as_date(date_completed) - month(1),
    age_completed = as.numeric(date_completed - Birth_Date) / 365
  ) %>%
  select(user_id, age_completed) %>%
  na.omit()

both <-
  inner_join(life_events, knot_points) %>%
  mutate(age_diff = abs(age_completed - KP)) %>%
  group_by(user_id) %>%
  summarize(min_diff = min(age_diff))

median(both$min_diff)
