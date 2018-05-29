# Plot the rate of data acquisition over time

library(readr)
library(cowplot)
library(dplyr)
library(lubridate)

surveys <- read_rds("data/raw/Michigan_DB_surveyentries_04_12_18.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
locs <- read_rds("data/processed/Michigan_DB_user_location_04_12_18_cleaned.rds")

# Get surveys completed by twins with the time since enrollment at completion
# in years
surveys <- filter(
  surveys,
  user_id %in% id_mapping_long$alternate_id,
  completed == 1,
  !is.na(date_completed)
  ) %>%
  select(user_id, date_completed) %>%
  left_join(id_mapping_long, by = c("user_id" = "alternate_id")) %>%
  left_join(twin_info, by = c("SVID" = "ID1")) %>%
  select(user_id, date_completed, Test_Date) %>%
  mutate(t = as_date(ymd_hms(date_completed)) - Test_Date, t = t / 365)


