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
  mutate(t = as_date(ymd_hms(date_completed)) - Test_Date, t = as.numeric(t) / 365) %>%
  select(user_id, t) %>%
  na.omit()

# Get GPS points from twins with the time since enrollment at measurement in years
locs <- filter(locs, user_id %in% id_mapping_long$alternate_id) %>%
  select(user_id, sample_time, app_type) %>%
  left_join(id_mapping_long, by = c("user_id" = "alternate_id")) %>%
  left_join(twin_info, by = c("SVID" = "ID1")) %>%
  select(user_id, sample_time, Test_Date, app_type) %>%
  mutate(t = as_date(sample_time) - Test_Date, t = as.numeric(t) / 365) %>%
  select(user_id, t, app_type) %>%
  na.omit()

locs[locs$app_type == "android", "app_type"] <- "Android"
locs[locs$app_type == "iOS", "app_type"] <- "iOS"

# Plot the per capita number of surveys completed each week, by years since
# enrollment in the study
surveys_plot <- ggplot(surveys, aes(t)) +
  geom_freqpoly(aes(y = ..count.. / 670), binwidth = 1/52) +
  xlim(0, 2) +
  labs(x = "Years since enrollment", y = "Surveys per twin per week") +
  geom_hline(yintercept = 2.05)

save_plot("figs/survey_rate.pdf", surveys_plot, base_aspect_ratio = 2)

# Plot the per capita number of locations recorded each week, by years
# since enrollment in the study
location_plot <- ggplot(locs, aes(t)) +
  geom_freqpoly(aes(y = ..count.. / 670), binwidth = 1/52) +
  xlim(0, 2) +
  labs(x = "Years since enrollment", y = "Locations per twin per week")

save_plot("figs/location_rate.pdf", location_plot, base_aspect_ratio = 2)
