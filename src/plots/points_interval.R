# Make plots of the time and distance between consecutive GPS points

library(readr)
library(dplyr)
library(geosphere)
library(ggplot2)

locs <- read_rds("data/processed/Michigan_DB_user_location_09_25_17_cleaned.rds")
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

# Get rid of data not from twins
locs <- semi_join(locs, id_mapping_long, by = c("user_id" = "alternate_id"))

# Get the device type
user_info <- read_rds("data/raw/Michigan_DB_users_09_25_17.rds")
locs <- left_join(locs, user_info, by = c("user_id" = "alternate_id"))

time_interval <- tibble()

for (twin in unique(locs$user_id)) {
  twin_time_interval <- 
    locs %>%
    filter(user_id == twin) %>%
    arrange(sample_time) %>%
    transmute(interval = lead(sample_time) - sample_time, app_type = app_type)
  time_interval <- bind_rows(time_interval, twin_time_interval)
}

# Offset by one to avoid taking the log of zero
time_interval %>%
  mutate(interval = as.numeric(interval) + 1) %>%
  ggplot(aes(x = interval, fill = app_type)) +
  geom_histogram() +
  scale_x_log10() +
  xlab("Time between points (seconds)")

ggsave("figs/time_interval.pdf", width = 6, height = 4)

space_interval <- tibble()

for (twin in unique(locs$user_id)) {
  twin_space_interval <-
    locs %>%
    filter(user_id == twin) %>%
    arrange(sample_time) %>%
    transmute(interval = distHaversine(cbind(lead(.$longitude), lead(.$latitude)), cbind(.$longitude, .$latitude)),
              app_type = app_type)
  space_interval <- bind_rows(space_interval, twin_space_interval)
}

# Offset by one to avoid taking the log of zero
space_interval %>%
  mutate(interval = interval + 1) %>%
  ggplot(aes(x = interval, fill = app_type)) +
  geom_histogram() +
  scale_x_log10() +
  xlab("Distance between points (meters)")

ggsave("figs/space_interval.pdf", width = 6, height = 4)
