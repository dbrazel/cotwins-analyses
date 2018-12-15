# Clean the location data
# Get rid of duplicate points and points that are missing a key field
# Convert the time stamps to datetime objects
# Require accuracy to be better than 500 meters and sample_time to be
# within the possible range
# Add an app_type column and select for points with timestamps in UTC

library(readr)
library(dplyr)
library(lubridate)

locations <- read_rds('data/raw/Michigan_DB_user_location_11_11_18.rds')
user_info <- read_rds('data/raw/Michigan_DB_users_11_11_18.rds')
user_info <- select(user_info, alternate_id, app_type)

locations <- locations[!duplicated(locations[c('user_id', 'latitude', 'longitude', 'sample_time')]), ]

locations <- locations %>%
  select(id:timestamp_type) %>%
  filter(
    !is.na(longitude),
    !is.na(latitude),
    !is.na(creation_date),
    !is.na(sample_time),
    !is.na(user_id),
    !is.na(accuracy)) %>%
  mutate(creation_date = ymd_hms(creation_date), sample_time = ymd_hms(sample_time))

locations <- locations %>% filter(accuracy < 500, sample_time > ymd('2015-01-02'), sample_time < ymd('2018-11-11'))

locations <- left_join(locations, user_info, by = c('user_id' = 'alternate_id'))
locations <- filter(locations, app_type == 'android' | timestamp_type == 'iOS_UTC')

write_rds(locations, 'data/processed/Michigan_DB_user_location_11_11_18_cleaned.rds')
