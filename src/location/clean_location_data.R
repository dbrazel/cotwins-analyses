library(readr)
library(dplyr)
library(lubridate)

locations <- read_rds('data/raw/Michigan_DB_user_location_05_31_17.rds')
locations <- locations %>%
  select(creation_date:sample_timezone) %>%
  na.omit() %>%
  unique() %>%
  mutate(creation_date = ymd_hms(creation_date), sample_time = ymd_hms(sample_time))
locations <- locations %>% filter(accuracy < 500, sample_time > ymd('2015-01-01'), sample_time < ymd('2017-06-01'))
write_rds(locations, 'data/processed/Michigan_DB_user_location_05_31_17_cleaned.rds')
