# Extract the in person parental monitoring survey

library(readr)
library(dplyr)
library(lubridate)

parents <- read_rds('data/raw/Robin_kid-Q-about-parent_12-1-16.rds')

# Select relevant columns and convert to DateTime
parents <- parents %>% select(startdate, submitdate, SVID = ID, KPQTESTER, tester, KPQ00003_SQ001:`KPQ00011_4#1`)
parents <- parents %>% mutate(startdate = ymd_hms(startdate), submitdate = ymd_hms(submitdate))

# TODO: Merge tester IDs, set 777, 888, 999, and 66 to missing across all survey columns, and calculate the scores