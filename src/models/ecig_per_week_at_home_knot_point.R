library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(lmerTest)

sub_use <- read_rds("data/processed/remote_substance_use.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

knot_points <- read_csv("data/raw/at_home_knot_points.csv", col_types = "ccnnn")
knot_points <- select(knot_points, user_id, KP)
knot_points <- na.omit(knot_points)

sub_use <- left_join(sub_use, id_mapping_long, by = c("user_id" = "alternate_id"))
sub_use <- left_join(sub_use, twin_info, by = c("SVID" = "ID1"))
sub_use <- select(
  sub_use,
  user_id,
  date_completed,
  any_substance_use,
  ecig_use,
  ecig_freq_days_per_week,
  ecig_freq_times_per_day,
  Sex1,
  Birth_Date,
  Test_Date,
  family
)
sub_use <- mutate(
  sub_use,
  test_age = as.numeric(as_date(date_completed) - Birth_Date) / 365,
  sex = Sex1 - 1
)

# If the twin didn't use alcohol that week, set the phenos to 0
sub_use$ecig_freq_days_per_week[!sub_use$any_substance_use] <- 0
sub_use$ecig_freq_days_per_week[!sub_use$ecig_use] <- 0
sub_use$ecig_freq_times_per_day[!sub_use$any_substance_use] <- 0
sub_use$ecig_freq_times_per_day[!sub_use$ecig_use] <- 0

# Calculate log-transformed ecig uses per week
sub_use <- mutate(
  sub_use,
  ecig_per_week =
    ecig_freq_days_per_week * ecig_freq_times_per_day,
  ecig_per_week = log(ecig_per_week + 1)
)

sub_use <- select(sub_use, user_id, family:ecig_per_week) %>% na.omit()

sub_use <- filter(sub_use, user_id %in% unique(knot_points$user_id))
sub_use <- group_by(sub_use, user_id) %>% mutate(time1 = test_age - min(test_age)) %>% ungroup()
sub_use_sum <- group_by(sub_use, user_id) %>% summarize(min_age = min(test_age))
sub_use_sum <- left_join(sub_use_sum, knot_points)
sub_use <- left_join(sub_use, sub_use_sum)
sub_use <- mutate(sub_use, time2 = time1 - (KP - min_age))
sub_use[sub_use$time2 < 0, "time2"] <- 0

ecig_ml_bilinear <- lmer(ecig_per_week ~ (time1 + time2) + (time1 + time2 | user_id), data = sub_use)
ecig_ml_monolinear <- lmer(ecig_per_week ~ time1 + (time1 | user_id), data = sub_use)

sub_use$KP <- 18.5
sub_use <- mutate(sub_use, time2 = time1 - (KP - min_age))
sub_use[sub_use$time2 < 0, "time2"] <- 0
ecig_ml_fixed_knot <- lmer(ecig_per_week ~ (time1 + time2) + (time1 + time2 | user_id), data = sub_use)

sub_use$pred <- predict(ecig_ml_bilinear)
