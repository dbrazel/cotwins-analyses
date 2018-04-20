# Calculate correlations between the growth parameters for DPW and
# time at home at night and get bootstrapped confidence intervals

library(lme4)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

source("src/models/bootmer_funcs.R")

sub_use <- read_rds("data/processed/remote_substance_use.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
at_home <- read_rds("data/processed/at_home.rds")

# Shift the locations to local time and restrict to 12 AM to 5 AM
at_home <- mutate(at_home, DateTime = DateTime + minutes(sample_timezone)) %>%
  filter(hour(DateTime) %in% c(0, 1, 2, 3, 4))

# Get ages for at_home
at_home <- left_join(at_home, twin_info, by = c("SVID" = "ID1")) %>%
  select(DateTime:SVID, family, sex = Sex1, Birth_Date) %>%
  mutate(sex = sex - 1,
         test_age = as.numeric(as_date(DateTime) - Birth_Date) / 365,
         test_age = test_age - 17)

# Restrict at_home to 18 or younger
at_home <- filter(at_home, test_age <= 1)

# Aggregate at_home at the week level
at_home <- group_by(at_home, user_id = Michigan_ID, week = week(DateTime), year = year(DateTime)) %>%
  summarize(home_frac = sum(at_home) / n(), test_age = mean(test_age), sex = first(sex), family = first(family))

sub_use <- left_join(sub_use, id_mapping_long, by = c("user_id" = "alternate_id"))
sub_use <- left_join(sub_use, twin_info, by = c("SVID" = "ID1"))
sub_use <- select(
  sub_use,
  user_id,
  date_completed,
  any_substance_use,
  alc_use,
  alc_quantity_drinks_per_day,
  alc_freq_days_per_week,
  Sex1,
  Birth_Date,
  Test_Date,
  family
)
sub_use <- mutate(
  sub_use,
  test_age = as.numeric(as_date(date_completed) - Birth_Date) / 365,
  test_age = test_age - 17,
  sex = Sex1 - 1
)

# If the twin didn't use alcohol that week, set the phenos to 0
sub_use$alc_quantity_drinks_per_day[!sub_use$any_substance_use] <- 0
sub_use$alc_quantity_drinks_per_day[!sub_use$alc_use] <- 0
sub_use$alc_freq_days_per_week[!sub_use$any_substance_use] <- 0
sub_use$alc_freq_days_per_week[!sub_use$alc_use] <- 0

# Calculate log-transformed DPW
sub_use <- mutate(
  sub_use,
  drinks_per_week = alc_quantity_drinks_per_day * alc_freq_days_per_week,
  drinks_per_week = log(drinks_per_week + 1)
)

# Remove missing rows
at_home <- select(at_home, user_id, family, sex, test_age, home_frac) %>% na.omit()
sub_use <- select(sub_use, user_id, family, test_age, sex, drinks_per_week) %>% na.omit()

# Subjects must be in both sub_use and at_home and sorted by user
at_home <- filter(at_home, user_id %in% sub_use$user_id) %>% arrange(user_id)
sub_use <- filter(sub_use, user_id %in% at_home$user_id) %>% arrange(user_id)

# Fit quadratic growth models
ml_home <-
  lmer(
    home_frac ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id),
    data = at_home
  )

ml_dpw <-
  lmer(
    drinks_per_week ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id),
    data = sub_use
  )

boot_obj_home <- boot_quadratic(ml_home)
write_rds(boot_obj_home, "data/models/home_boot.rds")
boot_obj_dpw <- boot_quadratic(ml_dpw)
write_rds(boot_obj_dpw, "data/models/dpw_boot.rds")

home_dpw_cors <- get_quadratic_corrs(boot_obj_home, boot_obj_dpw)
