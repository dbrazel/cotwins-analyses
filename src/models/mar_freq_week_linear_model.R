# Fit a linear growth model to marijuana days per week using nlme and
# write out the random slopes and intercepts

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(nlme)
library(broom)

sub_use <- read_rds("data/processed/remote_substance_use.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds")
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

# For each survey response get the twin's age at that time and switch sex to
# a 0/1 coding from 1/2
sub_use <- left_join(sub_use, id_mapping_long, by = c("user_id" = "alternate_id"))
sub_use <- left_join(sub_use, twin_info, by = c("SVID" = "ID1"))
sub_use <- select(
  sub_use,
  user_id,
  date_completed,
  any_substance_use,
  mar_use,
  mar_freq_days_per_week,
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

# If the twin didn't use marijuana that week, set the pheno to 0
sub_use$mar_freq_days_per_week[!sub_use$any_substance_use] <- 0
sub_use$mar_freq_days_per_week[!sub_use$mar_use] <- 0

# Fit a linear growth model with age at assessment as the time metric
# We adjust the intercept to correspond to age 14 and
# group by twin within family
ml <-
  nlme(
    mar_freq_days_per_week ~
      (beta_01 + beta_11 * sex + d_1i) +
      (beta_02 + beta_12 * sex + d_2i) * (test_age - 14),
    data = sub_use,
    fixed = beta_01 + beta_11 + beta_02 + beta_12 ~ 1,
    random = d_1i + d_2i ~ 1|family/user_id,
    na.action = "na.omit",
    start = c(0, 0, 0, 0)
  )

# Get the random effect estimates
re_est <- tidy(ml) %>% filter(term %in% c("d_1i", "d_2i"))

# Extract the twin ID
re_est <- separate(re_est, level, into = c("family", "twin"), sep = "/")

write_rds(re_est, "data/models/mar_freq_week_linear_random_effects.rds")