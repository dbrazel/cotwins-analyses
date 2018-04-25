# Fit a multi-level quadratic growth model to marijuana uses per week (MPW) using lme4 and
# write out the estimated model, its parameters, and the predictions and residuals

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(lme4)

sub_use <- read_rds("data/processed/remote_substance_use.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

# For each survey response get the twin's age at that time, centered at 17, and
# switch sex to a 0/1 coding from 1/2
sub_use <- left_join(sub_use, id_mapping_long, by = c("user_id" = "alternate_id"))
sub_use <- left_join(sub_use, twin_info, by = c("SVID" = "ID1"))
sub_use <- select(
  sub_use,
  user_id,
  date_completed,
  any_substance_use,
  mar_use,
  mar_freq_times_per_day,
  mar_freq_days_per_week,
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

# If the twin didn't use marijuana that week, set the phenos to 0
sub_use$mar_freq_times_per_day[!sub_use$any_substance_use] <- 0
sub_use$mar_freq_times_per_day[!sub_use$mar_use] <- 0
sub_use$mar_freq_days_per_week[!sub_use$any_substance_use] <- 0
sub_use$mar_freq_days_per_week[!sub_use$mar_use] <- 0

# Calculate log-transformed marijuana uses per week
sub_use <- mutate(
  sub_use,
  mar_per_week = mar_freq_times_per_day * mar_freq_days_per_week,
  mar_per_week = log(mar_per_week + 1)
)

sub_use <- select(sub_use, user_id, family:mar_per_week) %>% na.omit()

# Fit a quadratic growth model with age at assessment as the time metric
# and age squared and sex as fixed effects
ml <-
  lmer(
    mar_per_week ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id),
    data = sub_use
  )
ml_test <-
  lmerTest::lmer(
    mar_per_week ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id),
    data = sub_use
  )

# Get the random and fixed effects
rand_effs <- ranef(ml)
fix_effs <- fixef(ml)

# Get the random effects estimates for each twin and combine them with the
# fixed effects estimates to get the estimated parameters
rand_effs_family <- rand_effs$family %>% tibble::rownames_to_column("family")
rand_effs_twin <- rand_effs$`user_id:family` %>%
  tibble::rownames_to_column("both") %>%
  separate(both, c("user_id", "family"), ":")

parameters <- left_join(rand_effs_twin, rand_effs_family, by = "family") %>%
  transmute(
    user_id = user_id,
    intercept = `(Intercept).x` + `(Intercept).y` + fix_effs["(Intercept)"],
    slope = `test_age.x` + `test_age.y` + fix_effs["test_age"],
    quadratic = `I(test_age^2).x` + `I(test_age^2).y` + fix_effs["I(test_age^2)"],
    sex_beta = fix_effs["sex"]
  )

# Get the predicted values and the residuals
sub_use_pred <- left_join(sub_use, parameters, by = "user_id") %>%
  mutate(
    mar_per_week_pred =
      intercept +
      slope * test_age +
      quadratic * test_age^2 +
      sex_beta * sex,
    mar_per_week_resid = mar_per_week - mar_per_week_pred
  ) %>%
  select(user_id, family:mar_per_week, mar_per_week_pred, mar_per_week_resid)

write_rds(ml, "data/models/mpw_quadratic_model.rds")
write_rds(ml_test, "data/models/mpw_quadratic_model_lmerTest.rds")
write_rds(sub_use_pred, "data/models/mpw_quadratic_predictions.rds")
write_rds(parameters, "data/models/mpw_quadratic_parameters.rds")
