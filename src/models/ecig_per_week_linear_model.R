# Fit a multi-level linear growth model to ecig uses per week using lme4 and
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

# For each survey response get the twin's age at that time, centered at 17
# and switch sex to a 0/1 coding from 1/2
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
  ecig_quantity_puffs,
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
sub_use$ecig_freq_days_per_week[!sub_use$any_substance_use] <- 0
sub_use$ecig_freq_days_per_week[!sub_use$ecig_use] <- 0
sub_use$ecig_freq_times_per_day[!sub_use$any_substance_use] <- 0
sub_use$ecig_freq_times_per_day[!sub_use$ecig_use] <- 0
sub_use$ecig_quantity_puffs[!sub_use$any_substance_use] <- 0
sub_use$ecig_quantity_puffs[!sub_use$ecig_use] <- 0

# Calculate log-transformed puffs per week
sub_use <- mutate(
  sub_use,
  puffs_per_week =
    ecig_freq_days_per_week * ecig_freq_times_per_day * ecig_quantity_puffs,
  puffs_per_week = log(puffs_per_week + 1)
)

ml <-
  lmer(
    puffs_per_week ~ (test_age + I(test_age^2) + sex) + (test_age | family/user_id),
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
    quadratic = fix_effs["I(test_age^2)"],
    sex_beta = fix_effs["sex"]
  )

# Get the predicted values and the residuals
sub_use_pred <- left_join(sub_use, parameters, by = "user_id") %>%
  mutate(
    puffs_per_week_pred =
      intercept +
      slope * test_age +
      quadratic * test_age^2 +
      sex_beta * sex,
    puffs_per_week_resid = puffs_per_week - puffs_per_week_pred
  ) %>%
  select(user_id, family:puffs_per_week, puffs_per_week_pred, puffs_per_week_resid)

write_rds(ml, "data/models/ecig_linear_model.rds")
write_rds(sub_use_pred, "data/models/ecig_linear_predictions.rds")
write_rds(parameters, "data/models/ecig_linear_parameters.rds")
