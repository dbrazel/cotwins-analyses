# Fit a multi-level quadratic growth model to parental monitoring using lme4

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(lme4)

twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
par_mon <- read_rds("data/processed/remote_parents.rds") %>%
  select(user_id, date_completed, n_par_figs, max_monitor_score)

# Get ages
par_mon <- left_join(par_mon, id_mapping_long, by = c("user_id" = "alternate_id")) %>%
  select(-bestzygos) %>%
  left_join(twin_info, by = c("SVID" = "ID1")) %>%
  select(user_id:max_monitor_score, family, sex = Sex1, Birth_Date) %>%
  mutate(sex = sex - 1,
         test_age = as.numeric(as_date(date_completed) - Birth_Date) / 365,
         test_age = test_age - 17) %>%
  select(user_id, max_monitor_score, family, sex, test_age)

# Missing data doesn't play nice with the lme4 bootstrapping function
par_mon <- na.omit(par_mon)

# Restrict to 18 or younger
par_mon <- filter(par_mon, test_age <= 1)

# Fit a quadratic growth model with age at assessment as the time metric
# and age squared and sex as fixed effects
ml <-
  lmer(
    max_monitor_score ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id),
    data = par_mon
  )
ml_test <-
  lmerTest::lmer(
    max_monitor_score ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id),
    data = par_mon
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
par_mon_pred <- left_join(par_mon, parameters, by = "user_id") %>%
  mutate(
    max_monitor_score_pred =
      intercept +
      slope * test_age +
      quadratic * test_age^2 +
      sex_beta * sex,
    max_monitor_score_resid = max_monitor_score - max_monitor_score_pred
  ) %>%
  select(user_id, family, test_age, max_monitor_score, sex, max_monitor_score_pred, max_monitor_score_resid)

write_rds(ml, "data/models/par_mon_quadratic_model.rds")
write_rds(ml_test, "data/models/par_mon_quadratic_model_lmerTest.rds")
write_rds(par_mon_pred, "data/models/par_mon_quadratic_predictions.rds")
write_rds(parameters, "data/models/par_mon_quadratic_parameters.rds")
write_rds(par_mon, "data/models/par_mon_data.rds")
