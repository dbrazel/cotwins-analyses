# Fit a multi-level quadratic growth model to time at home at night using lme4
# so that we can compare it to the quadratic model

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(lme4)

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

# Aggregate at_home at the week level to match the checking in survey frequency
at_home <- group_by(at_home, user_id = Michigan_ID, week = week(DateTime), year = year(DateTime)) %>%
  summarize(home_frac = sum(at_home) / n(), test_age = mean(test_age), sex = first(sex), family = first(family))

at_home <- na.omit(at_home)

# Fit a quadratic growth model with age at assessment as the time metric
# and age squared and sex as fixed effects
ml <-
  lmer(
    home_frac ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id),
    data = at_home
  )
ml_test <-
  lmerTest::lmer(
    home_frac ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id),
    data = at_home
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
at_home_pred <- left_join(at_home, parameters, by = "user_id") %>%
  mutate(
    home_frac_pred =
      intercept +
      slope * test_age +
      quadratic * test_age^2 +
      sex_beta * sex,
    home_frac_resid = home_frac - home_frac_pred
  ) %>%
  select(user_id, family, test_age, home_frac, sex, home_frac_pred, home_frac_resid)

write_rds(ml, "data/models/at_home_quadratic_model.rds")
write_rds(ml_test, "data/models/at_home_quadratic_model_lmerTest.rds")
write_rds(at_home_pred, "data/models/at_home_quadratic_predictions.rds")
write_rds(parameters, "data/models/at_home_quadratic_parameters.rds")
write_rds(at_home, "data/models/at_home_data.rds")
