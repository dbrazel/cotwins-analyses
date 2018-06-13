# Fit a multi-level linear growth model to time at school using lme4 and
# write out the estimated model, its parameters, and the predictions and residuals

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(lmerTest)

at_school <- read_rds("data/processed/at_school.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

# We started getting more locations in November 2016. Let's restrict to there and
# later to make the manual part of this tractable
at_school <- filter(at_school, date(DateTime) >= ymd("2016-11-01"))

# Make a list of lubridate intervals representing days off from school
# Drawn from the Denver public schools calendar and figs/school_frac_calendar.pdf
vacays <- list(
  interval(ymd("2016-11-21"), ymd("2016-11-25")),
  interval(ymd("2016-12-22"), ymd("2017-01-06")),
  interval(ymd("2017-01-16"), ymd("2017-01-16")),
  interval(ymd("2017-02-20"), ymd("2017-02-20")),
  interval(ymd("2017-03-27"), ymd("2017-03-31")),
  interval(ymd("2017-05-29"), ymd("2017-05-31")),
  interval(ymd("2017-08-01"), ymd("2017-08-18")),
  interval(ymd("2017-09-04"), ymd("2017-09-04")),
  interval(ymd("2017-11-20"), ymd("2017-11-24")),
  interval(ymd("2017-12-22"), ymd("2018-01-05")),
  interval(ymd("2018-01-15"), ymd("2018-01-15")),
  interval(ymd("2018-02-19"), ymd("2018-02-19")),
  interval(ymd("2018-03-26"), ymd("2018-03-30"))
)

# Shift the locations to local time and restrict to school hours and days
at_school <- mutate(at_school, DateTime = DateTime + minutes(sample_timezone)) %>%
  filter(hour(DateTime) %in% c(8, 9, 10, 11, 12, 13, 14)) %>%
  # No school in June or July
  filter(month(DateTime) != 6, month(DateTime) != 7) %>%
  # No school on Saturday and Sunday
  filter(wday(DateTime) != 7, wday(DateTime) != 1) %>%
  # a %within% b where a is a date and b is a list of intervals, will check
  # if a falls within any of the intervals in b
  filter(!(date(DateTime) %within% vacays))

# Get ages for at_school
at_school <- left_join(at_school, id_mapping_long, by = c("Michigan_ID" = "alternate_id")) %>%
  left_join(twin_info, by = c("SVID" = "ID1")) %>%
  select(DateTime, user_id = Michigan_ID, orig_datetime, at_school, family, sex = Sex1, Birth_Date) %>%
  mutate(sex = sex - 1,
         test_age = as.numeric(as_date(DateTime) - Birth_Date) / 365,
         test_age = test_age - 17)

# Restrict at_school to 18 or younger
at_school <- filter(at_school, test_age <= 1)

# Aggregate at the week level to match the checking in survey frequency
at_school <- group_by(at_school, user_id, week = week(DateTime), year = year(DateTime)) %>%
  summarize(
    school_frac = sum(at_school) / n(),
    test_age = mean(test_age),
    sex = first(sex),
    family = first(family)
  )

# Fit a linear growth model with age at assessment as the time metric
# and age squared and sex as fixed effects
ml <-
  lmer(
    school_frac ~ (test_age + I(test_age^2) + sex) + (test_age | family/user_id),
    data = at_school
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

at_school_pred <- left_join(at_school, parameters, by = "user_id") %>%
  mutate(school_frac_pred =
           intercept +
           slope * test_age +
           quadratic * test_age^2 +
           sex_beta * sex,
         school_frac_resid = school_frac - school_frac_pred
  ) %>%
  select(user_id, family, test_age, school_frac, sex, school_frac_pred, school_frac_resid)

write_rds(ml, "data/models/at_school_linear_model.rds")
write_rds(at_school_pred, "data/models/at_school_linear_predictions.rds")
write_rds(parameters, "data/models/at_school_linear_parameters.rds")
