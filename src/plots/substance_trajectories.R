# Plot the trajectories of all growth model phenos by age
# Inspired by https://stackoverflow.com/a/48907846

library(readr)
library(ggplot2)
library(dplyr)
library(cowplot)
library(gamm4)
library(lubridate)

twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

# At Home
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

# Aggregate at_home at the week level to match the checking in survey frequency
at_home <- group_by(at_home, user_id = Michigan_ID, week = week(DateTime), year = year(DateTime)) %>%
  summarize(home_frac = sum(at_home) / n(), test_age = mean(test_age), sex = first(sex), family = first(family))

at_home_data <- na.omit(at_home)

at_home_ml <- gamm4(home_frac ~ s(test_age), random = ~(1 | family/user_id), data = at_home_data)
at_home_pred <- predict(at_home_ml$gam, se.fit = T)
at_home_plot_data <-
  tibble(
    test_age = at_home_data$test_age + 17,
    home_frac = at_home_pred$fit,
    home_frac_lower = at_home_pred$fit - 1.96 * at_home_pred$se.fit,
    home_frac_upper = at_home_pred$fit + 1.96 * at_home_pred$se.fit
  )

at_home_plot <- ggplot(at_home_plot_data, aes(test_age, home_frac)) +
  geom_ribbon(aes(ymax = home_frac_upper, ymin = home_frac_lower), alpha = 0.2) +
  geom_line() +
  xlab("Age in years") +
  ylab("Predicted fraction of time at home") +
  xlim(c(14, 20))

save_plot("figs/at_home_trajectory.pdf", at_home_plot)

# At School
at_school <- read_rds("data/processed/at_school.rds")

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

# Aggregate at the week level to match the checking in survey frequency
at_school <- group_by(at_school, user_id, week = week(DateTime), year = year(DateTime)) %>%
  summarize(
    school_frac = sum(at_school) / n(),
    test_age = mean(test_age),
    sex = first(sex),
    family = first(family)
  )

at_school_data <- na.omit(at_school)

at_school_ml <- gamm4(school_frac ~ s(test_age), random = ~(1 | family/user_id), data = at_school_data)
at_school_pred <- predict(at_school_ml$gam, se.fit = T)
at_school_plot_data <-
  tibble(
    test_age = at_school_data$test_age + 17,
    school_frac = at_school_pred$fit,
    school_frac_lower = at_school_pred$fit - 1.96 * at_school_pred$se.fit,
    school_frac_upper = at_school_pred$fit + 1.96 * at_school_pred$se.fit
  )

at_school_plot <- ggplot(at_school_plot_data, aes(test_age, school_frac)) +
  geom_ribbon(aes(ymax = school_frac_upper, ymin = school_frac_lower), alpha = 0.2) +
  geom_line() +
  xlab("Age in years") +
  ylab("Predicted fraction of time at school") +
  xlim(c(14, 20))

save_plot("figs/at_school_trajectory.pdf", at_school_plot)

# Parental Monitoring
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

par_mon_data <- na.omit(par_mon)

par_mon_ml <- gamm4(max_monitor_score ~ s(test_age), random = ~(1 | family/user_id), data = par_mon_data)
par_mon_pred <- predict(par_mon_ml$gam, se.fit = T)
par_mon_plot_data <-
  tibble(
    test_age = par_mon_data$test_age + 17,
    max_monitor_score = par_mon_pred$fit,
    max_monitor_score_lower = par_mon_pred$fit - 1.96 * par_mon_pred$se.fit,
    max_monitor_score_upper = par_mon_pred$fit + 1.96 * par_mon_pred$se.fit
  )

par_mon_plot <- ggplot(par_mon_plot_data, aes(test_age, max_monitor_score)) +
  geom_ribbon(aes(ymax = max_monitor_score_upper, ymin = max_monitor_score_lower), alpha = 0.2) +
  geom_line() +
  xlab("Age in years") +
  ylab("Predicted parental monitoring score") +
  xlim(c(14, 20))

save_plot("figs/par_mon_trajectory.pdf", par_mon_plot)

# Alcohol
dpw_data <- read_rds("data/models/dpw_data.rds")

dpw_ml <- gamm4(drinks_per_week ~ s(test_age), random = ~(1 | family/user_id), data = dpw_data)
dpw_pred <- predict(dpw_ml$gam, se.fit = T)
dpw_plot_data <-
  tibble(
    test_age = dpw_data$test_age + 17,
    dpw = dpw_pred$fit,
    dpw_lower = dpw_pred$fit - 1.96 * dpw_pred$se.fit,
    dpw_upper = dpw_pred$fit + 1.96 * dpw_pred$se.fit
  )

dpw_plot <- ggplot(dpw_plot_data, aes(test_age, dpw)) +
  geom_ribbon(aes(ymax = dpw_upper, ymin = dpw_lower), alpha = 0.2) +
  geom_line() +
  xlab("Age in years") +
  ylab("Predicted log DPW") +
  xlim(c(14, 20))

save_plot("figs/alcohol_trajectory.pdf", dpw_plot)

# Marijuana
mpw_data <- read_rds("data/models/mpw_data.rds")

mpw_ml <- gamm4(mar_per_week ~ s(test_age), random = ~(1 | family/user_id), data = mpw_data)
mpw_pred <- predict(mpw_ml$gam, se.fit = T)
mpw_plot_data <-
  tibble(
    test_age = mpw_data$test_age + 17,
    mpw = mpw_pred$fit,
    mpw_lower = mpw_pred$fit - 1.96 * mpw_pred$se.fit,
    mpw_upper = mpw_pred$fit + 1.96 * mpw_pred$se.fit
  )

mpw_plot <- ggplot(mpw_plot_data, aes(test_age, mpw)) +
  geom_ribbon(aes(ymax = mpw_upper, ymin = mpw_lower), alpha = 0.2) +
  geom_line() +
  xlab("Age in years") +
  ylab("Predicted log MPW") +
  xlim(c(14, 20))

save_plot("figs/marijuana_trajectory.pdf", mpw_plot)

# E-Cigarettes
ecig_data <- read_rds("data/models/ecig_data.rds")

ecig_ml <- gamm4(puffs_per_week ~ s(test_age), random = ~(1 | family/user_id), data = ecig_data)
ecig_pred <- predict(ecig_ml$gam, se.fit = T)
ecig_plot_data <-
  tibble(
    test_age = ecig_data$test_age + 17,
    ppw = ecig_pred$fit,
    ppw_lower = ecig_pred$fit - 1.96 * ecig_pred$se.fit,
    ppw_upper = ecig_pred$fit + 1.96 * ecig_pred$se.fit
  )

ecig_plot <- ggplot(ecig_plot_data, aes(test_age, ppw)) +
  geom_ribbon(aes(ymax = ppw_upper, ymin = ppw_lower), alpha = 0.2) +
  geom_line() +
  xlab("Age in years") +
  ylab("Predicted log PPW") +
  xlim(c(14, 20))

save_plot("figs/ecigarette_trajectory.pdf", ecig_plot)

# Combine the plots with aligned axes
all_plot <-
  plot_grid(
    at_home_plot,
    at_school_plot,
    par_mon_plot,
    dpw_plot,
    mpw_plot,
    ecig_plot,
    align = "hv",
    labels = c("A", "B", "C", "D", "E", "F")
  )

save_plot("figs/all_phenos_trajectories.pdf", all_plot, ncol = 3, nrow = 2)
