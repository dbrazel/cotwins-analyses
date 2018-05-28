# Plot the trajectories of all growth model phenos by age
# Inspired by https://stackoverflow.com/a/48907846

# k values chosen using gam.check and ?choose.k

library(readr)
library(dplyr)
library(gamm4)
library(lubridate)
library(cowplot)
library(ggExtra)

twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

# At Home and At School
at_home <- read_rds("data/processed/at_home_unfiltered.rds")

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

at_home_ml <- gamm4(home_frac ~ s(test_age, k = 13), random = ~(1 | family/user_id), data = at_home_data)
at_home_pred <- predict(at_home_ml$gam, se.fit = T)
at_home_plot_data <-
  tibble(
    test_age = at_home_data$test_age + 17,
    frac = at_home_pred$fit,
    lower = at_home_pred$fit - 1.96 * at_home_pred$se.fit,
    upper = at_home_pred$fit + 1.96 * at_home_pred$se.fit
  )

at_home_plot_data$pheno <- "Home"

at_school <- read_rds("data/processed/at_school_unfiltered.rds")

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

at_school_ml <- gamm4(school_frac ~ s(test_age, k = 18), random = ~(1 | family/user_id), data = at_school_data)
at_school_pred <- predict(at_school_ml$gam, se.fit = T)
at_school_plot_data <-
  tibble(
    test_age = at_school_data$test_age + 17,
    frac = at_school_pred$fit,
    lower = at_school_pred$fit - 1.96 * at_school_pred$se.fit,
    upper = at_school_pred$fit + 1.96 * at_school_pred$se.fit
  )

at_school_plot_data$pheno <- "School"

home_school_plot_data <- bind_rows(at_home_plot_data, at_school_plot_data)
home_school_plot_data$pheno <- factor(home_school_plot_data$pheno, levels = c("Home", "School"))

home_school_plot <- ggplot(home_school_plot_data, aes(test_age, frac, fill = pheno, color = pheno)) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2) +
  geom_line() +
  scale_color_brewer("", palette = "Dark2") +
  scale_fill_brewer("", palette = "Dark2") +
  labs(
    x = "Age (years)",
    y = "Predicted fraction of time"
  ) +
  theme(legend.direction = "horizontal", legend.position = c(0.55, 0.05)) +
  xlim(14, 20)

pdf("figs/home_school_trajectory.pdf", width = 6, height = 6)
ggMarginal(
  home_school_plot,
  margins = "x",
  groupColour = T,
  groupFill = T,
  type = "histogram")
dev.off()

# Twin Distance
tp_dists <- read_rds("data/processed/twin_distances.rds")

tp_dists <-
  left_join(tp_dists, id_mapping_long, by = c("tp_id" = "alternate_id"))
tp_dists <- select(tp_dists,-bestzygos)
tp_dists <- left_join(tp_dists, twin_info, by = c("SVID" = "ID1"))
tp_dists <-
  select(tp_dists, tp_id, zygosity, DateTime, distance, Birth_Date)
tp_dists <-
  mutate(tp_dists, age = as.numeric(as_date(DateTime) - Birth_Date) / 365)
tp_dists$zygosity = factor(tp_dists$zygosity, levels = c("MZ", "DZ", "OS"))
tp_dists <-
  group_by(tp_dists,
           tp_id,
           d = day(DateTime),
           m = month(DateTime),
           y = year(DateTime)) %>%
  summarise(
    zygosity = first(zygosity),
    distance = mean(distance),
    age = mean(age)
  )

# Winsorize based on the distribution of distance and log transform
tp_dists[tp_dists$distance > 2e5, "distance"] <- 2e5
tp_dists <- mutate(tp_dists, distance = log10(distance + 1))

dist_ml <- gamm4(
  distance ~ zygosity + s(age, by = zygosity),
  random = ~(1 | tp_id), data = tp_dists
)

dist_pred <- predict(dist_ml$gam, se.fit = T)

dist_plot_data <-
  tibble(
    age = tp_dists$age,
    zygosity = tp_dists$zygosity,
    distance = dist_pred$fit,
    dist_lower = dist_pred$fit - 1.96 * dist_pred$se.fit,
    dist_upper = dist_pred$fit + 1.96 * dist_pred$se.fit
  )

dist_plot <- ggplot(dist_plot_data, aes(age, distance, fill = zygosity, color = zygosity)) +
  geom_ribbon(aes(ymax = dist_upper, ymin = dist_lower), alpha = 0.2) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "Age (years)",
    y = "Predicted log10 of distance (meters)",
    color = "Zygosity",
    fill = "Zygosity"
  ) +
  theme(legend.direction = "horizontal", legend.position = c(0.35, 0.05)) +
  scale_y_continuous(
    breaks = c(3, 4, 5, 6),
    labels = c(
      "1 km",
      "10 km",
      "100 km",
      "1,000 km")
  ) +
  xlim(14, 20)

pdf("figs/twin_distance_trajectory.pdf", width = 6, height = 6)
ggMarginal(
  dist_plot,
  margins = "x",
  groupColour = T,
  groupFill = T,
  type = "histogram")
dev.off()

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

par_mon_ml <- gamm4(max_monitor_score ~ s(test_age, k = 6), random = ~(1 | family/user_id), data = par_mon_data)
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
  xlab("Age (years)") +
  ylab("Predicted parental monitoring score") +
  xlim(14, 20)

pdf("figs/par_mon_trajectory.pdf", width = 6, height = 6)
ggMarginal(
  par_mon_plot,
  margins = "x",
  type = "histogram")
dev.off()

# Substance Use
dpw_data <- read_rds("data/models/dpw_data.rds")

dpw_ml <- gamm4(drinks_per_week ~ s(test_age, k = 4), random = ~(1 | family/user_id), data = dpw_data)
dpw_pred <- predict(dpw_ml$gam, se.fit = T)
dpw_plot_data <-
  tibble(
    test_age = dpw_data$test_age + 17,
    use = dpw_pred$fit,
    lower = dpw_pred$fit - 1.96 * dpw_pred$se.fit,
    upper = dpw_pred$fit + 1.96 * dpw_pred$se.fit
  )

dpw_plot_data$pheno <- "Alcohol"

mpw_data <- read_rds("data/models/mpw_data.rds")

mpw_ml <- gamm4(mar_per_week ~ s(test_age, k = 5), random = ~(1 | family/user_id), data = mpw_data)
mpw_pred <- predict(mpw_ml$gam, se.fit = T)
mpw_plot_data <-
  tibble(
    test_age = mpw_data$test_age + 17,
    use = mpw_pred$fit,
    lower = mpw_pred$fit - 1.96 * mpw_pred$se.fit,
    upper = mpw_pred$fit + 1.96 * mpw_pred$se.fit
  )

mpw_plot_data$pheno <- "Marijuana"

ecig_data <- read_rds("data/models/ecig_data.rds")

ecig_ml <- gamm4(puffs_per_week ~ s(test_age, k = 7), random = ~(1 | family/user_id), data = ecig_data)
ecig_pred <- predict(ecig_ml$gam, se.fit = T)
ecig_plot_data <-
  tibble(
    test_age = ecig_data$test_age + 17,
    use = ecig_pred$fit,
    lower = ecig_pred$fit - 1.96 * ecig_pred$se.fit,
    upper = ecig_pred$fit + 1.96 * ecig_pred$se.fit
  )

ecig_plot_data$pheno <- "E-Cigarettes"

sub_use_plot_data <- bind_rows(dpw_plot_data, mpw_plot_data) %>% bind_rows(ecig_plot_data)
sub_use_plot_data$pheno <- factor(sub_use_plot_data$pheno, levels = c("Alcohol", "Marijuana", "E-Cigarettes"))

sub_use_plot <- ggplot(sub_use_plot_data, aes(test_age, use, fill = pheno, color = pheno)) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.2) +
  geom_line() +
  scale_color_brewer("", palette = "Dark2") +
  scale_fill_brewer("", palette = "Dark2") +
  labs(
    x = "Age (years)",
    y = "Predicted ln of substance use"
  ) +
  theme(legend.direction = "horizontal", legend.position = c(0.2, 0.05)) +
  xlim(14, 20)

pdf("figs/sub_use_trajectory.pdf", width = 6, height = 6)
ggMarginal(
  sub_use_plot,
  margins = "x",
  type = "histogram")
dev.off()

# Combine the plots with aligned axes
all_plot <-
  plot_grid(
    dist_plot,
    home_school_plot,
    par_mon_plot,
    sub_use_plot,
    align = "hv",
    labels = c("A", "B", "C", "D"),
    nrow = 2,
    ncol = 2
  )

save_plot(
  "figs/all_phenos_trajectories.pdf",
  all_plot,
  base_aspect_ratio = 1.3,
  base_height = 8
  )
