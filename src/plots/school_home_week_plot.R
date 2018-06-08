# Plot the fraction of points at home or at school by hour of day
# with frequency polygons faceted by day of week
# We want to limit by age and remove school holidays but not restrict to
# weekdays and school hours, so we can see how that shows up in the data

library(readr)
library(dplyr)
library(lubridate)
library(cowplot)

twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

home <- read_rds("data/processed/at_home.rds")
home <- na.omit(home)

school <- read_rds("data/processed/at_school.rds")
school <- na.omit(school)

home <- mutate(home, DateTime = DateTime + minutes(sample_timezone))
home <- left_join(home, twin_info, by = c("SVID" = "ID1")) %>%
  select(DateTime:SVID, family, Birth_Date) %>%
  mutate(test_age = as.numeric(as_date(DateTime) - Birth_Date) / 365) %>%
  filter(test_age < 18)

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
school <- mutate(school, DateTime = DateTime + minutes(sample_timezone)) %>%
  # No school in June or July
  filter(month(DateTime) != 6, month(DateTime) != 7) %>%
  # a %within% b where a is a date and b is a list of intervals, will check
  # if a falls within any of the intervals in b
  filter(!(date(DateTime) %within% vacays))

school <- left_join(school, id_mapping_long, by = c("Michigan_ID" = "alternate_id")) %>%
  left_join(twin_info, by = c("SVID" = "ID1")) %>%
  select(DateTime, user_id = Michigan_ID, orig_datetime, at_school, family, Birth_Date) %>%
  mutate(test_age = as.numeric(as_date(DateTime) - Birth_Date) / 365)

home <-
  group_by(home,
           d = wday(DateTime, label = T, abbr = F),
           h = hour(DateTime)) %>%
  summarize(frac = sum(at_home) / n())
home$pheno <- "Home"

school <-
  group_by(school,
           d = wday(DateTime, label = T, abbr = F),
           h = hour(DateTime)) %>%
  summarize(frac = sum(at_school) / n())
school$pheno <- "School"

plot_data <- bind_rows(home, school)

plt <- ggplot(plot_data, aes(h, frac, color = pheno)) +
  geom_line() +
  facet_wrap( ~ d, ncol = 1) +
  scale_color_brewer("", palette = "Dark2") +
  labs(y = "Fraction of time", x = "") +
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 23, 5),
                     labels = c("12 AM", "5 AM", "10 AM", "3 PM", "8 PM"))

save_plot(
  "figs/school_home_week_plot.pdf",
  plt,
  base_height = 8,
  base_aspect_ratio = 0.6
)
