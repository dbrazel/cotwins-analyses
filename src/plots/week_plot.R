# A plot of fraction of points at home and at school by day of week and hour of day

library(readr)
library(cowplot)
library(dplyr)
library(lubridate)
library(viridis)

twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

school <- read_rds("data/processed/at_school.rds")
school <- na.omit(school)

vacays <- list(
  interval(ymd("2016-11-21"), ymd("2016-11-25")),
  interval(ymd("2016-12-22"), ymd("2017-01-06")),
  interval(ymd("2017-01-16"), ymd("2017-01-16")),
  interval(ymd("2017-02-20"), ymd("2017-02-20")),
  interval(ymd("2017-03-27"), ymd("2017-03-31")),
  interval(ymd("2017-05-29"), ymd("2017-05-31")),
  interval(ymd("2017-06-05"), ymd("2017-07-31")),
  interval(ymd("2017-08-01"), ymd("2017-08-18")),
  interval(ymd("2017-09-04"), ymd("2017-09-04")),
  interval(ymd("2017-11-20"), ymd("2017-11-24")),
  interval(ymd("2017-12-22"), ymd("2018-01-05")),
  interval(ymd("2018-01-15"), ymd("2018-01-15")),
  interval(ymd("2018-02-19"), ymd("2018-02-19")),
  interval(ymd("2018-03-26"), ymd("2018-03-30")),
  interval(ymd("2018-05-24"), ymd("2018-08-17")),
  interval(ymd("2018-09-03"), ymd("2018-09-03"))
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
  mutate(test_age = as.numeric(as_date(DateTime) - Birth_Date) / 365) %>%
  filter(test_age < 18)

school <-
  mutate(
    school,
    d = wday(DateTime, label = T),
    h = hour(DateTime),
    h = parse_factor(as.character(h), as.character(seq(23, 0)))) %>%
  group_by(d, h) %>%
  summarize(frac = sum(at_school) / n())

home <- read_rds("data/processed/at_home.rds")
home <- na.omit(home)

home <- mutate(home, DateTime = DateTime + minutes(sample_timezone))
home <- left_join(home, twin_info, by = c("SVID" = "ID1")) %>%
  select(DateTime:SVID, family, Birth_Date) %>%
  mutate(test_age = as.numeric(as_date(DateTime) - Birth_Date) / 365) %>%
  filter(test_age < 18)

home <-
  mutate(
    home,
    d = wday(DateTime, label = T),
    h = hour(DateTime),
    h = parse_factor(as.character(h), as.character(seq(23, 0)))) %>%
  group_by(d, h) %>%
  summarize(frac = sum(at_home) / n())

school_plt <- ggplot(school, aes(d, h)) +
  geom_tile(aes(fill = frac)) +
  xlab("") +
  ylab("Hour of day") +
  scale_fill_viridis(name = "Fraction of\ntime at school")

home_plt <- ggplot(home, aes(d, h)) +
  geom_tile(aes(fill = frac)) +
  xlab("") +
  ylab("Hour of day") +
  scale_fill_viridis(name = "Fraction of\ntime at home")

plts <- plot_grid(home_plt, school_plt, labels = c("A", "B"), ncol = 2)

save_plot(
  "figs/week.pdf", 
  plts,
  ncol = 2,
  nrow = 1,
  base_aspect_ratio = 1.5
)
