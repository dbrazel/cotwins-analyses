# Visualize the fraction of points at school since the start of 2017,
# using a calendar heatmap

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggTimeSeries)
library(viridis)

locs <- read_rds("data/processed/at_school.rds")

# Up the font size
theme_set(theme_gray(base_size = 16))

locs %>%
  group_by(date = as_date(DateTime)) %>%
  summarize(school_frac = sum(at_school) / n()) %>%
  filter(date >= ymd("2017-01-01")) %>%
  ggplot_calendar_heatmap('date', 'school_frac') +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_viridis(name = "Fraction at school")

ggsave("figs/school_frac_calendar.pdf", width = 10.1, height = 6.8)
