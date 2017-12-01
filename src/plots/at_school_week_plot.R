# A plot of fraction of points at school by day of week and hour of day

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)
library(viridis)

locs <- read_rds("data/processed/at_school.rds")
locs <- na.omit(locs)

# Up the font size
theme_set(theme_gray(base_size = 16))

# Get local time and then extract day and hour
locs %>%
  mutate(DateTime = DateTime + minutes(sample_timezone)) %>%
  mutate(weekday = wday(DateTime, label = T), hourofday = hour(DateTime)) %>%
  mutate(hourofday = parse_factor(as.character(hourofday), as.character(seq(23, 0)))) %>%
  group_by(weekday, hourofday) %>%
  summarize(at_school_frac = sum(at_school) / n()) %>%
  ggplot(aes(weekday, hourofday)) +
  geom_tile(aes(fill = at_school_frac)) +
  xlab("Day of week") +
  ylab("Hour of day") +
  scale_fill_viridis(name = "Fraction at school")

ggsave("figs/at_school_week.pdf", width = 16, height = 10)
