# Visualize the fraction of points at school since the start of 2017,
# using a calendar heatmap

library(readr)
library(dplyr)
library(lubridate)
library(cowplot)
library(ggTimeSeries)
library(viridis)

locs <- read_rds("data/processed/at_school.rds")

twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

# Shift to local time
locs <- mutate(locs, DateTime = DateTime + minutes(sample_timezone)) %>%
  filter(hour(DateTime) %in% c(8, 9, 10, 11, 12, 13, 14))

plt <- locs %>%
  group_by(date = as_date(DateTime)) %>%
  summarize(school_frac = sum(at_school) / n()) %>%
  filter(date >= ymd("2017-01-01")) %>%
  ggplot_calendar_heatmap('date', 'school_frac') +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_viridis(name = "Fraction at school") +
  facet_wrap(~Year, ncol = 1)

save_plot("figs/school_frac_calendar.pdf", plt, base_aspect_ratio = 2.5, base_height = 4)
