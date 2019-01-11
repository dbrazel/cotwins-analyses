# Visualize the number of GPS points over time,
# using a calendar heatmap

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggTimeSeries)
library(viridis)

locs <- read_rds(
  "data/processed/Michigan_DB_user_location_11_11_18_cleaned.rds"
  )

# Up the font size
theme_set(theme_gray(base_size = 16))

locs %>%
  group_by(date = as_date(sample_time)) %>%
  summarize(N = n()) %>%
  ggplot_calendar_heatmap('date', 'N') +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_viridis() +
  facet_wrap(~Year, ncol = 1)

ggsave("figs/n_locs_calendar.pdf", width = 10, height = 8)
