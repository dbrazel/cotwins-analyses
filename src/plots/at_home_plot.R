# Make a plot of fraction of GPS points at home, over time, with a facet for each twin

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

at_home <- read_rds("data/processed/at_home.rds")

at_home_sum <-
  at_home %>%
  group_by(user_id, date = as_date(sample_time)) %>%
  summarize(at_home_frac = sum(at_home, na.rm = T) / n(), N = n())

ggplot(at_home_sum, aes(date, at_home_frac)) + geom_point(alpha = 0.5) + facet_wrap(~user_id)

ggsave("figs/at_home_facet.pdf", width = 40, height = 40, units = "in")
