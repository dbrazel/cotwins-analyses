# Plot the number of checkins completed per week against weeks in study

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Up the font size
theme_set(theme_gray(base_size = 16))

id_mapping_long <- read_csv(
  "data/processed/id_mapping_long.csv",
  col_types = "ccc"
  )
remote_sub_use <- read_rds("data/processed/remote_substance_use.rds")
paper_data <- read_rds(
  "data/processed/Robin_paper-entry_2-22-17_cleaned.rds"
  )


checkin_rate <-
  remote_sub_use %>%
  group_by(user_id) %>%
  summarize(n = n()) %>%
  left_join(id_mapping_long, by = c("user_id" = "alternate_id")) %>%
  left_join(paper_data, by = c("SVID" = "ID1")) %>%
  mutate(time_in_study = as.duration(ymd("2017-05-31") - Test_Date)) %>%
  mutate(
    time_in_study = if_else(
      time_in_study <= dyears(1), time_in_study, dyears(1)
      )
    ) %>%
  mutate(
    weeks_in_study = as.numeric(time_in_study) / 604800,
    checkin_per_week = n / (as.numeric(time_in_study) / 604800)
  ) %>%
  select(
    user_id,
    n,
    Test_Date,
    time_in_study,
    checkin_per_week,
    weeks_in_study)

ggplot(checkin_rate, aes(x = weeks_in_study, y = checkin_per_week)) +
  geom_point(alpha = 0.5) +
  ylab("Checkins per week") +
  xlab("Weeks in study")

ggsave("figs/checkins_per_week.pdf", width = 6, height = 4)
