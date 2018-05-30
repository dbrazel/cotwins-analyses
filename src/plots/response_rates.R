# Plot the rate of data acquisition over time

library(readr)
library(cowplot)
library(dplyr)
library(lubridate)

surveys <- read_rds("data/raw/Michigan_DB_surveyentries_04_12_18.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
locs <- read_rds("data/processed/Michigan_DB_user_location_04_12_18_cleaned.rds")

# Get surveys completed by twins with the time since enrollment at completion
# in years
surveys <- filter(
  surveys,
  user_id %in% id_mapping_long$alternate_id,
  completed == 1,
  !is.na(date_completed)
  ) %>%
  select(user_id, date_completed) %>%
  left_join(id_mapping_long, by = c("user_id" = "alternate_id")) %>%
  left_join(twin_info, by = c("SVID" = "ID1")) %>%
  select(user_id, date_completed, Test_Date) %>%
  mutate(t = as_date(ymd_hms(date_completed)) - Test_Date, t = as.numeric(t) / 365) %>%
  select(user_id, t) %>%
  na.omit()

# Get GPS points from twins with the time since enrollment at measurement in years
locs <- filter(locs, user_id %in% id_mapping_long$alternate_id) %>%
  select(user_id, sample_time, app_type) %>%
  left_join(id_mapping_long, by = c("user_id" = "alternate_id")) %>%
  left_join(twin_info, by = c("SVID" = "ID1")) %>%
  select(user_id, sample_time, Test_Date, app_type) %>%
  mutate(t = as_date(sample_time) - Test_Date, t = as.numeric(t) / 365) %>%
  select(user_id, t, app_type) %>%
  na.omit()

locs[locs$app_type == "android", "app_type"] <- "Android"
locs[locs$app_type == "ios", "app_type"] <- "iOS"

# Bin by week
surveys$bin <- cut(surveys$t, seq(0, 2, 1/52), include.lowest = T)
locs$bin <- cut(locs$t, seq(0, 2, 1/52), include.lowest = T)

surveys <- group_by(surveys, bin) %>% summarise(n = n(), t = mean(t)) %>% ungroup() %>% filter(!is.na(bin))
locs <- group_by(locs, app_type, bin) %>% summarise(n = n(), t = mean(t)) %>% ungroup() %>% filter(!is.na(bin))

# 670 twins for the first year, 531 for the second, normalize to measures per twin
surveys <- mutate(surveys, n = if_else(t > 1, n / 531, n / 670))
locs <- mutate(locs, n = if_else(t > 1, n / 531, n / 670))

# Plot the per capita number of surveys completed each week, by years since
# enrollment in the study
surveys_plot <- ggplot(surveys, aes(t, n)) +
  geom_line() +
  xlim(0, 2) +
  ylim(0, 2.5) +
  labs(x = "Years since enrollment", y = "Surveys per twin per week") +
  geom_hline(yintercept = 2.05, color = "green")

save_plot("figs/survey_rate.pdf", surveys_plot, base_aspect_ratio = 1.2)

# Plot the per capita number of locations recorded each week, by years
# since enrollment in the study
location_plot <- ggplot(locs, aes(t, n, color = app_type)) +
  geom_line() +
  xlim(0, 2) +
  ylim(0, 150) +
  labs(x = "Years since enrollment", y = "Locations per twin per week", color = "OS")

save_plot("figs/location_rate.pdf", location_plot, base_aspect_ratio = 1.2)

all_plot <- plot_grid(
  surveys_plot,
  location_plot,
  align = "h",
  labels = c("A", "B"),
  nrow = 1,
  ncol = 2
)

save_plot(
  "figs/rate_plots.pdf",
  all_plot,
  nrow = 1,
  ncol = 2,
  base_aspect_ratio = 1.2
  )
