# Make plots of the time and distance between consecutive GPS points

library(readr)
library(dplyr)
library(geosphere)
library(cowplot)
library(forcats)

locs <- read_rds("data/processed/Michigan_DB_user_location_11_11_18_cleaned.rds")
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

# Get rid of data not from twins
locs <- semi_join(locs, id_mapping_long, by = c("user_id" = "alternate_id"))

locs <- mutate(locs, app_type = fct_recode(app_type, Android = "android", iOS = "ios"))

time_interval <- tibble()

for (twin in unique(locs$user_id)) {
  twin_time_interval <- 
    locs %>%
    filter(user_id == twin) %>%
    arrange(sample_time) %>%
    transmute(interval = lead(sample_time) - sample_time, app_type = app_type)
  time_interval <- bind_rows(time_interval, twin_time_interval)
}

# Offset by one to avoid taking the log of zero
time_plt <- time_interval %>%
  mutate(interval = as.numeric(interval) + 1) %>%
  ggplot(aes(x = interval, fill = app_type)) +
  geom_histogram() +
  scale_x_log10(
    labels = c("1 min", "1 hour", "1 day", "1 month"),
    breaks = c(60, 60*60, 86400, 2.63e6)
  ) +
  xlab("Time between points") +
  scale_fill_brewer(palette = "Dark2", name = "OS")

save_plot("figs/time_interval.pdf", time_plt, base_aspect_ratio = 1.4)

space_interval <- tibble()

for (twin in unique(locs$user_id)) {
  twin_space_interval <-
    locs %>%
    filter(user_id == twin) %>%
    arrange(sample_time) %>%
    transmute(interval = distHaversine(cbind(lead(.$longitude), lead(.$latitude)), cbind(.$longitude, .$latitude)),
              app_type = app_type)
  space_interval <- bind_rows(space_interval, twin_space_interval)
}

# Offset by one to avoid taking the log of zero
space_plt <- space_interval %>%
  mutate(interval = interval + 1) %>%
  ggplot(aes(x = interval, fill = app_type)) +
  geom_histogram() +
  scale_x_log10(
    labels = c("1 meter", "1 km", "100 km", "10,000 km"),
    breaks = c(1e1, 1e3, 1e5, 1e7)) +
  xlab("Distance between points") +
  scale_fill_brewer(palette = "Dark2", name = "OS")

save_plot("figs/space_interval.pdf", space_plt, base_aspect_ratio = 1.4)

all_plots <-
  plot_grid(
    time_plt,
    space_plt,
    align = "hv",
    nrow = 1,
    ncol = 2,
    labels = c("A", "B")
  )

save_plot("figs/interval_plots.pdf", all_plots, nrow = 1, ncol = 2, base_aspect_ratio = 1.4)
