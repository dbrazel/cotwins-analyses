# Plot the length of the iOS forward fills by the day of the week and hour of day

library(readr)
library(dplyr)
library(cowplot)
library(lubridate)

std_locs <- read_rds("data/processed/std_locations_filled.rds")

std_locs_ios <- filter(std_locs, app_type == "ios")
std_locs_ios <- mutate(
  std_locs_ios,
  orig_datetime = orig_datetime + minutes(sample_timezone)
)

# Ensure that the data are sorted
std_locs_ios <- arrange(std_locs_ios, Michigan_ID, DateTime)

# Get the datetime and length of each forward fill in the ios std locs
# rle only works with atomic types so we have to convert to character
# and then back to datetime. Divide by two to convert from a length in
# units of 30 minutes to one in hours
fill_sizes_week <- rle(as.character(std_locs_ios$orig_datetime))
fill_sizes_week <- tibble(values = fill_sizes_week$values, lengths = fill_sizes_week$lengths / 2)
fill_sizes_week$values <- fill_sizes_week$values %>%
  ymd_hms() %>%
  wday(label = T) %>%
  as.factor()

fill_sizes_hour <- rle(as.character(std_locs_ios$orig_datetime))
fill_sizes_hour <- tibble(values = fill_sizes_hour$values, lengths = fill_sizes_hour$lengths / 2)
fill_sizes_hour$values <- fill_sizes_hour$values %>%
  ymd_hms() %>%
  hour() %>%
  as.factor()

plt_week <- ggplot(fill_sizes_week, aes(values, lengths)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Day of week") +
  ylab("Fill length (hours)")

plt_hour <- ggplot(fill_sizes_hour, aes(values, lengths)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Hour") +
  ylab("Fill length (hours)")

plts <- plot_grid(plt_hour, plt_week, labels = c("A", "B"), ncol = 2)

save_plot(
  "figs/ios_fill_length.pdf", 
  plts,
  ncol = 2,
  nrow = 1,
  base_aspect_ratio = 1.5
  )
