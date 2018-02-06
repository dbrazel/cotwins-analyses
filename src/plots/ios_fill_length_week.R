# Plot the length of the iOS forward fills by the day of the week

library(readr)
library(dplyr)
library(ggplot2)
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
# and then back to datetime
fill_sizes <- rle(as.character(std_locs_ios$orig_datetime))
fill_sizes <- tibble(values = fill_sizes$values, lengths = fill_sizes$lengths)
fill_sizes$values <- fill_sizes$values %>%
  ymd_hms() %>%
  wday(label = T) %>%
  as.factor()

ggplot(fill_sizes, aes(values, lengths)) +
  geom_boxplot() +
  xlab("Day of week") +
  ylab("Fill length")

ggsave("figs/ios_fill_length_week.pdf", width = 6, height = 4)
