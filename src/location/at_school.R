# Annotate each standardized location points with whether it is within
# 100 meters of the top school for that twin

library(readr)
library(dplyr)
library(geosphere)

std_locs <- read_rds("data/processed/std_locations.rds")
schools <- read_rds("data/processed/twin_top_school.rds")

# Use only non-missing points
std_locs <- na.omit(std_locs)

std_locs["at_school"] <- NA

# Get the top school for each twin
std_locs <- left_join(
  std_locs,
  schools,
  by = c("Colorado_ID" = "twin_id")
  )

dists <- distCosine(
  cbind(std_locs$school_longitude, std_locs$school_latitude),
  cbind(std_locs$longitude, std_locs$latitude)
  )

std_locs$at_school <- dists <= 100

std_locs %>%
  select(DateTime:at_school) %>%
  write_rds("data/processed/at_school.rds")

