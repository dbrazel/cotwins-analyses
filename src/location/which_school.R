# For each twin and school, find how many standardized points
# are within 100 meters

library(readr)
library(dplyr)
library(geosphere)

std_locs <- read_rds("data/processed/std_locations.rds")
schools <- read_rds("data/processed/all_school_address.rds")

# Use only non-missing points
std_locs <- na.omit(std_locs)

twins <- unique(std_locs$Michigan_ID)

# Make an empty tibble to hold the results
# One row per twin and school:
# n_school_points - Number of standardized points
#   within 100 meters of the school for the twin
# total_points - Total number of standardized points for the twin
# school_points_frac - n_school_points / total_points
results <- tibble(
  twin_id = character(),
  school_id = character(),
  latitude = numeric(),
  longitude = numeric(),
  n_school_points = integer(),
  total_points = integer(),
  school_points_frac = numeric()
)

# Loop over the twins
for (twin in twins) {
  # For each twin, get their locations and make a tibble to hold their results
  subset <- filter(std_locs, Michigan_ID == twin)
  twin_results <- tibble(
    twin_id = twin,
    school_id = schools$school_id,
    latitude = schools$latitude,
    longitude = schools$longitude,
    n_school_points = NA,
    total_points = NA,
    school_points_frac = NA
    )
  
  # Loop over each school and and find how many of the twin's points
  # are within 100 meters of the school
  for (i in 1:nrow(twin_results)) {
    dists <- distCosine(
      c(twin_results[[i, "longitude"]], twin_results[[i, "latitude"]]),
      cbind(subset$longitude, subset$latitude)
      )
    twin_results[i, "n_school_points"] <- sum(dists <= 100)
    twin_results[i, "total_points"] <- nrow(subset)
    twin_results[i, "school_points_frac"] <-
      twin_results[i, "n_school_points"] / twin_results[i, "total_points"]
  }
  
  # Append the twin's results to the results tibble
  results <- bind_rows(results, twin_results)
  print(twin)
}

# Sort by twin and n_school_points
results <- arrange(results, twin_id, desc(n_school_points))

write_rds(results, "data/processed/twin_by_school.rds")

# Select the top school for each twin
results %>%
  group_by(twin_id) %>%
  summarize(
    school_id = first(school_id),
    school_latitude = first(latitude),
    school_longitude = first(longitude),
    n_school_points = first(n_school_points),
    total_points = first(total_points),
    school_points_frac = first(school_points_frac)
    ) %>%
  write_rds("data/processed/twin_top_school.rds")
