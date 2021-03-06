# For iOS users, fill forward missing standardized locations to account for
# the fact that iOS will not record a new point until the user moves

library(readr)
library(dplyr)
library(lubridate)

std_locs <- read_rds('data/processed/std_locations.rds')

# Add a column to hold the original datetime a row had before it was copied
std_locs["orig_datetime"] <- std_locs$DateTime

# Define how far forward in time we will fill a point
time_valid <- hours(12)

# Order the points by user id and then ascending datetime
std_locs <- arrange(std_locs, Michigan_ID, DateTime)

# Use data.table for its update in-place functionality - using tibbles or dfs
# will cause R to create deep copies which is slow
std_locs <- data.table::as.data.table(std_locs)

# If the row is missing and the previous row is from the same iOS user and is
# not missing, and current row DateTime is no more than time_valid after the
# previous row orig_datetime, copy the previous row to the current row
for (i in 1:nrow(std_locs)) {
  if (i %% 1000 == 0) print(i)
  if (i == 1) next()
  
  if (
    is.na(std_locs[[i, 'id']]) &
    !is.na(std_locs[[i - 1, 'id']]) &
    std_locs[[i, 'Michigan_ID']] == std_locs[[i - 1, 'Michigan_ID']] &
    std_locs[[i - 1, 'app_type']] == 'ios' &
    std_locs[[i, 'DateTime']] - time_valid <= std_locs[[i - 1, 'orig_datetime']]
  ) {
    std_locs[i, 3 := std_locs[i - 1, 3]]
    std_locs[i, 4 := std_locs[i - 1, 4]]
    std_locs[i, 5 := std_locs[i - 1, 5]]
    std_locs[i, 6 := std_locs[i - 1, 6]]
    std_locs[i, 7 := std_locs[i - 1, 7]]
    std_locs[i, 8 := std_locs[i - 1, 8]]
    std_locs[i, 9 := std_locs[i - 1, 9]]
  }
}

std_locs <- as_tibble(std_locs)
write_rds(std_locs, 'data/processed/std_locations_filled.rds')
