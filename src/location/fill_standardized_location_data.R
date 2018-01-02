# For iOS users, fill forward missing standardized locations to account for
# the fact that iOS will not record a new point until the user moves

library(readr)
library(dplyr)
library(lubridate)

std_locs <- read_rds('data/processed/std_locations.rds')

# Add a column to hold the original datetime a row had before it was copied
std_locs["orig_datetime"] <- std_locs$DateTime

# Define how far back to search for a point
time_valid <- hours(12)

# Order the points by user id and then ascending datetime
std_locs <- arrange(std_locs, Colorado_ID, DateTime)

to_be_copied <- c(
  'latitude',
  'longitude',
  'id',
  'accuracy',
  'sample_timezone',
  'app_type',
  'orig_datetime'
)

# If the row is missing and the previous row is from the same iOS user and is
# not missing, and the difference between current row DateTime and previous row
# orig_datetime is less than or equal to time_valid, copy the previous row to
# the current row
for (i in 1:nrow(std_locs)) {
  if (i == 1) next()
  if (
    is.na(std_locs[[i, 'id']]) &
    !is.na(std_locs[[i - 1, 'id']]) &
    std_locs[[i, 'Colorado_ID']] == std_locs[[i - 1, 'Colorado_ID']] &
    std_locs[[i - 1, 'app_type']] == 'ios' &
    std_locs[[i, 'DateTime']] - time_valid <= std_locs[[i - 1, 'orig_datetime']]
  ) {
    std_locs[i, to_be_copied] <- std_locs[i - 1, to_be_copied]
  }
}
