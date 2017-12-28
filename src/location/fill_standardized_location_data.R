# For iOS users, fill forward missing standardized locations to account for
# the fact that iOS will not record a new point until the user moves

library(readr)
library(dplyr)
library(lubridate)

std_locs <- read_rds("data/processed/std_locations.rds")

# Add a column to hold the original datetime a row had before it was copied
std_locs["orig_datetime"] <- std_locs$DateTime

# Define how far back to search for a point
time_valid <- hours(12)

# If a row is missing for an iOS user and that twin has a sufficiently
# recent point, copy it
for (i in 1:nrow(std_locs)) {
  if (is.na(std_locs[[i, "id"]])) {
    valid_rows <- filter(
      std_locs,
      Colorado_ID == std_locs[[i, "Colorado_ID"]],
      orig_datetime >= std_locs[[i, "DateTime"]] - time_valid,
      orig_datetime < std_locs[[i, "DateTime"]],
      !is.na(std_locs[[i, "id"]]),
      app_type == "iOS"
      ) %>%
      arrange(desc(DateTime))
    
    # Go to the next row 
    if (nrow(valid_rows) == 0) next()
    
    std_locs[i, "latitude":"app_type"] <- valid_rows[1, "latitude":"app_type"]
    std_locs[i, "orig_datetime"] <- valid_rows[1, "orig_datetime"]
  }
}
