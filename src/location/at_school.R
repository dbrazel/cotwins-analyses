# Annotate each standardized location point with whether it is within
# 100 meters of a Colorado high school

library(readr)
library(dplyr)
library(geosphere)

std_locs <- read_rds("data/processed/std_locations_filled.rds")
schools <- read_rds("data/processed/all_school_address.rds")

# Use only non-missing points
std_locs <- na.omit(std_locs)

at_school <- rep(NA, nrow(std_locs))

for (i in 1:nrow(std_locs)) {
  at_school[i] <-
    any(
      distCosine(
        c(std_locs[[i, "longitude"]], std_locs[[i, "latitude"]]),
        cbind(schools$longitude, schools$latitude)
        ) <= 100
      )
}

std_locs["at_school"] <- at_school

write_rds(std_locs, "data/processed/at_school.rds")

