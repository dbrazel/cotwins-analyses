# Accepts the row number of a twin as input
# Calculates how many standardized points
# the twin has within 100 meters of each school

library(readr)
library(dplyr)
library(geosphere)

# Get the first command line argument
args <- commandArgs(trailingOnly = T)
twin <- as.numeric(args[1])

std_locs <- read_rds("data/processed/std_locations.rds")
id_mapping <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
schools <- read_rds("data/processed/all_school_address.rds")

# Subset the locs to the twin
twin_id <- id_mapping$alternate_id[[twin]]
std_locs <- filter(std_locs, Colorado_ID == twin_id)
std_locs <- filter(std_locs, complete.cases(std_locs))

# Count the number of locations within 100 meters of each school
schools["N"] <- 0

for (i in 1:nrow(schools)) {
  dists <- distCosine(c(schools$longitude[[i]], schools$latitude[[i]]), cbind(std_locs$longitude, std_locs$latitude))
  schools$N[[i]] <- sum(dists <= 100)
}
