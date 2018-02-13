# For every location point, count the number of bars, number of liquor stores,
# number of night clubs, the total number of places, and the fraction of bars,
# night clubs, and liquor stores from the stored Google Places data

library(readr)
library(dplyr)
library(jsonlite)
library(purrr)

std_locs <- read_rds("data/processed/std_locations_filled.rds")
google_places <- read_rds("data/raw/Michigan_DB_google_location_09_25_17.rds")
google_key <- read_rds("data/raw/Michigan_DB_user_location_google_location_09_25_17.rds")

std_locs <- na.omit(std_locs)

# Get the list of classifications (types) for each place
extract_types <- function (json_string) {
  fromJSON(json_string)$types
}
types <- map(google_places$google_location, extract_types)
google_places <- tibble(id = google_places$id, types = types)

# Remove places corresponding to absent points
google_key <- filter(google_key, user_location_id %in% std_locs$id)
google_key <- filter(google_key, google_location_id %in% google_places$id)

std_locs["n_bars"] <- 0
std_locs["n_liquor_stores"] <- 0
std_locs["n_night_clubs"] <- 0
std_locs["n_total"] <- 0

# For each point, update the counts
for (i in 1:nrow(std_locs)) {
  places <- filter(google_key, user_location_id == std_locs[[i, "id"]])
  
  if(nrow(places) == 0) {next()}
  
  std_locs[i, "n_total"] <- nrow(places)
  
  for (j in 1:nrow(places)) {
    place_types <-
      google_places[places[[j, "google_location_id"]] == google_places$id, ]$types[[1]]
    if ("bar" %in% place_types) {
      std_locs[i, "n_bars"] <- std_locs[i, "n_bars"] + 1
    }
    if ("night_club" %in% place_types) {
      std_locs[i, "n_night_clubs"] <- std_locs[i, "n_night_clubs"] + 1
    }
    if ("liquor_store" %in% place_types) {
      std_locs[i, "n_liquor_stores"] <- std_locs[i, "n_liquor_stores"] + 1
    }
  }
  if (i %% 1000 == 0) {print(i)}
}

std_locs <- mutate(std_locs, alc_frac = (n_bars + n_liquor_stores + n_night_clubs) / n_total)

write_rds(std_locs, "data/processed/alcohol_use_exposure.rds")
