# This script geocodes the home addresses from Amy, using the Google API

library(readr)
library(jsonlite)
library(urltools)

addresses <- read_csv("data/raw/COTWIN_ADDY_ALL.csv")
addresses["LATITUDE"] <- NA
addresses["LONGITUDE"] <- NA

# Google and Open Street Map have no record of these addresses
# but, for some, the USPS does. For now, I'm simply removing them
addresses <- addresses[-c(225, 254, 349),]

# This will only work if the env file has been sourced first
api_key <- Sys.getenv("GOOGLE_API_KEY")[[1]]

geocode <- function (address) {
  encoded_address <- url_encode(address)
  
  r <-
    fromJSON(
      paste0(
        "https://maps.googleapis.com/maps/api/geocode/json?address=",
        encoded_address,
        "&key=",
        api_key
      )
    )
  
  # The API can return more than one result, we take the top one
  return(c(r$results$geometry$location$lat[[1]], r$results$geometry$location$lng[[1]]))
}

# Loop over the addresses and get the lats and longs
for (i in 1:nrow(addresses)) {
  result <- geocode(addresses[i, 2])
  addresses[i, 3] <- result[[1]]
  addresses[i, 4] <- result[[2]]
  
  # Rate limit
  Sys.sleep(0.1)
}

write_rds(addresses, "data/processed/home_addresses.rds")