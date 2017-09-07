# Geocode school addresses, using the Google API

library(readr)
library(jsonlite)
library(urltools)
library(stringr)
library(dplyr)
library(tidyr)

# Bizarrely, the ELSI data uses "†" as a missing value
# The skip and n_max arguments are used because there is
# garbage at the start and end of each "CSV"
# THIS IS FRAGILE AND MAY BREAK ON NEW DATA
public <-
  read_csv("data/external/ELSI_CO_PUBLIC.csv",
           skip = 7,
           n_max = 1872,
           na = c("", "NA", "†", "–", "‡"),
           col_names = c(
             "school_name",
             NA,
             NA,
             "school_id",
             "agency_name",
             "agency_id",
             "address_1",
             "address_2",
             "address_3",
             "address_city",
             "address_state",
             "address_zip",
             "address_zip4"
           ))
private <-
  read_csv("data/external/ELSI_CO_PRIVATE.csv",
           skip = 7,
           n_max = 333,
           na = c("", "NA", "†", "–", "‡"),
           col_names = c(
             "school_name",
             NA,
             "address_1",
             "address_city",
             "address_zip",
             "school_id",
             NA
           ))

# Some entries are malformed like this: '="0806150"'
public <-
  mutate_all(public,
             str_replace_all,
             pattern = '="|"',
             replacement = '')
private <-
  mutate_all(private,
             str_replace_all,
             pattern = '="|"',
             replacement = '')

# Concatenate the addresses
public <- unite(public, "address", address_1, address_city, address_state, sep = ", ", remove = F)
public <- unite(public, "address_full", address, address_zip, sep = " ", remove = F)

private <- unite(private, "address_full", address_1, address_city, address_zip, sep = ", ", remove = F)

public <- select(public, school_name, school_id, agency_name, agency_id, address_full)
private <- select(private, school_name, school_id, address_full)

api_key <- Sys.getenv("GOOGLE_API_KEY")[[1]]

public["latitude"] <- NA
public["longitude"] <- NA
private["latitude"] <- NA
private["longitude"] <- NA

geocode <- function (address) {
  encoded_address <- url_encode(address)
  
  # The API throws occasionally throws a 500 error, which hasn't recurred when retried
  result <- tryCatch({fromJSON(
               paste0(
                 "https://maps.googleapis.com/maps/api/geocode/json?address=",
                 encoded_address,
                 "&key=",
                 api_key
               )
             )},
           error = function(e) {
             return(fromJSON(
                 paste0(
                   "https://maps.googleapis.com/maps/api/geocode/json?address=",
                   encoded_address,
                   "&key=",
                   api_key
                 )
               ))
           })
  # The API can return more than one result, we take the top one
  return(c(result$results$geometry$location$lat[[1]], 
           result$results$geometry$location$lng[[1]]))
}

for (i in 1:nrow(public)) {
  result <- geocode(public[[i, 5]])
  public[i, 6] <- result[[1]]
  public[i, 7] <- result[[2]]
  
  print(i)
  
  # Rate limit
  Sys.sleep(0.1)
}

for (i in 1:nrow(private)) {
  result <- geocode(private[[i, 3]])
  private[i, 4] <- result[[1]]
  private[i, 5] <- result[[2]]
  
  print(i)
  
  # Rate limit
  Sys.sleep(0.1)
}

write_rds(public, "data/processed/public_school_address.rds")
write_rds(private, "data/processed/private_school_address.rds")
