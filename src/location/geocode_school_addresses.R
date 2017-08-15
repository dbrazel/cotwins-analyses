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
public <- unite(public, "address", address, address_zip, sep = " ", remove = F)

private <- unite(private, "address", address_1, address_city, address_zip, sep = ", ", remove = F)
