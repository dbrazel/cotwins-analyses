# Get the fraction of points in which each subject is near a home address
# recorded for their family

library(readr)
library(dplyr)
library(geosphere)
library(stringr)

locs <- read_rds("data/processed/std_locations_filled.rds")
id_mapping <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
addresses <- read_rds("data/processed/home_addresses.rds")

# Select only non-missing rows
locs <- na.omit(locs)

locs["at_home"] <- NA
#locs["home_distance"] <- NA

# Get the twin SVID
locs <- left_join(locs, id_mapping, by = c("Michigan_ID" = "alternate_id")) %>%
  select(-bestzygos)

# Get the SVID for the family (the first six characters of the twin SVID)
locs["fam_id"] <- str_sub(locs$SVID, 1, 6)

# Only keep locations from twins with home addresses
locs <- semi_join(locs, addresses, by = c("fam_id" = "FAMILY_SVID"))

# Loop over the locations
for (i in 1:nrow(locs)) {
  # Get home addresses for the twin
  fam_addresses <- addresses %>% filter(FAMILY_SVID == locs$fam_id[[i]])
  
  # Calculate the distance from each home address to the point
  # If distCosine is given one point as one argument and multiple points 
  # as the other, it will broadcast the single point to calculate distances
  dists <- distCosine(c(locs$longitude[[i]], locs$latitude[[i]]), 
                      cbind(fam_addresses$LONGITUDE, fam_addresses$LATITUDE))
  
  # If the point is within 100 meters of any of the home addresses for a twin
  # mark them as at home
  locs$at_home[i] <- any(dists < 100)
  #locs$home_distance[i] <- min(dists)
  
  if (i %% 5000 == 0) {print(i)}
}

write_rds(locs, "data/processed/at_home.rds")
