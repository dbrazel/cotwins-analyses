# Get the fraction of points in which each subject is near a home address
# recorded for their family

library(readr)
library(dplyr)
library(geosphere)
library(stringr)

std_locs <- read_rds("data/processed/std_locations_filled.rds")
id_mapping <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
addresses <- read_rds("data/processed/home_addresses.rds")

# Select only non-missing rows
std_locs <- na.omit(std_locs)

std_locs["at_home"] <- NA
#std_locs["home_distance"] <- NA

# Get the twin SVID
std_locs <- left_join(std_locs, id_mapping, by = c("Michigan_ID" = "alternate_id")) %>%
  select(-bestzygos)

# Get the SVID for the family (the first six characters of the twin SVID)
std_locs["fam_id"] <- str_sub(std_locs$SVID, 1, 6)

# Only keep locations from twins with home addresses
std_locs <- semi_join(std_locs, addresses, by = c("fam_id" = "FAMILY_SVID"))

# Loop over the locations
for (i in 1:nrow(std_locs)) {
  # Get home addresses for the twin
  fam_addresses <- addresses %>% filter(FAMILY_SVID == std_locs$fam_id[[i]])
  
  # Calculate the distance from each home address to the point
  # If distCosine is given one point as one argument and multiple points 
  # as the other, it will broadcast the single point to calculate distances
  dists <- distCosine(c(std_locs$longitude[[i]], std_locs$latitude[[i]]), 
                      cbind(fam_addresses$LONGITUDE, fam_addresses$LATITUDE))
  
  # If the point is within 100 meters of any of the home addresses for a twin
  # mark them as at home
  std_locs$at_home[i] <- any(dists < 100)
  #std_locs$home_distance[i] <- min(dists)
  
  if (i %% 5000 == 0) {print(i)}
}

# Remove individuals where we seem to have the wrong home address (time at zero)
to_remove <- c(
  "11248670135330017765",
  "13229567082602041830",
  "13083450721665028581",
  "12927325565745566181",
  "12895567701863240166",
  "12864640847326745061",
  "12762581617064350181",
  "12758166492366115301",
  "13237375926974550502",
  "13328117410434126310",
  "13615620051371889126",
  "1367183437562122725",
  "13819409695296524773",
  "13912542496699126246",
  "14744394060635705830",
  "14484051301928669670",
  "14960215945076347365",
  "15105520598946550245",
  "15178091693452956134",
  "15948128509447115237",
  "1566179732500255205",
  "15537000833807815141",
  "16240839238530568677",
  "16499539579147325926",
  "16815671524313338341",
  "16886821518747177445",
  "17115357253202481637",
  "17769492594624565734",
  "17721693856920900070",
  "17709342514169123301",
  "17683149342714302950",
  "1791173437335081446",
  "18050943746688815590",
  "1859993660825670117",
  "1946851033841209830",
  "1951219933264286181",
  "2242992076439884261",
  "2615817593484808677",
  "2349592786307650021",
  "32551469132550630",
  "4317829319896535525",
  "3938387966544122342",
  "4014097478797169125",
  "5010840317728133606",
  "504073031152046566",
  "5042799590284464614",
  "610358495308354022",
  "61904100253897190",
  "6344070633913061862",
  "6487928271721140710",
  "6699992588549034469",
  "7657058238604448229",
  "7616866149110190566",
  "7573524569261019621",
  "7248852988431176165",
  "7830494544442823141",
  "8065750438904271333",
  "8097104607370940902",
  "8279711307737469413",
  "8495451171156726246",
  "8578982621091992037",
  "9348967687765037541",
  "940878216463323621",
  "9479140187580207590",
  "9610836922579882469",
  "9855660461613388261",
  "9953483755916825062"
)

std_locs <- filter(std_locs, !(Michigan_ID %in% to_remove))

write_rds(std_locs, "data/processed/at_home.rds")
