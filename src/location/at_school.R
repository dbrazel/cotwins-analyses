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
        ) <= 200
      )
}

std_locs["at_school"] <- at_school

write_rds(std_locs, "data/processed/at_school_unfiltered.rds")

# Remove individuals where we seem to have failed to identify a school
# (that is, looking at points before age 18, are they all at zero,
# or is there a period of a month or more where they are all at zero.)
to_remove <- c(
  "10473343825718415846",
  "10523881975798960614",
  "10956220549324870118",
  "11721367829140869606",
  "12758166492366115301",
  "12563054890317189606",
  "13237375926974550502",
  "13328117410434126310",
  "14350241506328383974",
  "13658636631925199334",
  "14701744522713764326",
  "14744394060635705830",
  "14871033358480511461",
  "15268244675444609510",
  "15264921250408305126",
  "15123069704779272677",
  "15105520598946550245",
  "15030571462218158566",
  "14960215945076347365",
  "15651941071501595109",
  "16066070780119618021",
  "17033925671561269734",
  "16861585393037480421",
  "16489144711640453606",
  "16240839238530568677",
  "17572161221382574565",
  "17709342514169123301",
  "18391409567331652070",
  "2242992076439884261",
  "2945633638677484006",
  "3505404453353755110",
  "3562092811494232549",
  "3625544947942953445",
  "4014097478797169125",
  "4541330688408490470",
  "4375018766117966310",
  "5394553637001499110",
  "6344070633913061862",
  "6330582079776625126",
  "5818180864071963110",
  "6574513473549046245",
  "6699992588549034469",
  "681211369319698917",
  "7571414726053007846",
  "7428381008752021990",
  "7676950681446322661",
  "7830494544442823141",
  "8035821379331625446",
  "8097104607370940902",
  "8827118698856059366",
  "8562042324871418342",
  "843339933702820325",
  "9117122467906458086",
  "9159551478131659238",
  "9574345145543037413",
  "9561319888524022245"
)
std_locs <- filter(std_locs, !(Michigan_ID %in% to_remove))

write_rds(std_locs, "data/processed/at_school.rds")
