# Find the distance between twins

library(lubridate)
library(readr)
library(dplyr)
library(geosphere)

std_locs <- read_rds("data/processed/std_locations_filled.rds")
id_mapping <- read_csv("data/processed/id_mapping.csv", col_types = "ccccc")

std_locs <- na.omit(std_locs)

tp_dists <- tibble(
  tp_id = character(),
  zygosity = character(),
  DateTime = as.POSIXct(character()),
  distance = numeric()
)

# Loop over the twin pairs and select rows with the right twin IDs,
# where the twins have points at the same time
for (i in 1: nrow(id_mapping)) {
  row <- id_mapping[i, ]
  
  t1_locs <- filter(std_locs, Michigan_ID == row$T1_alternate_id)
  t2_locs <- filter(std_locs, Michigan_ID == row$T2_alternate_id)
  
  t1_locs <- filter(t1_locs, DateTime %in% t2_locs$DateTime)
  t2_locs <- filter(t2_locs, DateTime %in% t1_locs$DateTime)
  
  if (nrow(t1_locs) == 0) {next()}
  
  # Ensure that the time points are aligned correctly
  t_locs <- left_join(t1_locs, t2_locs, by = 'DateTime')
  
  t_dist <- distGeo(t_locs[, c('longitude.x', 'latitude.x')], t_locs[, c('longitude.y', 'latitude.y')])
  
  tp_dists <-
    bind_rows(
      tp_dists,
      tibble(
        tp_id = t_locs$Michigan_ID.x,
        zygosity = rep(row$bestzygos, nrow(t_locs)),
        DateTime = t_locs$DateTime,
        distance = t_dist
      )
    )
}

tp_mean_dists <- group_by(tp_dists, tp_id, zygosity) %>%
  summarize(mean_dist = mean(distance), n = n())

write_rds(tp_dists, 'data/processed/twin_distances.rds')
write_rds(tp_mean_dists, 'data/processed/twin_mean_distances.rds')

