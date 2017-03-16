library(lubridate)
library(readr)
library(dplyr)
library(geosphere)

tp_dists <- tibble(tp_id = character(), zygosity = character(), 
    DateTime = as.POSIXct(character()), distance = numeric())
tp_mean_dists <- tibble(tp_id = character(), zygosity = character(), mean_dist = numeric(), n = numeric())

std_locs <- read_rds('/work/KellerLab/david/cotwins-analyses/data/processed/std_locations.rds')
twin_info <- read_rds('/work/KellerLab/david/cotwins-analyses/data/raw/Robin_paper-entry_12-6-16.rds')
id_mapping <- read_csv('/work/KellerLab/david/cotwins-analyses/data/processed/id_mapping.csv', col_types = 
                         cols(
                           T1 = col_character(),
                           T2 = col_character(),
                           T1_alternate_id = col_character(),
                           T2_alternate_id = col_character(),
                           bestzygos = col_character()
                         ))

id_mapping <- bind_rows(tibble(SVID = id_mapping$T1, Colorado_ID = id_mapping$T1_alternate_id), 
    tibble(SVID = id_mapping$T2, Colorado_ID = id_mapping$T2_alternate_id))
twin_info <- left_join(twin_info, id_mapping, by = c('ID1' = 'SVID'))

# Remove rows with missing fields
std_locs <- std_locs[complete.cases(std_locs), ]

# Loop over the twin pairs and select rows with the right twin IDs, where
# both twins have points at the same time
for (twin_pair in seq(to = nrow(twin_info), by = 2)) {
    t1_locs <- filter(std_locs, Colorado_ID == twin_info$Colorado_ID[twin_pair])
    t2_locs <- filter(std_locs, Colorado_ID == twin_info$Colorado_ID[(twin_pair + 1)])
    t1_locs <- filter(t1_locs, DateTime %in% t2_locs$DateTime)
    t2_locs <- filter(t2_locs, DateTime %in% t1_locs$DateTime)
    
    if (nrow(t1_locs) == 0) {next()}
    
    # Ensure that the time points are aligned correctly
    t_locs <- left_join(t1_locs, t2_locs, by = 'DateTime')
    
    # Get the distance for all points on the WGS84 ellipsoid
    t_dist <- distGeo(t_locs[, c('longitude.x', 'latitude.x')], t_locs[, c('longitude.y', 'latitude.y')])
    
    tp_dists <- bind_rows(tp_dists, tibble(tp_id = t_locs$Colorado_ID.x, 
        zygosity = rep(twin_info$bestzygos[twin_pair], nrow(t_locs)), DateTime = t_locs$DateTime,
        distance = t_dist))

    tp_mean_dists[nrow(tp_mean_dists) + 1, ] <- list(twin_info$Colorado_ID[twin_pair], 
        twin_info$bestzygos[twin_pair], mean(t_dist), nrow(t_locs))

    print(as.character(twin_pair))
}

write_rds(tp_dists, '/work/KellerLab/david/cotwins-analyses/data/processed/twin_distances.rds')
write_rds(tp_mean_dists, '/work/KellerLab/david/cotwins-analyses/data/processed/twin_mean_distances.rds')
