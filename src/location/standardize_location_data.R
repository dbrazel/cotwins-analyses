# Accepts the row number of the first twin in a twin pair as input
# Aligns locations for that twin pair to a 10 minute grid

library(readr)
library(lubridate)
library(dplyr)

print('Libraries loaded')

# Get the first command line argument
args <- commandArgs(trailingOnly = T)
twin_pair <- as.numeric(args[1])

print('Loading location')
locations <- read_rds('/work/KellerLab/david/cotwins-analyses/data/processed/Michigan_DB_user_location_09_25_17_cleaned.rds')
print('Loading twin info')
twin_info <- read_rds('/work/KellerLab/david/cotwins-analyses/data/raw/Robin_paper-entry_2-22-17.rds')
print('Loading id mapping')
id_mapping <- read_csv('/work/KellerLab/david/cotwins-analyses/data/processed/id_mapping.csv', col_types = cols(
                           T1 = col_character(),
                           T2 = col_character(),
                           T1_alternate_id = col_character(),
                           T2_alternate_id = col_character(),
                           bestzygos = col_character()
                         ))

# Put twins on separate rows and then join to the twin info table
id_mapping <- bind_rows(tibble(SVID = id_mapping$T1, Colorado_ID = id_mapping$T1_alternate_id), tibble(SVID = id_mapping$T2, Colorado_ID = id_mapping$T2_alternate_id))
twin_info <- left_join(twin_info, id_mapping, by = c('ID1' = 'SVID'))

# Get locations corresponding to the twin pair
locations <- filter(locations, user_id %in% twin_info$Colorado_ID[twin_pair:(twin_pair + 1)])

# If the twins have no locations, terminate
if (nrow(locations) == 0) {quit(save = 'no')}

# Get the grid of time points, at a ten minute frequency
time_grid <- seq(min(locations$sample_time), max(locations$sample_time), 60*10)

# Get the cartesian product of the twin IDs and the time grid and add empty columns for lat and long
results <- expand.grid(DateTime = time_grid, Colorado_ID = twin_info$Colorado_ID[twin_pair:(twin_pair + 1)], stringsAsFactors = F)
results[, c('latitude', 'longitude')] <- NA

for (row in seq(to = nrow(results))) {
    subset <- filter(locations, user_id == results[row, 2], sample_time < results[row, 1] + minutes(5), sample_time > results[row, 1] - minutes(5))
    if (nrow(subset) == 0) {next()}
    else {
        results[row, 3] <- mean(subset$latitude, na.rm = T)
        results[row, 4] <- mean(subset$longitude, na.rm = T)
    }
}

write_csv(results, paste0('/work/KellerLab/david/cotwins-analyses/data/processed/std_locations_', as.character(twin_pair), '.csv'), col_names = F)
