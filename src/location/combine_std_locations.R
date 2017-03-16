#Concatenate the standardized locations and convert them to an rds file

library(readr)

system('cat /work/KellerLab/david/cotwins-analyses/data/processed/std* > /work/KellerLab/david/cotwins-analyses/data/processed/std_locations.csv')

std_locs <- read_csv('std_locations.csv', col_types = 'Tcnn', col_names = c('DateTime', 'Colorado_ID', 'latitude', 'longitude'))

write_rds(std_locs, '/work/KellerLab/david/cotwins-analyses/data/processed/std_locations.rds')
