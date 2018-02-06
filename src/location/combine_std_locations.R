#Concatenate the standardized locations and convert them to an rds file

library(readr)

system('cat /work/KellerLab/david/cotwins-analyses/data/processed/std* > /work/KellerLab/david/cotwins-analyses/data/processed/std_locations.csv')

std_locs <- read_csv('/work/KellerLab/david/cotwins-analyses/data/processed/std_locations.csv', col_types = 'Tcnnincc', col_names = c('DateTime', 'Michigan_ID', 'latitude', 'longitude', 'id', 'accuracy', 'sample_timezone', 'app_type'))

write_rds(std_locs, '/work/KellerLab/david/cotwins-analyses/data/processed/std_locations.rds')
