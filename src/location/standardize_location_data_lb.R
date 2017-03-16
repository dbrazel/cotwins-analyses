# Make the load balancer file for all twin pairs

for (twin_pair in seq(to = 670, by = 2)) {
write(paste0('Rscript /work/KellerLab/david/cotwins-analyses/src/location/standardize_location_data.R ', as.character(twin_pair)), '/work/KellerLab/david/cotwins-analyses/src/location/standardize_location_data_lb.txt', append = T)
}
