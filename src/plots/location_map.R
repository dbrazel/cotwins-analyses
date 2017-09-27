library(readr)
library(ggplot2)
library(dplyr)
library(viridis)

locs <- read_rds("data/processed/Michigan_DB_user_location_05_31_17_cleaned.rds") %>% sample_n(1000000)
states <- map_data("state")

theme_set(theme_gray(base_size = 16))

nas <- data.frame(matrix(NA, nrow = nrow(locs) - nrow(states), ncol = 6))
names(nas) <- names(states)
states <- rbind(states, nas)
locs <- bind_cols(locs, states)

ggplot(locs) + 
geom_polygon(aes(long, lat, group = group), fill = "white", colour = "black") + 
geom_bin2d(aes(longitude, latitude), alpha = 0.5, binwidth = c(0.05, 0.05)) + 
coord_map(xlim = c(-110, -101), ylim = c(36, 42)) +
scale_fill_viridis() + 
xlab("Longitude") +
ylab("Latitude") +
annotate("text", x = -104.9847, y = 39.73915, label = "Denver") +
annotate("text", x = -105.08442, y = 40.58526, label = "Ft. Collins") +
annotate("text", x = -104.82136, y = 38.83388, label = "Colorado Springs")

ggsave("figs/locs_map.pdf")
