library(readr)
library(ggplot2)
library(dplyr)

locs <- read_rds("data/processed/Michigan_DB_user_location_09_25_17_cleaned.rds")

downtown_locs <- locs %>%
  filter(
    latitude < 39.753696,
    longitude > -105.005600, 
    latitude > 39.740365,
    longitude < -104.986696)

p <- 
  ggplot(downtown_locs, aes(longitude, latitude)) +
  coord_map() +
  geom_point(alpha = 0.5) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.background = element_blank())

ggsave(
  "figs/downtown_denver.pdf",
  plot = p,
  width = 10,
  height = 10,
  units = "in")