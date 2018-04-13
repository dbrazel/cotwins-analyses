# Make a static plot and an animated GIF of the locations in downtown Denver

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gganimate)

locs <- read_rds(
  "data/processed/Michigan_DB_user_location_04_12_18_cleaned.rds"
  )

downtown_locs <- locs %>%
  filter(
    latitude < 39.753696,
    longitude > -105.005600, 
    latitude > 39.740365,
    longitude < -104.986696) %>%
  mutate(
    sample_hour = hour(sample_time + minutes(sample_timezone))
    )

p <- 
  ggplot(
    downtown_locs,
    aes(longitude, latitude, color = user_id, frame = sample_hour)
    ) +
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

gganimate(p, "figs/downtown_denver.gif")
