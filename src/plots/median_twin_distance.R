# Make a plot of the distribution of the median
# distance between twins, grouped by zygosity

library(readr)
library(ggplot2)
library(dplyr)

dists <- read_rds("data/processed/twin_distances.rds")

dists <- dists %>%
  group_by(tp_id) %>%
  summarize(
    zygosity = first(zygosity),
    N = n(),
    med_dist = median(distance))

p <- dists %>%
  filter(N > 100) %>%
  ggplot(aes(med_dist, fill = zygosity)) +
  geom_density(alpha = 0.5, bw = "SJ") +
  scale_x_log10() +
  xlab("Median twin distance (meters)") +
  ylab("Density")

ggsave(
  "figs/median_twin_distance.pdf",
  plot = p,
  width = 6,
  height = 4,
  units = "in"
  )
