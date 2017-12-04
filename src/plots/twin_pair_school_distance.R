# Plot the distribution of the distances between
# the schools identified for twin pair

library(readr)
library(dplyr)
library(ggplot2)
library(geosphere)

schools <- read_rds("data/processed/twin_top_school.rds")
id_mapping <- read_csv("data/processed/id_mapping.csv", col_types = "ccccc")

# Get the school for each twin 1 and 2 and eliminate pairs where one or both
# twins have few points at the school or don't have an identified school
schools_join <- id_mapping %>%
  left_join(schools, by = c("T1_alternate_id" = "twin_id")) %>%
  left_join(schools, by = c("T2_alternate_id" = "twin_id")) %>%
  filter(n_school_points.x > 50, n_school_points.y > 50) %>%
  na.omit()

# Calculate the distances
schools_join$dists <- distCosine(
  cbind(schools_join$school_longitude.x, schools_join$school_latitude.x),
  cbind(schools_join$school_longitude.y, schools_join$school_latitude.y)
  )

# Up the font size
theme_set(theme_gray(base_size = 16))

schools_join %>%
  ggplot(aes(dists)) +
  geom_histogram() +
  xlab("Distance (meters)")

ggsave("figs/twin_pair_school_distance.pdf", width = 6, height = 4)
