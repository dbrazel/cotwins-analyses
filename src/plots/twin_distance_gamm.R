library(readr)
library(dplyr)
library(gamm4)
library(lubridate)
library(cowplot)

tp_dists <- read_rds("data/processed/twin_distances.rds")
twin_info <-
  read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <-
  read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
tp_dists <-
  left_join(tp_dists, id_mapping_long, by = c("tp_id" = "alternate_id"))
tp_dists <- select(tp_dists,-bestzygos)
tp_dists <- left_join(tp_dists, twin_info, by = c("SVID" = "ID1"))
tp_dists <-
  select(tp_dists, tp_id, zygosity, DateTime, distance, Birth_Date)
tp_dists <-
  mutate(tp_dists, age = as.numeric(as_date(DateTime) - Birth_Date) / 365)
tp_dists$zygosity = factor(tp_dists$zygosity, levels = c("MZ", "DZ", "OS"))
tp_dists <-
  group_by(tp_dists,
           tp_id,
           d = day(DateTime),
           m = month(DateTime),
           y = year(DateTime)) %>%
  summarise(
    zygosity = first(zygosity),
    distance = mean(distance),
    age = mean(age)
  )

# Winsorize based on the distribution of distance and log transform
tp_dists[tp_dists$distance > 2e5, "distance"] <- 2e5
tp_dists <- mutate(tp_dists, distance = log10(distance + 1))

ml <- gamm4(
  distance ~ zygosity + s(age, by = zygosity),
  random = ~(1 | tp_id), data = tp_dists
  )

pred <- predict(ml$gam, se.fit = T)

plot_data <-
  tibble(
    age = tp_dists$age,
    zygosity = tp_dists$zygosity,
    distance = pred$fit,
    dist_lower = pred$fit - 1.96 * pred$se.fit,
    dist_upper = pred$fit + 1.96 * pred$se.fit
    )

plt <- ggplot(plot_data, aes(age, distance, fill = zygosity, color = zygosity)) +
  geom_ribbon(aes(ymax = dist_upper, ymin = dist_lower), alpha = 0.2) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Age (years)",
    y = "log10 of distance (meters)",
    color = "Zygosity",
    fill = "Zygosity"
  ) +
  theme(legend.direction = "horizontal", legend.position = c(0.35, 0.1)) +
  scale_y_continuous(
    breaks = c(3, 4, 5, 6),
    labels = c(
      "1 km",
      "10 km",
      "100 km",
      "1,000 km")
  )

save_plot("figs/twin_distance_gamm.pdf", plt, base_aspect_ratio = 1.33)
