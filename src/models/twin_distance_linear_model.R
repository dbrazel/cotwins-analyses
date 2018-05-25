library(readr)
library(dplyr)
library(lmerTest)
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
tp_dists_sum <-
  group_by(tp_dists,
           tp_id,
           w = week(DateTime),
           y = year(DateTime)) %>%
  summarise(
    zygosity = first(zygosity),
    distance = mean(distance),
    age = mean(age)
  )

tp_dists_sum_dz <- filter(tp_dists_sum, zygosity == "DZ")
tp_dists_sum_mz <- filter(tp_dists_sum, zygosity == "MZ")
tp_dists_sum_os <- filter(tp_dists_sum, zygosity == "OS")

mz_ml <-
  gamm4(distance ~ s(age), random = ~ (1 |
                                         tp_id), data = tp_dists_sum_mz)
dz_ml <-
  gamm4(distance ~ s(age), random = ~ (1 |
                                         tp_id), data = tp_dists_sum_dz)
os_ml <-
  gamm4(distance ~ s(age), random = ~ (1 |
                                         tp_id), data = tp_dists_sum_os)

mz_pred <- predict(mz_ml$gam, se.fit = T)
dz_pred <- predict(dz_ml$gam, se.fit = T)
os_pred <- predict(os_ml$gam, se.fit = T)

mz_plot_data <-
  tibble(
    age = tp_dists_sum_mz$age,
    distance = mz_pred$fit,
    dist_lower = mz_pred$fit - 1.96 * mz_pred$se.fit,
    dist_upper = mz_pred$fit + 1.96 * mz_pred$se.fit
  )
dz_plot_data <-
  tibble(
    age = tp_dists_sum_dz$age,
    distance = dz_pred$fit,
    dist_lower = dz_pred$fit - 1.96 * dz_pred$se.fit,
    dist_upper = dz_pred$fit + 1.96 * dz_pred$se.fit
  )
os_plot_data <-
  tibble(
    age = tp_dists_sum_os$age,
    distance = os_pred$fit,
    dist_lower = os_pred$fit - 1.96 * os_pred$se.fit,
    dist_upper = os_pred$fit + 1.96 * os_pred$se.fit
  )

mz_plot_data$zygosity = "MZ"
dz_plot_data$zygosity = "DZ"
os_plot_data$zygosity = "OS"

plot_data <-
  bind_rows(mz_plot_data, dz_plot_data) %>% bind_rows(os_plot_data)
ggplot(plot_data, aes(age, distance, fill = zygosity, color = zygosity)) +
  geom_ribbon(aes(ymax = dist_upper, ymin = dist_lower), alpha = 0.2) +
  geom_line()
