library(readr)
library(dplyr)
library(lmerTest)
library(lubridate)

tp_dists <- read_rds("data/processed/twin_distances.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
haven::zap_formats() %>%
haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
tp_dists <- left_join(tp_dists, id_mapping_long, by = c("tp_id" = "alternate_id"))
tp_dists <- select(tp_dists, -bestzygos)
tp_dists <- left_join(tp_dists, twin_info, by = c("SVID" = "ID1"))
tp_dists <- select(tp_dists, tp_id, zygosity, DateTime, distance, Birth_Date)
tp_dists <- mutate(tp_dists, age = as.numeric(as_date(DateTime) - Birth_Date) / 365)
tp_dists$zygosity = factor(tp_dists$zygosity, levels = c("MZ", "DZ", "OS"))
tp_dists_sum <- group_by(tp_dists, tp_id, w = week(DateTime), y = year(DateTime)) %>%
  summarise(zygosity = first(zygosity), distance = mean(distance), age = mean(age))
