# A plot of fraction of points at home by day of week and hour of day

library(readr)
library(cowplot)
library(dplyr)
library(lubridate)
library(viridis)

twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

home <- read_rds("data/processed/at_home.rds")
home <- na.omit(home)

home <- mutate(home, DateTime = DateTime + minutes(sample_timezone))
home <- left_join(home, twin_info, by = c("SVID" = "ID1")) %>%
  select(DateTime:SVID, family, Birth_Date) %>%
  mutate(test_age = as.numeric(as_date(DateTime) - Birth_Date) / 365) %>%
  filter(test_age < 18)

home <-
  mutate(
    home,
    d = wday(DateTime, label = T),
    h = hour(DateTime),
    h = parse_factor(as.character(h), as.character(seq(23, 0)))) %>%
  group_by(d, h) %>%
  summarize(frac = sum(at_home) / n())

plt <- ggplot(home, aes(d, h)) +
  geom_tile(aes(fill = frac)) +
  xlab("") +
  ylab("Hour of day") +
  scale_fill_viridis(name = "Fraction of\ntime at home")


save_plot("figs/at_home_week.pdf", plt, base_aspect_ratio = 1.2)
