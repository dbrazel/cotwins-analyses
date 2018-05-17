# For individuals with at home data before and after age 18 make plots of their
# time at home so that we can predict when they moved out

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(cowplot)

twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
at_home <- read_rds("data/processed/at_home.rds")

# Shift the locations to local time and restrict to 12 AM to 5 AM
at_home <- mutate(at_home, DateTime = DateTime + minutes(sample_timezone)) %>%
  filter(hour(DateTime) %in% c(0, 1, 2, 3, 4))

# Get ages for at_home
at_home <- left_join(at_home, twin_info, by = c("SVID" = "ID1")) %>%
  select(DateTime:SVID, family, sex = Sex1, Birth_Date) %>%
  mutate(sex = sex - 1,
         test_age = as.numeric(as_date(DateTime) - Birth_Date) / 365)

at_home <- group_by(at_home, user_id = Michigan_ID, week = week(DateTime), year = year(DateTime)) %>%
  summarize(home_frac = sum(at_home) / n(), test_age = mean(test_age), sex = first(sex), family = first(family), month = first(month(DateTime, label = T)))

at_home <- na.omit(at_home)

# Only consider twins with data after age 18 and sort by user_id
at_home <- at_home %>%
  group_by(user_id) %>%
  filter(max(test_age) > 18) %>%
  ungroup() %>%
  arrange(user_id)

# Assign each twin an incrementing number for easier reference in the spreadsheet
at_home_twins <- tibble(user_id = unique(at_home$user_id), user_id_inc = 1:250)
at_home <- left_join(at_home, at_home_twins)

at_home_plot <- ggplot(at_home, aes(test_age, home_frac, color = month)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 2/7) +
  facet_wrap(~user_id_inc, scales = "free_x")

save_plot("figs/at_home_indiv.pdf", at_home_plot, base_width = 60, base_height = 50, limitsize = F)
