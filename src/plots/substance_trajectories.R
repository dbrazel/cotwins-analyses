# Plot the trajectories of substance use by age

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)

# Up the font size
theme_set(theme_gray(base_size = 16))

id_mapping_long <- read_csv(
  "data/processed/id_mapping_long.csv",
  col_types = "ccc"
)
remote_sub_use <- read_rds("data/processed/remote_substance_use.rds")
paper_data <- read_rds(
  "data/processed/Robin_paper-entry_2-22-17_cleaned.rds"
)

remote_sub_use_age <-
  remote_sub_use %>%
  left_join(id_mapping_long, by = c("user_id" = "alternate_id")) %>%
  left_join(paper_data, by = c("SVID" = "ID1")) %>%
  mutate(
    age_at_completion =
      as.numeric(as_date(date_completed) - Birth_Date) / 365
    )

# Alcohol
remote_sub_use_age$alc_quantity_drinks_per_day[remote_sub_use_age$any_substance_use == FALSE] = 0
remote_sub_use_age$alc_quantity_drinks_per_day[remote_sub_use_age$alc_use == FALSE] = 0
ggplot(
  remote_sub_use_age,
  aes(x = age_at_completion, y = alc_quantity_drinks_per_day)
  ) +
  geom_smooth() +
  xlab("Age in years") +
  ylab("Drinks per day") +
  xlim(14.5, 19)

ggsave("figs/alcohol_trajectory.pdf", width = 6, height = 4)

# Marijuana

remote_sub_use_age$mar_freq_times_per_day[remote_sub_use_age$any_substance_use == FALSE] = 0
remote_sub_use_age$mar_freq_times_per_day[remote_sub_use_age$mar_use == FALSE] = 0
ggplot(remote_sub_use_age, aes(x = age_at_completion, y = mar_freq_times_per_day)) + geom_smooth() + xlab("Age in years") + ylab("Marijuana use, times per day") + xlim(14.5, 19)

ggsave("figs/marijuana_trajectory.pdf", width = 6, height = 4)

# Cigarettes

remote_sub_use_age$cig_quantity_per_day[remote_sub_use_age$any_substance_use == FALSE] = 0
remote_sub_use_age$cig_quantity_per_day[remote_sub_use_age$cig_use == FALSE] = 0
ggplot(remote_sub_use_age, aes(x = age_at_completion, y = cig_quantity_per_day)) + geom_smooth() + xlab("Age in years") + ylab("Cigarettes per day") + xlim(14.5, 19)

ggsave("figs/cigarette_trajectory.pdf", width = 6, height = 4)

# E-Cigarettes

remote_sub_use_age$ecig_freq_times_per_day[remote_sub_use_age$any_substance_use == FALSE] = 0
remote_sub_use_age$ecig_freq_times_per_day[remote_sub_use_age$ecig_use == FALSE] = 0
ggplot(remote_sub_use_age, aes(x = age_at_completion, y = ecig_freq_times_per_day)) + geom_smooth() + xlab("Age in years") + ylab("E-Cigarette uses per day") + xlim(14.5, 19)

ggsave("figs/ecigarette_trajectory.pdf", width = 6, height = 4)

