# Plot the fraction of points at each of the top three schools for each twin

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

df <- read_rds("data/processed/twin_by_school.rds")

# Don't use twins with very few points
df <- filter(df, total_points > 50) 

# Up the font size
theme_set(theme_gray(base_size = 16))

# Get the first, second, and third ranked schools for each twin
# Rearrange to tidy format, order twin_id so that the plot will
# sort from greatest to least, left to right
df %>%
  group_by(twin_id) %>%
  summarize(
    `1` = first(school_points_frac),
    `2` = nth(school_points_frac, 2),
    `3` = nth(school_points_frac, 3)
    ) %>%
  gather(`1`:`3`, key = "rank", value = "fraction") %>%
  mutate(twin_id = fct_reorder(twin_id, fraction, sum, .desc = T)) %>%
  ggplot(aes(twin_id, fraction)) +
    geom_col(aes(fill = rank)) +
    labs(x = "Twins", y = "Fraction of Points at School") +
    scale_fill_discrete(name = "School Rank") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggsave("figs/twin_school_frac.pdf", width = 6, height = 4)
  