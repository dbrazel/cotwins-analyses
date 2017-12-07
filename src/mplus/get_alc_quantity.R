# Get reported drinks per day and age at report and output in wide format
# arranged for Mplus

library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

sub_use <- read_rds("data/processed/remote_substance_use.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds")
id_mapping <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

# For each survey response get the twin's age at that time
sub_use <- left_join(sub_use, id_mapping, by = c("user_id" = "alternate_id"))
sub_use <- left_join(sub_use, twin_info, by = c("SVID" = "ID1"))
sub_use <- select(
  sub_use,
  user_id,
  date_completed,
  any_substance_use,
  alc_use,
  alc_quantity_drinks_per_day,
  Birth_Date,
  Test_Date
  )
sub_use <- mutate(sub_use, Test_Age = as.numeric(as_date(date_completed) - Birth_Date) / 365)

# If the twin didn't use alcohol that week, set DPD to 0
sub_use$alc_quantity_drinks_per_day[!sub_use$any_substance_use] <- 0
sub_use$alc_quantity_drinks_per_day[!sub_use$alc_use] <- 0

# Convert age and DPD to wide format
sub_use <- sub_use %>%
  arrange(user_id, Test_Age) %>%
  group_by(user_id) %>%
  mutate(dpd = row_number(Test_Age)) %>%
  ungroup() %>%
  select(user_id, alc_quantity_drinks_per_day, Test_Age, dpd)

sub_use_dpd <- sub_use %>%
  select(-Test_Age) %>%
  spread(dpd, alc_quantity_drinks_per_day, sep = "_")

sub_use_age <- sub_use %>%
  mutate(age = dpd) %>%
  select(-alc_quantity_drinks_per_day, -dpd) %>%
  spread(age, Test_Age, sep = "_")

sub_use <- bind_cols(select(sub_use_dpd, -user_id), select(sub_use_age, -user_id))

write_csv(sub_use, "data/processed/alc_quantity_mplus.dat", col_names = F, na = "*")
