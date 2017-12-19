# Fit a linear growth model to drinks per day using nlme and
# write out the random slopes and intercepts

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(nlme)
library(broom)

sub_use <- read_rds("data/processed/remote_substance_use.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds")
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

# For each survey response get the twin's age at that time
sub_use <- left_join(sub_use, id_mapping_long, by = c("user_id" = "alternate_id"))
sub_use <- left_join(sub_use, twin_info, by = c("SVID" = "ID1"))
sub_use <- select(
  sub_use,
  user_id,
  date_completed,
  any_substance_use,
  alc_use,
  alc_quantity_drinks_per_day,
  Birth_Date,
  Test_Date,
  family
)
sub_use <- mutate(sub_use, Test_Age = as.numeric(as_date(date_completed) - Birth_Date) / 365)

# If the twin didn't use alcohol that week, set DPD to 0
sub_use$alc_quantity_drinks_per_day[!sub_use$any_substance_use] <- 0
sub_use$alc_quantity_drinks_per_day[!sub_use$alc_use] <- 0

# Fit a linear growth model with age at assessment as the time metric
# We adjust the intercept to correspond to age 14 and
# group by twin within family
ml <-
  nlme(
    alc_quantity_drinks_per_day ~ b_1i + b_2i * (Test_Age-14),
    data = sub_use,
    fixed = b_1i + b_2i ~ 1,
    random = b_1i + b_2i ~ 1|family/user_id,
    na.action = "na.omit",
    start = c(0, 0)
    )

# Get the random effect estimates
re_est <- tidy(ml)

# Extract the twin ID
re_est <- separate(re_est, level, into = c("family", "twin"), sep = "/")

write_rds(re_est, "data/models/dpd_linear_random_effects.rds")
