library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(nlme)

sub_use <- read_rds("data/processed/remote_substance_use.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
at_home <- read_rds("data/processed/at_home.rds")

# Shift the locations to local time and restrict to 12 AM to 5 AM
at_home <- mutate(at_home, DateTime = DateTime + minutes(sample_timezone)) %>%
  filter(hour(DateTime) %in% c(0, 1, 2, 3, 4))

# For each survey response get the twin's age at that time, centered at 16.5
# and switch sex to a 0/1 coding from 1/2.
sub_use <- left_join(sub_use, id_mapping_long, by = c("user_id" = "alternate_id"))
sub_use <- left_join(sub_use, twin_info, by = c("SVID" = "ID1"))
sub_use <- select(
  sub_use,
  user_id,
  date_completed,
  any_substance_use,
  alc_use,
  alc_quantity_drinks_per_day,
  alc_freq_days_per_week,
  Sex1,
  Birth_Date,
  Test_Date,
  family
)
sub_use <- mutate(
  sub_use,
  test_age = as.numeric(as_date(date_completed) - Birth_Date) / 365,
  test_age = test_age - 16.5,
  sex = Sex1 - 1
)

# Time at home isn't meaningful after age 18 or so
#sub_use <- filter(sub_use, test_age <= 1.5)

# If the twin didn't use alcohol that week, set the phenos to 0
sub_use$alc_quantity_drinks_per_day[!sub_use$any_substance_use] <- 0
sub_use$alc_quantity_drinks_per_day[!sub_use$alc_use] <- 0
sub_use$alc_freq_days_per_week[!sub_use$any_substance_use] <- 0
sub_use$alc_freq_days_per_week[!sub_use$alc_use] <- 0

# Calculate log-transformed DPW
sub_use <- mutate(
  sub_use,
  drinks_per_week = alc_quantity_drinks_per_day * alc_freq_days_per_week,
  drinks_per_week = log(drinks_per_week + 1)
)

# Get rid of subjects with very few responses
valid_ids <- sub_use %>%
  group_by(user_id) %>%
  summarize(N = n()) %>%
  filter(N > 5)

sub_use <- filter(sub_use, user_id %in% valid_ids$user_id)

sub_use["home_frac"] <- NA
sub_use["home_n"] <- NA

sub_use <- filter(sub_use, date_completed >= min(at_home$DateTime))

for (i in 1:nrow(sub_use)) {
  subset <- filter(at_home, Michigan_ID == sub_use[[i, "user_id"]], (DateTime > sub_use[[i, "date_completed"]] - days(3) - hours(12)), (DateTime < sub_use[[i, "date_completed"]] + days(3) + hours(12)))
  if(nrow(subset) == 0) {next()}
  sub_use[i, "home_n"] <- nrow(subset)
  sub_use[i, "home_frac"] <- sum(subset$at_home) / nrow(subset)
  print(i)
}

sub_use <- select(sub_use, user_id, family, test_age, drinks_per_week, sex, home_frac, home_n)
sub_use <- na.omit(sub_use)

# Get rid of subjects with very few responses
valid_ids <- sub_use %>%
  group_by(user_id) %>%
  summarize(N = n()) %>%
  filter(N > 5)

sub_use <- filter(sub_use, user_id %in% valid_ids$user_id)

# Convert to a single variable, dummy-coded
dpw <- select(sub_use, user_id, family, var = drinks_per_week, test_age)
dpw["d_dpw"] <- 1
dpw["d_home"] <- 0
dpw["grp"] <- "dpw"

home <- select(sub_use, user_id, family, var = home_frac, test_age)
home["d_dpw"] <- 0
home["d_home"] <- 1
home["grp"] <- "home"

multivariate = rbind(dpw, home)

multi_model <- nlme(var ~ d_dpw * (b_1i + b_2i * test_age) + d_home * (h_1i + h_2i * test_age), data = multivariate, fixed = b_1i + b_2i + h_1i + h_2i ~ 1, random = b_1i + b_2i + h_1i + h_2i ~ 1, group = ~user_id, start = c(0, 0.01, 0, 0.01), weights = varIdent(c(home = 0.5), form = ~1|grp), na.action = na.omit)
multi_model_twin <- nlme(var ~ d_dpw * (b_1i + b_2i * test_age) + d_home * (h_1i + h_2i * test_age), data = multivariate, fixed = b_1i + b_2i + h_1i + h_2i ~ 1, random = b_1i + b_2i + h_1i + h_2i ~ 1|family/user_id, start = c(0, 0.01, 0, 0.01), weights = varIdent(c(home = 0.5), form = ~1|grp), na.action = na.omit)

write_rds(multi_model, "data/models/at_home_dpw_model.rds")
