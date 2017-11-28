library(readr)
library(dplyr)
library(lubridate)

sub_use <- read_rds("data/processed/remote_substance_use.rds")
twin_info <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds")
id_mapping <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

sub_use <- left_join(sub_use, id_mapping, by = c("user_id" = "alternate_id"))
sub_use <- left_join(sub_use, twin_info, by = c("SVID" = "ID1"))
sub_use <- select(sub_use, user_id, date_completed, any_substance_use, alc_use, alc_quantity_drinks_per_day, Birth_Date, Test_Date)
sub_use <- mutate(sub_use, Test_Age = as.numeric(as_date(date_completed) - Birth_Date) / 365)

sub_use$alc_quantity_drinks_per_day[!sub_use$any_substance_use] <- 0
sub_use$alc_quantity_drinks_per_day[!sub_use$alc_use] <- 0

results <- tibble("user_id" = unique(sub_use$user_id))
results[c('dpd1', 'dpd2', 'dpd3', 'dpd4', 'dpd5', 'dpd6', 'dpd7', 'dpd8', 'dpd9', 'dpd10', 'age1', 'age2', 'age3', 'age4', 'age5', 'age6', 'age7', 'age8', 'age9', 'age10')] <- NA

for (row in seq(to = nrow(results))) {
  subset <- filter(sub_use, user_id == results[[row, "user_id"]])
  subset <- arrange(subset, Test_Age)
  if (nrow(subset) == 0) {next()}
  for (response in seq(to = nrow(subset))) {
    if (response > 10) {break()}
    results[row, response + 1] <- subset[[response, "alc_quantity_drinks_per_day"]]
    results[row, response + 11] <- subset[[response, "Test_Age"]]
  }
}

select(results, dpd1:age10) %>% write_csv("data/processed/alc_quantity_mplus.dat", col_names = F, na = "*")
