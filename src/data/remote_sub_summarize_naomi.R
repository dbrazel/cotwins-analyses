# Generate a summary of the remote substance use results for Naomi

library(readr)
library(dplyr)
library(lubridate)

remote_sub_use <- read_rds("data/processed/remote_substance_use.rds")
paper_data <- read_rds("data/processed/Robin_paper-entry_2-22-17_cleaned.rds")
id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")

remote_sub_use %>%
  left_join(id_mapping_long, by = c("user_id" = "alternate_id")) %>%
  left_join(paper_data, by = c("SVID" = "ID1")) %>%
  group_by(user_id) %>%
  summarize(
    use_frac = sum(any_substance_use, na.rm = T) / n(),
    mean_drinks_per_day_when_drinking = mean(alc_quantity_drinks_per_day, na.rm = T),
    mar_use_frac = sum(mar_use, na.rm = T) / n(),
    alc_use_frac = sum(alc_use, na.rm = T) / n(),
    total_drugs_used =
      any(cig_use, ecig_use, other_tob_use, na.rm = T) +
      any(alc_use, na.rm = T) +
      any(mar_use, na.rm = T) +
      any(other_drug_use, na.rm = T),
    N = n(),
    age_last_completed = (as_date(max(date_completed)) - Birth_Date[1]) / 365) %>%
  write_csv("data/processed/remote_substance_use_summarized.csv")
