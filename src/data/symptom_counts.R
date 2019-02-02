# Calculate rank correlations of the in-person and remote symptom counts

library(readr)
library(dplyr)

id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
intake_phenx <- read_rds("data/processed/PhenX_diagnoses.rds")
remote_phenx <- read_rds("data/processed/remote_PhenX_diagnoses.rds")

remote_phenx <- left_join(remote_phenx, id_mapping_long, by = c("user_id" = "alternate_id")) %>%
  arrange(date_completed) %>%
  group_by(SVID) %>%
  summarize(remote_alc_depend_count_1 = first(alc_depend_count),
            remote_alc_depend_count_2 = nth(alc_depend_count, 2),
            remote_alc_depend_count_3 = nth(alc_depend_count, 3),
            remote_alc_depend_count_4 = nth(alc_depend_count, 4),
            remote_mar_depend_count_1 = first(mar_depend_count),
            remote_mar_depend_count_2 = nth(mar_depend_count, 2),
            remote_mar_depend_count_3 = nth(mar_depend_count, 3),
            remote_mar_depend_count_4 = nth(mar_depend_count, 4),
            remote_tob_depend_count_1 = first(tob_depend_count),
            remote_tob_depend_count_2 = nth(tob_depend_count, 2),
            remote_tob_depend_count_3 = nth(tob_depend_count, 3),
            remote_tob_depend_count_4 = nth(tob_depend_count, 4)
            )

phenx <- left_join(intake_phenx, remote_phenx, by = "SVID") %>%
  select(
    year_alc_depend_count,
    year_mar_depend_count,
    year_tob_depend_count, 
    remote_alc_depend_count_1:remote_tob_depend_count_4
    )

results <- tibble(
  t = rep(as.character(c(1L, 2L, 3L, 4L, 2L, 3L, 4L)), 3),
  from = rep(c("Intake", "Intake", "Intake", "Intake", "Remote", "Remote", "Remote"), 3),
  corr = rep(NA, 21),
  substance = c(rep("Alcohol", 7), rep("Marijuana", 7), rep("Tobacco", 7))
  )

for (i in seq(1, 3)) {
  results[1 + (i - 1) * 7, 3] <- cor(phenx[, i], phenx[, 4 + (i -1) * 4], use = "complete.obs", method = "spearman")
  results[2 + (i - 1) * 7, 3] <- cor(phenx[, i], phenx[, 5 + (i -1) * 4], use = "complete.obs", method = "spearman")
  results[3 + (i - 1) * 7, 3] <- cor(phenx[, i], phenx[, 6 + (i -1) * 4], use = "complete.obs", method = "spearman")
  results[4 + (i - 1) * 7, 3] <- cor(phenx[, i], phenx[, 7 + (i -1) * 4], use = "complete.obs", method = "spearman")
  results[5 + (i - 1) * 7, 3] <- cor(phenx[, 4 + (i -1) * 4], phenx[, 5 + (i -1) * 4], use = "complete.obs", method = "spearman")
  results[6 + (i - 1) * 7, 3] <- cor(phenx[, 4 + (i -1) * 4], phenx[, 6 + (i -1) * 4], use = "complete.obs", method = "spearman")
  results[7 + (i - 1) * 7, 3] <- cor(phenx[, 4 + (i -1) * 4], phenx[, 7 + (i -1) * 4], use = "complete.obs", method = "spearman")
}
