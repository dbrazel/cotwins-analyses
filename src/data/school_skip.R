# Look at whether kids' reported school skipping correlates with our intercept for at school

library(readr)
library(dplyr)

id_mapping_long <- read_csv("data/processed/id_mapping_long.csv", col_types = "ccc")
school_params <- read_rds("data/models/at_school_quadratic_parameters.rds") %>% as_tibble()
kidq <- read_rds("data/raw/Robin_kid-Q-1_12-19-16.rds") %>%
  haven::zap_formats() %>%
  haven::zap_labels()

kidq <- select(kidq, SVID = id, cut_school = KQ1Q0038) %>%
  mutate(cut_school  = recode(
    cut_school,
    `1` = 0,
    `2` = 1,
    `3` = 2,
    `4` = 3,
    `5` = 4.5,
    `6` = 8,
    `7` = 11,
    `999` = NaN
  ))

school_params <- left_join(school_params, id_mapping_long, by = c("user_id" = "alternate_id")) %>%
  left_join(kidq, by = "SVID")

cor.test(school_params$intercept, school_params$cut_school)
