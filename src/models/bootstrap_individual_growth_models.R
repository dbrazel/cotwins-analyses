# Bootstrap individual quadratic growth models

library(readr)
library(lme4)

source("src/models/bootmer_funcs.R")

at_home_ml <- read_rds("data/models/at_home_quadratic_model.rds")
dpw_ml <- read_rds("data/models/dpw_quadratic_model.rds")
mpw_ml <- read_rds("data/models/mpw_quadratic_model.rds")
ecig_ml <- read_rds("data/models/ecig_quadratic_model.rds")

at_home_boot <- boot_vcov_quadratic(at_home_ml)
dpw_boot <- boot_vcov_quadratic(dpw_ml)
mpw_boot <- boot_vcov_quadratic(mpw_ml)
ecig_boot <- boot_vcov_quadratic(ecig_ml)

write_rds(at_home_boot, "data/models/at_home_quadratic_model_boot.rds")
write_rds(dpw_boot, "data/models/dpw_quadratic_model_boot.rds")
write_rds(mpw_boot, "data/models/mpw_quadratic_model_boot.rds")
write_rds(ecig_boot, "data/models/ecig_quadratic_model_boot.rds")
