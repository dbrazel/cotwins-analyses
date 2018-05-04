# Bootstrap the correlations between growth parameter estimates across phenotypes

library(readr)
library(dplyr)
library(lme4)
library(boot)

source("src/models/bootmer_funcs.R")

num_cpus <- 4

at_home_data <- read_rds("data/models/at_home_data.rds")
at_home_formula <- formula(home_frac ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
at_home_families <- unique(at_home_data$family)

at_school_data <- read_rds("data/models/at_school_data.rds")
at_school_formula <- formula(school_frac ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
at_school_families <- unique(at_school_data$family)

par_mon_data <- read_rds("data/models/par_mon_data.rds")
par_mon_formula <- formula(max_monitor_score ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
par_mon_families <- unique(par_mon_data$family)

dpw_data <- read_rds("data/models/dpw_data.rds")
dpw_formula <- formula(drinks_per_week ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
dpw_families <- unique(dpw_data$family)

mpw_data <- read_rds("data/models/mpw_data.rds")
mpw_formula <- formula(mar_per_week ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
mpw_families <- unique(mpw_data$family)

ecig_data <- read_rds("data/models/ecig_data.rds")
ecig_formula <- formula(puffs_per_week ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
ecig_families <- unique(ecig_data$family)
