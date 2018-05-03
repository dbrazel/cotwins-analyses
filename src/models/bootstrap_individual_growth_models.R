# Bootstrap individual quadratic growth models

library(readr)
library(lme4)
library(dplyr)
library(boot)
library(broom)

source("src/models/bootmer_funcs.R")

num_cpus <- 4

# Time at home
at_home_data <- read_rds("data/models/at_home_data.rds")
at_home_formula <- formula(home_frac ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
at_home_families <- unique(at_home_data$family)

at_home_boot <-
  boot(
    at_home_families,
    get_vcov_quadratic_nonpara,
    R = 1000,
    lme_formula = at_home_formula,
    pheno_data = at_home_data,
    parallel = "snow",
    ncpus = num_cpus
  )

at_home_cis <- tidy(at_home_boot, conf.int = T) %>% mutate(pheno = "Home")

write_rds(at_home_boot, "data/models/at_home_quadratic_model_boot.rds")
write_rds(at_home_cis, "data/models/at_home_quadratic_model_cis.rds")

# Time at school
at_school_data <- read_rds("data/models/at_school_data.rds")
at_school_formula <- formula(school_frac ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
at_school_families <- unique(at_school_data$family)

at_school_boot <-
  boot(
    at_school_families,
    get_vcov_quadratic_nonpara,
    R = 1000,
    lme_formula = at_school_formula,
    pheno_data = at_school_data,
    parallel = "snow",
    ncpus = num_cpus
  )

at_school_cis <- tidy(at_school_boot, conf.int = T) %>% mutate(pheno = "School")

write_rds(at_school_boot, "data/models/at_school_quadratic_model_boot.rds")
write_rds(at_school_cis, "data/models/at_school_quadratic_model_cis.rds")

# Parental monitoring
par_mon_data <- read_rds("data/models/par_mon_data.rds")
par_mon_formula <- formula(max_monitor_score ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
par_mon_families <- unique(par_mon_data$family)

par_mon_boot <-
  boot(
    par_mon_families,
    get_vcov_quadratic_nonpara,
    R = 1000,
    lme_formula = par_mon_formula,
    pheno_data = par_mon_data,
    parallel = "snow",
    ncpus = num_cpus
  )

par_mon_cis <- tidy(par_mon_boot, conf.int = T) %>% mutate(pheno = "Parents")

write_rds(par_mon_boot, "data/models/par_mon_quadratic_model_boot.rds")
write_rds(par_mon_cis, "data/models/par_mon_quadratic_model_cis.rds")

# DPW
dpw_data <- read_rds("data/models/dpw_data.rds")
dpw_formula <- formula(drinks_per_week ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
dpw_families <- unique(dpw_data$family)

dpw_boot <-
  boot(
    dpw_families,
    get_vcov_quadratic_nonpara,
    R = 1000,
    lme_formula = dpw_formula,
    pheno_data = dpw_data,
    parallel = "snow",
    ncpus = num_cpus
  )

dpw_cis <- tidy(dpw_boot, conf.int = T) %>% mutate(pheno = "DPW")

write_rds(dpw_boot, "data/models/dpw_quadratic_model_boot.rds")
write_rds(dpw_cis, "data/models/dpw_quadratic_model_cis.rds")

# MPW
mpw_data <- read_rds("data/models/mpw_data.rds")
mpw_formula <- formula(mar_per_week ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
mpw_families <- unique(mpw_data$family)

mpw_boot <-
  boot(
    mpw_families,
    get_vcov_quadratic_nonpara,
    R = 1000,
    lme_formula = mpw_formula,
    pheno_data = mpw_data,
    parallel = "snow",
    ncpus = num_cpus
  )

mpw_cis <- tidy(mpw_boot, conf.int = T) %>% mutate(pheno = "MPW")

write_rds(mpw_boot, "data/models/mpw_quadratic_model_boot.rds")
write_rds(mpw_cis, "data/models/mpw_quadratic_model_cis.rds")

# ecig
ecig_data <- read_rds("data/models/ecig_data.rds")
ecig_formula <- formula(puffs_per_week ~ (test_age + I(test_age^2) + sex) + (test_age + I(test_age^2) | family/user_id))
ecig_families <- unique(ecig_data$family)

ecig_boot <-
  boot(
    ecig_families,
    get_vcov_quadratic_nonpara,
    R = 1000,
    lme_formula = ecig_formula,
    pheno_data = ecig_data,
    parallel = "snow",
    ncpus = num_cpus
  )

ecig_cis <- tidy(ecig_boot, conf.int = T) %>% mutate(pheno = "PPW")

write_rds(ecig_boot, "data/models/ecig_quadratic_model_boot.rds")
write_rds(ecig_cis, "data/models/ecig_quadratic_model_cis.rds")

all_cis <- bind_rows(at_home_cis, at_school_cis, par_mon_cis, dpw_cis, mpw_cis, ecig_cis)
write_rds(all_cis, "data/models/all_phenos_cis.rds")
