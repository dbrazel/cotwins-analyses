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

# The substance use phenotypes all share the same twin pairs. The other phenotypes have distinct sets from each
# other and from the substance use phenotypes.

# First, the substance use pairs
dpw_mpw_boot <-
  boot(
    dpw_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = dpw_formula,
    lme_formula2 = mpw_formula,
    pheno_data1 = dpw_data,
    pheno_data2 = mpw_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(dpw_mpw_boot, "data/models/dpw_mpw_boot.rds")

dpw_ecig_boot <-
  boot(
    dpw_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = dpw_formula,
    lme_formula2 = ecig_formula,
    pheno_data1 = dpw_data,
    pheno_data2 = ecig_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(dpw_ecig_boot, "data/models/dpw_ecig_boot.rds")

mpw_ecig_boot <-
  boot(
    mpw_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = mpw_formula,
    lme_formula2 = ecig_formula,
    pheno_data1 = mpw_data,
    pheno_data2 = ecig_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(mpw_ecig_boot, "data/models/mpw_ecig_boot.rds")

# Now from at home to substance use. We need the intersection of twin pairs
at_home_sub_use_families <- intersect(at_home_families, dpw_families)

at_home_dpw_boot <-
  boot(
    at_home_sub_use_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = at_home_formula,
    lme_formula2 = dpw_formula,
    pheno_data1 = at_home_data,
    pheno_data2 = dpw_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(at_home_dpw_boot, "data/models/at_home_dpw_boot.rds")

at_home_mpw_boot <-
  boot(
    at_home_sub_use_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = at_home_formula,
    lme_formula2 = mpw_formula,
    pheno_data1 = at_home_data,
    pheno_data2 = mpw_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(at_home_mpw_boot, "data/models/at_home_mpw_boot.rds")

at_home_ecig_boot <-
  boot(
    at_home_sub_use_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = at_home_formula,
    lme_formula2 = ecig_formula,
    pheno_data1 = at_home_data,
    pheno_data2 = ecig_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(at_home_ecig_boot, "data/models/at_home_ecig_boot.rds")

# at school to sub use
at_school_sub_use_families <- intersect(at_school_families, dpw_families)

at_school_dpw_boot <-
  boot(
    at_school_sub_use_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = at_school_formula,
    lme_formula2 = dpw_formula,
    pheno_data1 = at_school_data,
    pheno_data2 = dpw_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(at_school_dpw_boot, "data/models/at_school_dpw_boot.rds")

at_school_mpw_boot <-
  boot(
    at_school_sub_use_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = at_school_formula,
    lme_formula2 = mpw_formula,
    pheno_data1 = at_school_data,
    pheno_data2 = mpw_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(at_school_mpw_boot, "data/models/at_school_mpw_boot.rds")

at_school_ecig_boot <-
  boot(
    at_school_sub_use_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = at_school_formula,
    lme_formula2 = ecig_formula,
    pheno_data1 = at_school_data,
    pheno_data2 = ecig_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(at_school_ecig_boot, "data/models/at_school_ecig_boot.rds")

# parental monitoring to sub use
par_mon_sub_use_families <- intersect(par_mon_families, dpw_families)

par_mon_dpw_boot <-
  boot(
    par_mon_sub_use_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = par_mon_formula,
    lme_formula2 = dpw_formula,
    pheno_data1 = par_mon_data,
    pheno_data2 = dpw_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(par_mon_dpw_boot, "data/models/par_mon_dpw_boot.rds")

par_mon_mpw_boot <-
  boot(
    par_mon_sub_use_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = par_mon_formula,
    lme_formula2 = mpw_formula,
    pheno_data1 = par_mon_data,
    pheno_data2 = mpw_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(par_mon_mpw_boot, "data/models/par_mon_mpw_boot.rds")

par_mon_ecig_boot <-
  boot(
    par_mon_sub_use_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = par_mon_formula,
    lme_formula2 = ecig_formula,
    pheno_data1 = par_mon_data,
    pheno_data2 = ecig_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(par_mon_ecig_boot, "data/models/par_mon_ecig_boot.rds")

# Remaining pairs
at_home_at_school_families <- intersect(at_home_families, at_school_families)
at_home_dpw_boot <-
  boot(
    at_home_at_school_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = at_home_formula,
    lme_formula2 = at_school_formula,
    pheno_data1 = at_home_data,
    pheno_data2 = at_school_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(at_home_at_school_boot, "data/models/at_home_at_school_boot.rds")

at_home_par_mon_families <- intersect(at_home_families, par_mon_families)
at_home_par_mon_boot <-
  boot(
    at_home_par_mon_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = at_home_formula,
    lme_formula2 = par_mon_formula,
    pheno_data1 = at_home_data,
    pheno_data2 = par_mon_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(at_home_par_mon_boot, "data/models/at_home_par_mon_boot.rds")

at_school_par_mon_families <- intersect(at_school_families, par_mon_families)
at_school_par_mon_boot <-
  boot(
    at_school_par_mon_families,
    get_growth_params_cor_nonpara,
    R = 1000,
    lme_formula1 = at_school_formula,
    lme_formula2 = par_mon_formula,
    pheno_data1 = at_school_data,
    pheno_data2 = par_mon_data,
    parallel = "snow",
    ncpus = num_cpus
  )
write_rds(at_school_par_mon_boot, "data/models/at_school_par_mon_boot.rds")
