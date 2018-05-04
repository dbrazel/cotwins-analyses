# Given a fitted lme4 quadratic growth model, get a named vector of the intercepts
# slopes, and quadratic effects
get_growth_params_quadratic <- function(ml) {
  require(dplyr)
  require(lme4)
  require(tidyr)
  
  rand_effs <- ranef(ml)
  fix_effs <- fixef(ml)
  rand_effs_family <- rand_effs$family %>% tibble::rownames_to_column("family")
  rand_effs_twin <- rand_effs$`user_id:family` %>%
    tibble::rownames_to_column("both") %>%
    separate(both, c("user_id", "family"), ":")
  
  parameters <- left_join(rand_effs_twin, rand_effs_family, by = "family") %>%
    transmute(
      user_id = user_id,
      intercept = `(Intercept).x` + `(Intercept).y` + fix_effs["(Intercept)"],
      slope = `test_age.x` + `test_age.y` + fix_effs["test_age"],
      quadratic = `I(test_age^2).x` + `I(test_age^2).y` + fix_effs["I(test_age^2)"]
    )
  
  coefs <- c(parameters$intercept, parameters$slope, parameters$quadratic)
  names(coefs) <-
    c(
      paste0(parameters$user_id, "_intercept"),
      paste0(parameters$user_id, "_slope"),
      paste0(parameters$user_id, "_quadratic")
    )
  return(coefs)
}

# Semiparametric bootstrap of a lme4 quadratic growth model
boot_quadratic <- function(ml) {
  require(lme4)
  bootMer(
    ml,
    get_growth_params_quadratic,
    nsim = 1000,
    use.u = T,
    type = "semiparametric",
    ncpus = 3,
    parallel = "multicore"
  )
}

# Given two bootstrapped lme4 quadratic growth models, calculate the
# correlations between their growth parameters
get_quadratic_cors <- function(ml1, ml2) {
  require(lme4)
  require(dplyr)
  
  ml1_t <- as_tibble(ml1$t)
  ml2_t <- as_tibble(ml2$t)
  
  ml1_ints <- select(ml1_t, contains("intercept"))
  ml1_slopes <- select(ml1_t, contains("slope"))
  ml1_quads <- select(ml1_t, contains("quadratic"))
  
  ml2_ints <- select(ml2_t, contains("intercept"))
  ml2_slopes <- select(ml2_t, contains("slope"))
  ml2_quads <- select(ml2_t, contains("quadratic"))
  
  nsim <- nrow(ml1_t)
  results <- tibble(
    ml1_int_ml2_int = rep(NA, nsim),
    ml1_int_ml2_slope = rep(NA, nsim),
    ml1_int_ml2_quad = rep(NA, nsim),
    ml1_slope_ml2_int = rep(NA, nsim),
    ml1_slope_ml2_slope = rep(NA, nsim),
    ml1_slope_ml2_quad = rep(NA, nsim),
    ml1_quad_ml2_int = rep(NA, nsim),
    ml1_quad_ml2_slope = rep(NA, nsim),
    ml1_quad_ml2_quad = rep(NA, nsim)
  )
  
  for (i in 1:nsim) {
    results$ml1_int_ml2_int[i] <-
      cor(as.numeric(ml1_ints[i,]), as.numeric(ml2_ints[i,]))
    results$ml1_int_ml2_slope[i] <-
      cor(as.numeric(ml1_ints[i,]), as.numeric(ml2_slopes[i,]))
    results$ml1_int_ml2_quad[i] <-
      cor(as.numeric(ml1_ints[i,]), as.numeric(ml2_quads[i,]))
    
    results$ml1_slope_ml2_int[i] <-
      cor(as.numeric(ml1_slopes[i,]), as.numeric(ml2_ints[i,]))
    results$ml1_slope_ml2_slope[i] <-
      cor(as.numeric(ml1_slopes[i,]), as.numeric(ml2_slopes[i,]))
    results$ml1_slope_ml2_quad[i] <-
      cor(as.numeric(ml1_slopes[i,]), as.numeric(ml2_quads[i,]))
    
    results$ml1_quad_ml2_int[i] <-
      cor(as.numeric(ml1_quads[i,]), as.numeric(ml2_ints[i,]))
    results$ml1_quad_ml2_slope[i] <-
      cor(as.numeric(ml1_quads[i,]), as.numeric(ml2_slopes[i,]))
    results$ml1_quad_ml2_quad[i] <-
      cor(as.numeric(ml1_quads[i,]), as.numeric(ml2_quads[i,]))
  }
  
  return(results)
}

# Given two bootstrapped lme4 quadratic growth models and the output
# of get_quadratic_cors, get the point estimates and 95% CIs
get_quad_cis <- function(ml1, ml2, cors) {
  i <- length(ml1$t0) / 3
  
  ml1_ints <- ml1$t0[1:i]
  ml1_slopes <- ml1$t0[(i+1):(2*i)]
  ml1_quads <- ml1$t0[(2*i+1):(3*i)]
  
  ml2_ints <- ml2$t0[1:i]
  ml2_slopes <- ml2$t0[(i+1):(2*i)]
  ml2_quads <- ml2$t0[(2*i+1):(3*i)]
  
  estimates = c(
    cor(ml1_ints, ml2_ints),
    cor(ml1_ints, ml2_slopes),
    cor(ml1_ints, ml2_quads),
    cor(ml1_slopes, ml2_ints),
    cor(ml1_slopes, ml2_slopes),
    cor(ml1_slopes, ml2_quads),
    cor(ml1_quads, ml2_ints),
    cor(ml1_quads, ml2_slopes),
    cor(ml1_quads, ml2_quads)
  )
  
  tibble(
    lbound = apply(cors, 2, quantile, 0.025),
    estimate = estimates,
    ubound = apply(cors, 2, quantile, 0.975),
    name = colnames(cors)
  )
}

# Given a fitted lme4 quadratic growth model, extract a named vector
# of variance covariance parameters
get_vcov_quadratic <- function(ml) {
  require(lme4)
  require(dplyr)
  
  vcor <- VarCorr(ml) %>% as.data.frame()
  
  # Assert that the vcov matrix has the expected order and entries
  stopifnot(all(
    vcor$grp == c(
      "user_id:family",
      "user_id:family",
      "user_id:family",
      "user_id:family",
      "user_id:family",
      "user_id:family",
      "family",
      "family",
      "family",
      "family",
      "family",
      "family",
      "Residual"
    )
  ),
  all(
    vcor$var1 == c(
      "(Intercept)",
      "test_age",
      "I(test_age^2)",
      "(Intercept)",
      "(Intercept)",
      "test_age",
      "(Intercept)",
      "test_age",
      "I(test_age^2)",
      "(Intercept)",
      "(Intercept)",
      "test_age",
      NA
    ),
    na.rm = T
  ),
  all(
    vcor$var2 == c(
      NA,
      NA,
      NA,
      "test_age",
      "I(test_age^2)",
      "I(test_age^2)",
      NA,
      NA,
      NA,
      "test_age",
      "I(test_age^2)",
      "I(test_age^2)",
      NA
    ),
    na.rm = T
  ))
  
  vcor_standardized <- vcor$sdcor
  names(vcor_standardized) <- c(
    "twin_std_dev_intercept",
    "twin_std_dev_slope",
    "twin_std_dev_quadratic",
    "twin_cor_intercept_slope",
    "twin_cor_intercept_quadratic",
    "twin_cor_slope_quadratic",
    "family_std_dev_intercept",
    "family_std_dev_slope",
    "family_std_dev_quadratic",
    "family_cor_intercept_slope",
    "family_cor_intercept_quadratic",
    "family_cor_slope_quadratic",
    "residual"
    )
  
  return(vcor_standardized)
}

# Semiparametric bootstrap of an lme4 quadratic growth model
boot_vcov_quadratic <- function(ml) {
  require(lme4)
  bootMer(
    ml,
    get_vcov_quadratic,
    nsim = 1000,
    use.u = T,
    type = "semiparametric",
    ncpus = 3,
    parallel = "snow"
  )
}

# Given a bootstrapped lme4 quadratic growth model, get CIs
get_single_quad_cis <- function(ml_boot, pheno_name) {
  require(dplyr)
  
  param_names <- names(ml_boot$t0)
  out <- tibble(
    pheno = rep(pheno_name, 13),
    param = param_names,
    estimate = rep(NA, 13),
    lower = rep(NA, 13),
    upper = rep(NA, 13)
  )
  
  for (i in 1:13) {
    ci <- boot::boot.ci(ml_boot, type = "perc", index = i)
    out[i, "estimate"] = ci$t0
    out[i, "lower"] = ci$percent[4]
    out[i, "upper"] = ci$percent[5]
  }
  
  return(out)
}

# Given selected twin pairs (from the boot function) fit an lme4 
# model and return a named vector of variance covariance parameters.
# Meant to work with nonparametric bootstrapping
#
# tp_ids - a vector of possible twin pair IDs
# idx - a vector of indices in tp_ids to be included
# lme_formula - the lmer formula to be used in the model fit
# pheno_data - a dataframe containing the data for the model fit
get_vcov_quadratic_nonpara <- function(tp_ids, idx, lme_formula, pheno_data) {
  require(lme4)
  require(dplyr)
  
  tp_ids <- tp_ids[idx]
  pheno_data <- filter(pheno_data, family %in% tp_ids)
  
  ml <- lmer(formula = lme_formula, data = pheno_data)
  
  # Inline get_vcov_quadratic because snow workers don't inherit contexts
  vcor <- VarCorr(ml) %>% as.data.frame()
  
  # Assert that the vcov matrix has the expected order and entries
  stopifnot(all(
    vcor$grp == c(
      "user_id:family",
      "user_id:family",
      "user_id:family",
      "user_id:family",
      "user_id:family",
      "user_id:family",
      "family",
      "family",
      "family",
      "family",
      "family",
      "family",
      "Residual"
    )
  ),
  all(
    vcor$var1 == c(
      "(Intercept)",
      "test_age",
      "I(test_age^2)",
      "(Intercept)",
      "(Intercept)",
      "test_age",
      "(Intercept)",
      "test_age",
      "I(test_age^2)",
      "(Intercept)",
      "(Intercept)",
      "test_age",
      NA
    ),
    na.rm = T
  ),
  all(
    vcor$var2 == c(
      NA,
      NA,
      NA,
      "test_age",
      "I(test_age^2)",
      "I(test_age^2)",
      NA,
      NA,
      NA,
      "test_age",
      "I(test_age^2)",
      "I(test_age^2)",
      NA
    ),
    na.rm = T
  ))
  
  vcor_standardized <- vcor$sdcor
  names(vcor_standardized) <- c(
    "twin_std_dev_intercept",
    "twin_std_dev_slope",
    "twin_std_dev_quadratic",
    "twin_cor_intercept_slope",
    "twin_cor_intercept_quadratic",
    "twin_cor_slope_quadratic",
    "family_std_dev_intercept",
    "family_std_dev_slope",
    "family_std_dev_quadratic",
    "family_cor_intercept_slope",
    "family_cor_intercept_quadratic",
    "family_cor_slope_quadratic",
    "residual"
  )
  
  return(vcor_standardized)
}

# Given selected twin pairs (from the boot function) fit lme4 models to two
# different phenotypes and return a vector of growth parameter correlations
#
# tp_ids - a vector of possible twin pair IDs
# idx - a vector of indices in tp_ids to be included
# lme_formula1 - the lmer formula for the first pheno
# lme_formula2 - the lmer formula for the second pheno
# pheno_data1 - a data frame for the first pheno
# pheno_data2 - a data frame for the second pheno
get_growth_params_cor_nonpara <- function(tp_ids, idx, lme_formula1, lme_formula2, pheno_data1, pheno_data2) {
  require(lme4)
  require(dplyr)
  
  tp_ids <- tp_ids[idx]
  pheno_data1 <- filter(pheno_data1, family %in% tp_ids)
  pheno_data2 <- filter(pheno_data2, family %in% tp_ids)
  
  # Make sure both dfs have the same twins in the same order
  pheno_data1 <- filter(user_id %in% pheno_data2$user_id) %>%
    arrange(user_id)
  pheno_data2 <- filter(user_id %in% pheno_data1$user_id) %>%
    arrange(user_id)
  
  ml1 <- lmer(formula = lme_formula1, data = pheno_data1)
  ml2 <- lmer(formula = lme_formula2, data = pheno_data2)
  
  ml1_params <- get_growth_params_quadratic(ml1)
  ml2_params <- get_growth_params_quadratic(ml2)
  
  # Make sure the vectors have the same order
  stopifnot(names(ml1_params == ml2_params))
  
  ml1_ints <- ml1_params[1:(length(ml1_params)/3)]
  ml1_slopes <- ml1_params[((length(ml1_params)/3)+1):(length(ml1_params)/3*2)]
  ml1_quads <- ml1_params[((length(ml1_params)/3*2)+1):(length(ml1_params))]
  
  ml2_ints <- ml2_params[1:(length(ml2_params)/3)]
  ml2_slopes <- ml2_params[((length(ml2_params)/3)+1):(length(ml2_params)/3*2)]
  ml2_quads <- ml2_params[((length(ml2_params)/3*2)+1):(length(ml2_params))]
}
