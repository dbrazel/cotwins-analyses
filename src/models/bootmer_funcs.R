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
    verbose = T,
    use.u = T,
    type = "semiparametric",
    ncpus = 3,
    parallel = "multicore"
  )
}

# Given two bootstrapped lme4 quadratic growth models, calculate the
# correlations between their growth parameters
get_quadratic_corrs <- function(ml1, ml2) {
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
    results$ml1_int_ml2_int[i] <- cor(as.numeric(ml1_ints[i, ]), as.numeric(ml2_ints[i, ]))
    results$ml1_int_ml2_slope[i] <- cor(as.numeric(ml1_ints[i, ]), as.numeric(ml2_slopes[i, ]))
    results$ml1_int_ml2_quad[i] <- cor(as.numeric(ml1_ints[i, ]), as.numeric(ml2_quads[i, ]))
    
    results$ml1_slope_ml2_int[i] <- cor(as.numeric(ml1_slopes[i, ]), as.numeric(ml2_ints[i, ]))
    results$ml1_slope_ml2_slope[i] <- cor(as.numeric(ml1_slopes[i, ]), as.numeric(ml2_slopes[i, ]))
    results$ml1_slope_ml2_quad[i] <- cor(as.numeric(ml1_slopes[i, ]), as.numeric(ml2_quads[i, ]))
    
    results$ml1_quad_ml2_int[i] <- cor(as.numeric(ml1_quads[i, ]), as.numeric(ml2_ints[i, ]))
    results$ml1_quad_ml2_slope[i] <- cor(as.numeric(ml1_quads[i, ]), as.numeric(ml2_slopes[i, ]))
    results$ml1_quad_ml2_quad[i] <- cor(as.numeric(ml1_quads[i, ]), as.numeric(ml2_quads[i, ]))
  }
  
  return(results)
}
