# Functions that fit OpenMx twin models and return an MxModel object

# mz_data and dz_data are dataframes containing the phenotype, with one
# row per twin pair. t1_var and t2_var are the column names for the
# phenotypes of the first and second twin, respectively.
fit_ace_univariate <- function (mz_data, dz_data, t1_var, t2_var) {
  require(OpenMx)
  
  mx_mz_data <- mxData(observed = data.frame(mz_data), type="raw")
  mx_dz_data <- mxData(observed = data.frame(dz_data), type="raw")
  
  phenos <- c(t1_var, t2_var)
  nv <- 1 # number of observed variables
  
  # Matrices ac, cc, and ec to store a, c, and e path coefficients
  # for latent phenotype(s)
  
  X <-
    mxMatrix(
      type = "Lower",
      nrow = nv,
      ncol = nv,
      free = TRUE,
      values = .6,
      labels = "x",
      name = "X"
    )
  Y <-
    mxMatrix(
      type = "Lower",
      nrow = nv,
      ncol = nv,
      free = TRUE,
      values = .6,
      labels = "y",
      name = "Y"
    )
  Z <-
    mxMatrix(
      type = "Lower",
      nrow = nv,
      ncol = nv,
      free = TRUE,
      values = .6,
      labels = "z",
      name = "Z"
    )
  
  A <- mxAlgebra(X %*% t(X), name="A")
  C <- mxAlgebra(Y %*% t(Y), name="C")
  E <- mxAlgebra(Z %*% t(Z), name="E")
  
  cov_P <- mxAlgebra(expression=A+C+E, name="V")
  
  a2 <- mxAlgebra(expression=A/V, name="a2")
  c2 <- mxAlgebra(expression=C/V, name="c2")
  e2 <- mxAlgebra(expression=E/V, name="e2")
  
  mat_I <- mxMatrix(type = "Iden", nrow = nv, ncol = nv, name = "I")
  inv_SD <- mxAlgebra(expression = solve(sqrt(I*V)), name = "iSD")
  mean_G <-
    mxMatrix(
      type = "Full",
      nrow = 1,
      ncol = nv * 2,
      free = TRUE,
      values = 1,
      labels = c("m1", "m1"),
      name = "expMean"
    )
  
  cov_MZ <-
    mxAlgebra(
      expression = rbind(cbind(A+C+E, A+C), cbind(A+C,  A+C+E)),
      name = "expCovMZ"
    )
  cov_DZ <-
    mxAlgebra(
      expression = rbind(cbind(A+C+E, 0.5 %x% A+C), cbind(0.5 %x% A+C, A+C+E)),
      name="expCovDZ")
  
  ### Combine Groups
  obj_MZ <-
    mxExpectationNormal(covariance = "expCovMZ",
                        means = "expMean",
                        dimnames = phenos)
  obj_DZ <-
    mxExpectationNormal(covariance = "expCovDZ",
                        means = "expMean",
                        dimnames = phenos)
  
  
  pars <- list(X, Y, Z, A, C, E, a2, c2, e2, cov_P, mat_I, inv_SD, mean_G)
  
  fun_ML <- mxFitFunctionML()
  
  model_MZ <- mxModel(pars, cov_MZ, mx_mz_data, obj_MZ, fun_ML, name = "MZ")
  model_DZ <- mxModel(pars, cov_DZ, mx_dz_data, obj_DZ, fun_ML, name = "DZ")
  
  fit_ML <- mxFitFunctionMultigroup(c("MZ.fitfunction","DZ.fitfunction"))
  Cf <- mxModel("ACE", pars, model_MZ, model_DZ, fit_ML, mxCI(c('a2', 'c2', 'e2')))
  
  mxRun(Cf, intervals=T) ### Run CholACE model
}
