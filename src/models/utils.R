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
      values = .25,
      labels = "x",
      name = "X"
    )
  Y <-
    mxMatrix(
      type = "Lower",
      nrow = nv,
      ncol = nv,
      free = TRUE,
      values = .35,
      labels = "y",
      name = "Y"
    )
  Z <-
    mxMatrix(
      type = "Lower",
      nrow = nv,
      ncol = nv,
      free = TRUE,
      values = .45,
      labels = "z",
      name = "Z"
    )
  
  A <- mxAlgebra(X %*% t(X), name = "A")
  C <- mxAlgebra(Y %*% t(Y), name = "C")
  E <- mxAlgebra(Z %*% t(Z), name = "E")
  
  cov_P <- mxAlgebra(expression = A + C + E, name = "V")
  
  a2 <- mxAlgebra(expression = A / V, name = "a2")
  c2 <- mxAlgebra(expression = C / V, name = "c2")
  e2 <- mxAlgebra(expression = E / V, name = "e2")
  
  mat_I <- mxMatrix(type = "Iden", nrow = nv, ncol = nv, name = "I")
  inv_SD <- mxAlgebra(expression = solve(sqrt(I * V)), name = "iSD")
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
  
  cov_mz <-
    mxAlgebra(
      expression = rbind(cbind(A + C + E, A + C), cbind(A + C,  A + C + E)),
      name = "expCovMZ"
    )
  cov_dz <-
    mxAlgebra(
      expression =
        rbind(cbind(A + C + E, 0.5 %x% A + C), cbind(0.5 %x% A + C, A + C + E)),
      name="expCovDZ")
  
  ### Combine Groups
  obj_mz <-
    mxExpectationNormal(covariance = "expCovMZ",
                        means = "expMean",
                        dimnames = phenos)
  obj_dz <-
    mxExpectationNormal(covariance = "expCovDZ",
                        means = "expMean",
                        dimnames = phenos)
  
  
  pars <- list(X, Y, Z, A, C, E, a2, c2, e2, cov_P, mat_I, inv_SD, mean_G)
  
  fun_ML <- mxFitFunctionML()
  
  model_mz <- mxModel(pars, cov_mz, mx_mz_data, obj_mz, fun_ML, name = "MZ")
  model_dz <- mxModel(pars, cov_dz, mx_dz_data, obj_dz, fun_ML, name = "DZ")
  
  fit_ML <- mxFitFunctionMultigroup(c("MZ.fitfunction","DZ.fitfunction"))
  Cf <-
    mxModel("ACE", pars, model_mz, model_dz, fit_ML, mxCI(c('a2', 'c2', 'e2')))
  
  mxRun(Cf, intervals = T) ### Run CholACE model
}

# mz_data and dz_data are dataframes containing the phenotype, with one
# row per twin pair. t1_var1 and t2_var1 are the column names for the
# first phenotype for the first and second twin and t1_var2 and t2_var2
# for the second phenotype
fit_ace_bivariate <-
  function (mz_data,
            dz_data,
            t1_var1,
            t2_var1,
            t1_var2,
            t2_var2) {
    require(OpenMx)
    
    mx_mz_data <- mxData(observed = data.frame(mz_data), type="raw")
    mx_dz_data <- mxData(observed = data.frame(dz_data), type="raw")
    
    nv <- 2 # Number of observed variables
    phenos <- c(t1_var1, t1_var2, t2_var1, t2_var2)
    
    X <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("x11", "x21", "x22"),
        name = "X"
      )
    Y <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("y11", "y21", "y22"),
        name = "Y"
      )
    Z <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("z11", "z21", "z22"),
        name = "Z"
      )
    
    A <- mxAlgebra(X %*% t(X), name = "A")
    C <- mxAlgebra(Y %*% t(Y), name = "C")
    E <- mxAlgebra(Z %*% t(Z), name = "E")
    
    cov_P <- mxAlgebra(expression = A + C + E, name = "V")
    
    a1_2 <- mxAlgebra(expression = A[1, 1] / V[1, 1], name = "a1_2")
    c1_2 <- mxAlgebra(expression = C[1, 1] / V[1, 1], name = "c1_2")
    e1_2 <- mxAlgebra(expression = E[1, 1] / V[1, 1], name = "e1_2")
    
    a2_2 <- mxAlgebra(expression = A[2, 2] / V[2, 2], name = "a2_2")
    c2_2 <- mxAlgebra(expression = C[2, 2] / V[2, 2], name = "c2_2")
    e2_2 <- mxAlgebra(expression = E[2, 2] / V[2, 2], name = "e2_2")
    
    mat_I <- mxMatrix(
      type = "Iden",
      nrow = nv,
      ncol = nv,
      name = "I"
    )
    inv_SD <- mxAlgebra(expression = solve(sqrt(I * V)), name = "iSD")
    
    mean_G <-
      mxMatrix(
        type = "Full",
        nrow = 1,
        ncol = nv * 2,
        free = TRUE,
        values = 1,
        labels = c("m1", "m1", "m2", "m2"),
        name = "expMean"
      )
    
    cov_mz <-
      mxAlgebra(
        expression = rbind(cbind(A + C + E, A + C), cbind(A + C,  A + C + E)),
        name = "expCovMZ"
        )
    cov_dz <-
      mxAlgebra(
        expression = rbind(
          cbind(A + C + E, 0.5 %x% A + C),
          cbind(0.5 %x% A + C, A + C + E)
          ),
        name = "expCovDZ"
        )
    
    obj_mz <-
      mxExpectationNormal(covariance = "expCovMZ",
                          means = "expMean",
                          dimnames = phenos)
    obj_dz <-
      mxExpectationNormal(covariance = "expCovDZ",
                          means = "expMean",
                          dimnames = phenos)
    
    # Calculate genetic and environmental correlations
    corA <-
      mxAlgebra(expression = solve(sqrt(I * A)) %&% A, name = "rA")
    corC <-
      mxAlgebra(expression = solve(sqrt(I * C)) %&% C, name = "rC")
    corE <-
      mxAlgebra(expression = solve(sqrt(I * E)) %&% E, name = "rE")
    
    pars <-
      list(X,
           Y,
           Z,
           A,
           C,
           E,
           a1_2,
           c1_2,
           e1_2,
           a2_2,
           c2_2,
           e2_2,
           cov_P,
           mat_I,
           inv_SD,
           mean_G,
           corA,
           corC,
           corE)
    
    fun_ML <- mxFitFunctionML()
    
    model_mz <- mxModel(pars, cov_mz, mx_mz_data, obj_mz, fun_ML, name = "MZ")
    model_dz <- mxModel(pars, cov_dz, mx_dz_data, obj_dz, fun_ML, name = "DZ")
    
    # Create algebra for variance components
    col_VC <- c("var1", "var2")
    row_VC <- rep(c('A', 'C', 'E', 'SA', 'SC', 'SE'), each = nv)
    est_VC <-
      mxAlgebra(
        expression = rbind(A, C, E, A / V, C / V, E / V),
        name = "VC",
        dimnames = list(row_VC, col_VC)
      )
    
    fit_ML <-
      mxFitFunctionMultigroup(c("MZ.fitfunction", "DZ.fitfunction"))
    Cf <-
      mxModel(
        "ACE",
        pars,
        model_mz,
        model_dz,
        fit_ML,
        est_VC,
        mxCI(c("a1_2", "c1_2", "e1_2", "a2_2", "c2_2", "e2_2")
             )
        )
    # Run the CholACE model
    mxRun(Cf, intervals = T)
  }

# mz_data and dz_data are dataframes containing the phenotype, with one
# row per twin pair. t1_var1 and t2_var1 are the column names for the
# first phenotype for the first and second twin, t1_var2 and t2_var2
# for the second phenotype, and t1_var3 and t2_var3 for the third
fit_ace_trivariate <-
  function (mz_data,
            dz_data,
            t1_var1,
            t2_var1,
            t1_var2,
            t2_var2,
            t1_var3,
            t2_var3) {
    require(OpenMx)
    
    mx_mz_data <- mxData(observed = data.frame(mz_data), type="raw")
    mx_dz_data <- mxData(observed = data.frame(dz_data), type="raw")
    
    nv <- 3 # Number of observed variables
    phenos <- c(t1_var1, t1_var2, t1_var3, t2_var1, t2_var2, t2_var3)
    
    X <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("x11", "x21", "x31", "x22", "x32", "x33"),
        name = "X"
      )
    Y <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("y11", "y21", "y31", "y22", "y32", "y33"),
        name = "Y"
      )
    Z <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("z11", "z21", "z31", "z22", "z32", "z33"),
        name = "Z"
      )
    
    A <- mxAlgebra(X %*% t(X), name = "A")
    C <- mxAlgebra(Y %*% t(Y), name = "C")
    E <- mxAlgebra(Z %*% t(Z), name = "E")
    
    cov_P <- mxAlgebra(expression = A + C + E, name = "V")
    
    a1_2 <- mxAlgebra(expression = A[1, 1] / V[1, 1], name = "a1_2")
    c1_2 <- mxAlgebra(expression = C[1, 1] / V[1, 1], name = "c1_2")
    e1_2 <- mxAlgebra(expression = E[1, 1] / V[1, 1], name = "e1_2")
    
    a2_2 <- mxAlgebra(expression = A[2, 2] / V[2, 2], name = "a2_2")
    c2_2 <- mxAlgebra(expression = C[2, 2] / V[2, 2], name = "c2_2")
    e2_2 <- mxAlgebra(expression = E[2, 2] / V[2, 2], name = "e2_2")
    
    a3_2 <- mxAlgebra(expression = A[3, 3] / V[3, 3], name = "a3_2")
    c3_2 <- mxAlgebra(expression = C[3, 3] / V[3, 3], name = "c3_2")
    e3_2 <- mxAlgebra(expression = E[3, 3] / V[3, 3], name = "e3_2")
    
    mat_I <- mxMatrix(
      type = "Iden",
      nrow = nv,
      ncol = nv,
      name = "I"
    )
    inv_SD <- mxAlgebra(expression = solve(sqrt(I * V)), name = "iSD")
    
    mean_G <-
      mxMatrix(
        type = "Full",
        nrow = 1,
        ncol = nv * 2,
        free = TRUE,
        values = c(0.07, 0.07, 0.05, 0.05, 0.007, 0.007),
        labels = c("m1", "m1", "m2", "m2", "m3", "m3"),
        name = "expMean"
      )
    
    cov_mz <-
      mxAlgebra(
        expression = rbind(cbind(A + C + E, A + C), cbind(A + C,  A + C + E)),
        name = "expCovMZ"
      )
    cov_dz <-
      mxAlgebra(
        expression = rbind(
          cbind(A + C + E, 0.5 %x% A + C),
          cbind(0.5 %x% A + C, A + C + E)
        ),
        name = "expCovDZ"
      )
    
    obj_mz <-
      mxExpectationNormal(covariance = "expCovMZ",
                          means = "expMean",
                          dimnames = phenos)
    obj_dz <-
      mxExpectationNormal(covariance = "expCovDZ",
                          means = "expMean",
                          dimnames = phenos)
    
    # Calculate genetic and environmental correlations
    corA <-
      mxAlgebra(expression = solve(sqrt(I * A)) %&% A, name = "rA")
    corC <-
      mxAlgebra(expression = solve(sqrt(I * C)) %&% C, name = "rC")
    corE <-
      mxAlgebra(expression = solve(sqrt(I * E)) %&% E, name = "rE")
    
    pars <-
      list(X,
           Y,
           Z,
           A,
           C,
           E,
           a1_2,
           c1_2,
           e1_2,
           a2_2,
           c2_2,
           e2_2,
           a3_2,
           c3_2,
           e3_2,
           cov_P,
           mat_I,
           inv_SD,
           mean_G,
           corA,
           corC,
           corE)
    
    fun_ML <- mxFitFunctionML()
    
    model_mz <- mxModel(pars, cov_mz, mx_mz_data, obj_mz, fun_ML, name = "MZ")
    model_dz <- mxModel(pars, cov_dz, mx_dz_data, obj_dz, fun_ML, name = "DZ")
    
    # Create algebra for variance components
    col_VC <- c("var1", "var2", "var3")
    row_VC <- rep(c('A', 'C', 'E', 'SA', 'SC', 'SE'), each = nv)
    est_VC <-
      mxAlgebra(
        expression = rbind(A, C, E, A / V, C / V, E / V),
        name = "VC",
        dimnames = list(row_VC, col_VC)
      )
    
    fit_ML <-
      mxFitFunctionMultigroup(c("MZ.fitfunction", "DZ.fitfunction"))
    Cf <-
      mxModel(
        "ACE",
        pars,
        model_mz,
        model_dz,
        fit_ML,
        est_VC,
        mxCI(c("a1_2", "c1_2", "e1_2", "a2_2", "c2_2", "e2_2", "a3_2", "c3_2", "e3_2")
        )
      )
    # Run the CholACE model
    mxRun(Cf, intervals = T)
  }
