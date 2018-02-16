# Take the random slopes and intercepts genrated by dpd_linear_model.R and
# use plain OpenMx to fit a bivariate ACE model and two univariate ACE models

library(OpenMx)
library(readr)
library(dplyr)

re_est <- read_rds("data/models/dpd_linear_random_effects.rds")
id_mapping <- read_csv("data/processed/id_mapping.csv", col_types = "ccccc")

# Filter out the intercept and slope terms
intercepts <- filter(re_est, term == "b_1i")
slopes <- filter(re_est, term == "b_2i")

# Get the slope and intercepts for each twin pair
id_mapping <- select(id_mapping, T1_id = T1_alternate_id, T2_id = T2_alternate_id, bestzygos)

id_mapping <- left_join(id_mapping, intercepts, by = c("T1_id" = "twin"))
id_mapping <- select(id_mapping, T1_id:bestzygos, intercept1 = estimate)

id_mapping <- left_join(id_mapping, intercepts, by = c("T2_id" = "twin"))
id_mapping <- select(id_mapping, T1_id:intercept1, intercept2 = estimate)

id_mapping <- left_join(id_mapping, slopes, by = c("T1_id" = "twin"))
id_mapping <- select(id_mapping, T1_id:intercept2, slope1 = estimate)

id_mapping <- left_join(id_mapping, slopes, by = c("T2_id" = "twin"))
id_mapping <- select(id_mapping, T1_id:slope1, slope2 = estimate)

# Separate out MZs and DZs
mzData <- filter(id_mapping, bestzygos == "MZ")
dzData <- filter(id_mapping, bestzygos %in% c("DZ", "OS"))

# Select just the phenotypes
dzData <- dzData[, 4:7]
mzData <- mzData[, 4:7]

# Convert to MxData objects
mxDataMZ <- mxData(observed = data.frame(mzData), type="raw")
mxDataDZ <- mxData(observed = data.frame(dzData), type="raw")

# Univariate model of the intercept

selVars <- c("intercept1", "intercept2")
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

covP <- mxAlgebra(expression=A+C+E, name="V")

a2 <- mxAlgebra(expression=A/V, name="a2")
c2 <- mxAlgebra(expression=C/V, name="c2")
e2 <- mxAlgebra(expression=E/V, name="e2")


matI <- mxMatrix(type = "Iden", nrow = nv, ncol = nv, name = "I")
invSD <- mxAlgebra(expression = solve(sqrt(I*V)), name = "iSD")
meanG <-
  mxMatrix(
    type = "Full",
    nrow = 1,
    ncol = nv * 2,
    free = TRUE,
    values = 1,
    labels = c("m1", "m1"),
    name = "expMean"
  )

covMZ <-
  mxAlgebra(
    expression = rbind(cbind(A+C+E, A+C), cbind(A+C,  A+C+E)),
    name = "expCovMZ"
    )
covDZ <-
  mxAlgebra(
    expression = rbind(cbind(A+C+E, 0.5 %x% A+C), cbind(0.5 %x% A+C, A+C+E)),
    name="expCovDZ")

### Combine Groups
objMZ <-
  mxExpectationNormal(covariance = "expCovMZ",
                      means = "expMean",
                      dimnames = selVars)
objDZ <-
  mxExpectationNormal(covariance = "expCovDZ",
                      means = "expMean",
                      dimnames = selVars)


pars <- list(X, Y, Z, A, C, E, a2, c2, e2, covP, matI, invSD, meanG)

funML <- mxFitFunctionML()

ModelMZ <- mxModel(pars, covMZ, mxDataMZ, objMZ, funML, name = "MZ")
ModelDZ <- mxModel(pars, covDZ, mxDataDZ, objDZ, funML, name = "DZ")

fitML <- mxFitFunctionMultigroup(c("MZ.fitfunction","DZ.fitfunction"))
Cf <- mxModel("ACE", pars, ModelMZ, ModelDZ, fitML, mxCI(c('a2', 'c2', 'e2')))

intercept_fit <- mxRun(Cf, intervals=T) ### Run CholACE model

print("Summary of the univariate intercept model")
summary(intercept_fit)

