# Take the random slopes and intercepts genrated by dpd_linear_model.R and
# use umx to fit a bivariate ACE model and two univariate ACE models

library(readr)
library(dplyr)
library(umx)

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

# Univariate model of the intercepts
int_model <- umxACE(selDVs = "intercept", dzData = dzData, mzData = mzData, sep = "")

# Univariate model of the slopes
slope_model <- umxACE(selDVs = "slope", dzData = dzData, mzData = mzData, sep = "")

# Bivariate model
both_model <- umxACE(selDVs = c("slope", "intercept"), dzData = dzData, mzData = mzData, sep = "")
