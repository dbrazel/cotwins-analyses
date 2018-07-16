# Another plot of the bootstrapped cross phenotype correlations for the growth
# model parameters, meant for the paper main text

library(readr)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)

# To save space, plot just the intercept and slope correlations
all_cis <- read_rds("data/models/cross_pheno_cis.rds") %>%
  as_tibble() %>%
  filter(term %in% c("cor_int_int", "cor_slope_slope"))

all_cis$term <-
  factor(
    all_cis$term,
    levels = c("cor_int_int", "cor_slope_slope"),
    labels = c("Intercept", "Slope")
  )

all_cis <- separate(
  all_cis,
  pheno,
  sep = " <-> ",
  into = c("pheno_a", "pheno_b")
  )

# Make the correlation matrix lower triangular, this is ugly but I'm in a hurry
all_cis[31, ] = list("Intercept", 1, NA, NA, NA, NA, "Alcohol", "Alcohol")
all_cis[32, ] = list("Slope", 1, NA, NA, NA, NA, "Alcohol", "Alcohol")
all_cis[33, ] = list("Intercept", 1, NA, NA, NA, NA, "Marijuana", "Marijuana")
all_cis[34, ] = list("Slope", 1, NA, NA, NA, NA, "Marijuana", "Marijuana")
all_cis[35, ] = list("Intercept", 1, NA, NA, NA, NA, "E-Cigarettes", "E-Cigarettes")
all_cis[36, ] = list("Slope", 1, NA, NA, NA, NA, "E-Cigarettes", "E-Cigarettes")
all_cis[37, ] = list("Intercept", 1, NA, NA, NA, NA, "Parents", "Parents")
all_cis[38, ] = list("Slope", 1, NA, NA, NA, NA, "Parents", "Parents")
all_cis[39, ] = list("Intercept", 1, NA, NA, NA, NA, "Home", "Home")
all_cis[40, ] = list("Slope", 1, NA, NA, NA, NA, "Home", "Home")
all_cis[41, ] = list("Intercept", 1, NA, NA, NA, NA, "School", "School")
all_cis[42, ] = list("Slope", 1, NA, NA, NA, NA, "School", "School")

all_cis[1, c("pheno_a", "pheno_b")] <- c("Marijuana", "Alcohol")
all_cis[2, c("pheno_a", "pheno_b")] <- c("Marijuana", "Alcohol")
all_cis[3, c("pheno_a", "pheno_b")] <- c("E-Cigarettes", "Alcohol")
all_cis[4, c("pheno_a", "pheno_b")] <- c("E-Cigarettes", "Alcohol")
all_cis[5, c("pheno_a", "pheno_b")] <- c("E-Cigarettes", "Marijuana")
all_cis[6, c("pheno_a", "pheno_b")] <- c("E-Cigarettes", "Marijuana")
all_cis[25, c("pheno_a", "pheno_b")] <- c("School", "Home")
all_cis[26, c("pheno_a", "pheno_b")] <- c("School", "Home")

all_cis$pheno_a <-
  factor(
    all_cis$pheno_a,
    levels = c(
      "Alcohol",
      "Marijuana",
      "E-Cigarettes",
      "Parents",
      "Home",
      "School")
  )

all_cis$pheno_b <-
  factor(
    all_cis$pheno_b,
    levels = c(
      "Alcohol",
      "Marijuana",
      "E-Cigarettes",
      "Parents",
      "Home",
      "School")
  )

# Round the correlations for display
all_cis$stat_form <- round(all_cis$statistic, digits = 2)

plt <- ggplot(all_cis, aes(pheno_a, pheno_b, fill = statistic)) +
  geom_tile() +
  geom_text(aes(label = stat_form)) +
  facet_wrap(~term, ncol = 1, scales = "free") +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = "Correlation") +
  ylab(NULL) +
  xlab(NULL) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    legend.direction = "horizontal",
    legend.position = c(0.55, 0.86),
    legend.justification = c(1, 0),
    legend.title = element_text(size = 12)
    ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    barwidth = 7,
    barheight = 1,
    title.hjust = 0.5
    ))

save_plot(
  "figs/growth_model_cross_pheno_cors_paper.pdf",
  plt,
  base_aspect_ratio = 0.6,
  base_height = 8)
