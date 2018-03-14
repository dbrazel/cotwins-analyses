# Plot the variance component and correlation estimates from the twin growth models

library(readr)
library(ggplot2)
library(stringr)
library(dplyr)

# Up the font size
theme_set(theme_gray(base_size = 16))

dpw_quadratic_comps <- read_rds("data/models/dpw_quadratic_ACE_comps.rds")
mpw_quadratic_comps <- read_rds("data/models/mpw_quadratic_ACE_comps.rds")
dpw_linear_comps <- read_rds("data/models/dpw_linear_ACE_comps.rds")
mpw_linear_comps <- read_rds("data/models/mpw_linear_ACE_comps.rds")

dpw_quadratic_corrs <- filter(dpw_quadratic_comps, str_detect(pheno, "to"))
dpw_quadratic_comps <- filter(dpw_quadratic_comps, !str_detect(pheno, "to"))

mpw_quadratic_corrs <- filter(mpw_quadratic_comps, str_detect(pheno, "to"))
mpw_quadratic_comps <- filter(mpw_quadratic_comps, !str_detect(pheno, "to"))

dpw_linear_corrs <- filter(dpw_linear_comps, str_detect(pheno, "to"))
dpw_linear_comps <- filter(dpw_linear_comps, !str_detect(pheno, "to"))

mpw_linear_corrs <- filter(mpw_linear_comps, str_detect(pheno, "to"))
mpw_linear_comps <- filter(mpw_linear_comps, !str_detect(pheno, "to"))

plot_comps <- function(df, title, ylim) {
  ggplot(df,
         aes(
           pheno,
           estimate,
           fill = component,
           ymax = ubound,
           ymin = lbound
         )) +
    geom_col(position = "dodge") +
    geom_errorbar(position = "dodge") +
    labs(title = title, y = "Estimate") +
    coord_cartesian(ylim = ylim) +
    theme(axis.title.x = element_blank(), legend.title = element_blank())
}

# 3:2 width:height
width <- 8
height <- 5.28

plot_comps(dpw_quadratic_comps, "DPW Quadratic Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/dpw_quadratic_ACE_comps.pdf", width = width, height = height)

plot_comps(mpw_quadratic_comps, "MPW Quadratic Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/mpw_quadratic_ACE_comps.pdf", width = width, height = height)

plot_comps(dpw_linear_comps, "DPW Linear Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/dpw_linear_ACE_comps.pdf", width = width, height = height)

plot_comps(mpw_linear_comps, "MPW Linear Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/mpw_linear_ACE_comps.pdf", width = width, height = height)

plot_comps(dpw_quadratic_corrs, "DPW Quadratic Growth Model: Correlations", c(-1, 1))
ggsave("figs/dpw_quadratic_ACE_corrs.pdf", width = width, height = height)

plot_comps(mpw_quadratic_corrs, "MPW Quadratic Growth Model: Correlations", c(-1, 1))
ggsave("figs/mpw_quadratic_ACE_corrs.pdf", width = width, height = height)

plot_comps(dpw_linear_corrs, "DPW Linear Growth Model: Correlations", c(-1, 1))
ggsave("figs/dpw_linear_ACE_corrs.pdf", width = width, height = height)

plot_comps(mpw_linear_corrs, "MPW Linear Growth Model: Correlations", c(-1, 1))
ggsave("figs/mpw_linear_ACE_corrs.pdf", width = width, height = height)
