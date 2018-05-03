# Plot the variance component and correlation estimates from the twin growth models

library(readr)
library(ggplot2)
library(stringr)
library(dplyr)

# Up the font size
theme_set(theme_gray(base_size = 16))

at_home_quadratic_comps <- read_rds("data/models/at_home_quadratic_ACE_comps.rds")
at_school_quadratic_comps <- read_rds("data/models/at_school_quadratic_ACE_comps.rds")
par_mon_quadratic_comps <- read_rds("data/models/par_mon_quadratic_ACE_comps.rds")
dpw_quadratic_comps <- read_rds("data/models/dpw_quadratic_ACE_comps.rds")
mpw_quadratic_comps <- read_rds("data/models/mpw_quadratic_ACE_comps.rds")
ecig_quadratic_comps <- read_rds("data/models/ecig_quadratic_ACE_comps.rds")
dpw_linear_comps <- read_rds("data/models/dpw_linear_ACE_comps.rds")
mpw_linear_comps <- read_rds("data/models/mpw_linear_ACE_comps.rds")
ecig_linear_comps <- read_rds("data/models/ecig_linear_ACE_comps.rds")

at_home_quadratic_corrs <- filter(at_home_quadratic_comps, str_detect(pheno, "to"))
at_home_quadratic_comps <- filter(at_home_quadratic_comps, !str_detect(pheno, "to"))

at_school_quadratic_corrs <- filter(at_school_quadratic_comps, str_detect(pheno, "to"))
at_school_quadratic_comps <- filter(at_school_quadratic_comps, !str_detect(pheno, "to"))

par_mon_quadratic_corrs <- filter(par_mon_quadratic_comps, str_detect(pheno, "to"))
par_mon_quadratic_comps <- filter(par_mon_quadratic_comps, !str_detect(pheno, "to"))

dpw_quadratic_corrs <- filter(dpw_quadratic_comps, str_detect(pheno, "to"))
dpw_quadratic_comps <- filter(dpw_quadratic_comps, !str_detect(pheno, "to"))

mpw_quadratic_corrs <- filter(mpw_quadratic_comps, str_detect(pheno, "to"))
mpw_quadratic_comps <- filter(mpw_quadratic_comps, !str_detect(pheno, "to"))

ecig_quadratic_corrs <- filter(ecig_quadratic_comps, str_detect(pheno, "to"))
ecig_quadratic_comps <- filter(ecig_quadratic_comps, !str_detect(pheno, "to"))

dpw_linear_corrs <- filter(dpw_linear_comps, str_detect(pheno, "to"))
dpw_linear_comps <- filter(dpw_linear_comps, !str_detect(pheno, "to"))

mpw_linear_corrs <- filter(mpw_linear_comps, str_detect(pheno, "to"))
mpw_linear_comps <- filter(mpw_linear_comps, !str_detect(pheno, "to"))

ecig_linear_corrs <- filter(ecig_linear_comps, str_detect(pheno, "to"))
ecig_linear_comps <- filter(ecig_linear_comps, !str_detect(pheno, "to"))

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

plot_comps(at_home_quadratic_comps, "Home Quadratic Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/at_home_quadratic_ACE_comps.pdf", width = width, height = height)

plot_comps(at_school_quadratic_comps, "School Quadratic Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/at_school_quadratic_ACE_comps.pdf", width = width, height = height)

plot_comps(par_mon_quadratic_comps, "Parents Quadratic Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/par_mon_quadratic_ACE_comps.pdf", width = width, height = height)

plot_comps(dpw_quadratic_comps, "DPW Quadratic Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/dpw_quadratic_ACE_comps.pdf", width = width, height = height)

plot_comps(mpw_quadratic_comps, "MPW Quadratic Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/mpw_quadratic_ACE_comps.pdf", width = width, height = height)

plot_comps(ecig_quadratic_comps, "PPW Quadratic Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/ecig_quadratic_ACE_comps.pdf", width = width, height = height)

plot_comps(dpw_linear_comps, "DPW Linear Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/dpw_linear_ACE_comps.pdf", width = width, height = height)

plot_comps(mpw_linear_comps, "MPW Linear Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/mpw_linear_ACE_comps.pdf", width = width, height = height)

plot_comps(ecig_linear_comps, "PPW Linear Growth Model: ACE Estimates", c(0, 1))
ggsave("figs/ecig_linear_ACE_comps.pdf", width = width, height = height)

plot_comps(at_home_quadratic_corrs, "Home Quadratic Growth Model: Correlations", c(-1, 1))
ggsave("figs/at_home_quadratic_ACE_corrs.pdf", width = width, height = height)

plot_comps(at_school_quadratic_corrs, "School Quadratic Growth Model: Correlations", c(-1, 1))
ggsave("figs/at_school_quadratic_ACE_corrs.pdf", width = width, height = height)

plot_comps(par_mon_quadratic_corrs, "Parents Quadratic Growth Model: Correlations", c(-1, 1))
ggsave("figs/par_mon_quadratic_ACE_corrs.pdf", width = width, height = height)

plot_comps(dpw_quadratic_corrs, "DPW Quadratic Growth Model: Correlations", c(-1, 1))
ggsave("figs/dpw_quadratic_ACE_corrs.pdf", width = width, height = height)

plot_comps(mpw_quadratic_corrs, "MPW Quadratic Growth Model: Correlations", c(-1, 1))
ggsave("figs/mpw_quadratic_ACE_corrs.pdf", width = width, height = height)

plot_comps(ecig_quadratic_corrs, "PPW Quadratic Growth Model: Correlations", c(-1, 1))
ggsave("figs/ecig_quadratic_ACE_corrs.pdf", width = width, height = height)

plot_comps(dpw_linear_corrs, "DPW Linear Growth Model: Correlations", c(-1, 1))
ggsave("figs/dpw_linear_ACE_corrs.pdf", width = width, height = height)

plot_comps(mpw_linear_corrs, "MPW Linear Growth Model: Correlations", c(-1, 1))
ggsave("figs/mpw_linear_ACE_corrs.pdf", width = width, height = height)

plot_comps(ecig_linear_corrs, "PPW Linear Growth Model: Correlations", c(-1, 1))
ggsave("figs/ecig_linear_ACE_corrs.pdf", width = width, height = height)
