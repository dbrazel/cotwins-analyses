# Plot the variance component and correlation estimates from the twin growth models

library(readr)
library(cowplot)
library(stringr)
library(dplyr)

at_home_quadratic_comps <- read_rds("data/models/at_home_quadratic_ACE_comps.rds")
at_school_quadratic_comps <- read_rds("data/models/at_school_quadratic_ACE_comps.rds")
par_mon_quadratic_comps <- read_rds("data/models/par_mon_quadratic_ACE_comps.rds")
dpw_quadratic_comps <- read_rds("data/models/dpw_quadratic_ACE_comps.rds")
mpw_quadratic_comps <- read_rds("data/models/mpw_quadratic_ACE_comps.rds")
ecig_quadratic_comps <- read_rds("data/models/ecig_quadratic_ACE_comps.rds")

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

plt <- plot_comps(at_home_quadratic_comps, "Home Growth Model: ACE Estimates", c(0, 1))
save_plot("figs/at_home_quadratic_ACE_comps.pdf", plt)

plt <- plot_comps(at_school_quadratic_comps, "School Growth Model: ACE Estimates", c(0, 1))
save_plot("figs/at_school_quadratic_ACE_comps.pdf", plt)

plt <- plot_comps(par_mon_quadratic_comps, "Parents Growth Model: ACE Estimates", c(0, 1))
save_plot("figs/par_mon_quadratic_ACE_comps.pdf", plt)

plt <- plot_comps(dpw_quadratic_comps, "Alcohol Growth Model: ACE Estimates", c(0, 1))
save_plot("figs/dpw_quadratic_ACE_comps.pdf", plt)

plt <- plot_comps(mpw_quadratic_comps, "Marijuana Growth Model: ACE Estimates", c(0, 1))
save_plot("figs/mpw_quadratic_ACE_comps.pdf", plt)

plt <- plot_comps(ecig_quadratic_comps, "E-Cigarettes Growth Model: ACE Estimates", c(0, 1))
save_plot("figs/ecig_quadratic_ACE_comps.pdf", plt)

plt <- plot_comps(at_home_quadratic_corrs, "Home Growth Model: Correlations", c(-1, 1))
save_plot("figs/at_home_quadratic_ACE_corrs.pdf", plt)

plt <- plot_comps(at_school_quadratic_corrs, "School Growth Model: Correlations", c(-1, 1))
save_plot("figs/at_school_quadratic_ACE_corrs.pdf", plt)

plt <- plot_comps(par_mon_quadratic_corrs, "Parents Growth Model: Correlations", c(-1, 1))
save_plot("figs/par_mon_quadratic_ACE_corrs.pdf", plt)

plt <- plot_comps(dpw_quadratic_corrs, "Alcohol Growth Model: Correlations", c(-1, 1))
save_plot("figs/dpw_quadratic_ACE_corrs.pdf", plt)

plt <- plot_comps(mpw_quadratic_corrs, "Marijuana Growth Model: Correlations", c(-1, 1))
save_plot("figs/mpw_quadratic_ACE_corrs.pdf", plt)

plt <- plot_comps(ecig_quadratic_corrs, "E-Cigarettes Growth Model: Correlations", c(-1, 1))
save_plot("figs/ecig_quadratic_ACE_corrs.pdf", plt)
