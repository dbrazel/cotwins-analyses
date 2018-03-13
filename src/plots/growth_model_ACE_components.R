# Plot the variance component estimates from the twin growth models

library(readr)
library(ggplot2)

# Up the font size
theme_set(theme_gray(base_size = 16))

dpw_quadratic_comps <- read_rds("data/models/dpw_quadratic_ACE_comps.rds")
mpw_quadratic_comps <- read_rds("data/models/mpw_quadratic_ACE_comps.rds")
dpw_linear_comps <- read_rds("data/models/dpw_linear_ACE_comps.rds")
mpw_linear_comps <- read_rds("data/models/mpw_linear_ACE_comps.rds")

ggplot(dpw_quadratic_comps,
       aes(
         pheno,
         estimate,
         fill = component,
         ymax = ubound,
         ymin = lbound
       )) +
  geom_col(position = "dodge") +
  geom_errorbar(position = "dodge") +
  labs(title = "DPW Quadratic Growth Model: ACE Estimates") +
  coord_cartesian(ylim = c(0, 1))
ggsave("figs/dpw_quadratic_ACE_comps.pdf", width = 7, height = 4.67)

ggplot(mpw_quadratic_comps,
       aes(
         pheno,
         estimate,
         fill = component,
         ymax = ubound,
         ymin = lbound
       )) +
  geom_col(position = "dodge") +
  geom_errorbar(position = "dodge") +
  labs(title = "MPW Quadratic Growth Model: ACE Estimates") +
  coord_cartesian(ylim = c(0, 1))
ggsave("figs/mpw_quadratic_ACE_comps.pdf", width = 7, height = 4.67)

ggplot(dpw_linear_comps,
       aes(
         pheno,
         estimate,
         fill = component,
         ymax = ubound,
         ymin = lbound
       )) +
  geom_col(position = "dodge") +
  geom_errorbar(position = "dodge") +
  labs(title = "DPW Linear Growth Model: ACE Estimates") +
  coord_cartesian(ylim = c(0, 1))
ggsave("figs/dpw_linear_ACE_comps.pdf", width = 7, height = 4.67)

ggplot(mpw_linear_comps,
       aes(
         pheno,
         estimate,
         fill = component,
         ymax = ubound,
         ymin = lbound
       )) +
  geom_col(position = "dodge") +
  geom_errorbar(position = "dodge") +
  labs(title = "MPW Linear Growth Model: ACE Estimates") +
  coord_cartesian(ylim = c(0, 1))
ggsave("figs/mpw_linear_ACE_comps.pdf", width = 7, height = 4.67)

