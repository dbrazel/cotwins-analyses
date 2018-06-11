# Plot the predicted trajectories and residuals from the growth models

library(readr)
library(cowplot)

at_home_quad_preds <- read_rds("data/models/at_home_quadratic_predictions.rds")
at_school_quad_preds <- read_rds("data/models/at_school_quadratic_predictions.rds")
par_mon_quad_preds <- read_rds("data/models/par_mon_quadratic_predictions.rds")
dpw_quad_preds <- read_rds("data/models/dpw_quadratic_predictions.rds")
mpw_quad_preds <- read_rds("data/models/mpw_quadratic_predictions.rds")
ecig_quad_preds <- read_rds("data/models/ecig_quadratic_predictions.rds")

plt <- ggplot(at_home_quad_preds,
       aes(test_age + 17, home_frac_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Predicted fraction of time",
    title = "Quadratic growth model of time at home at night")
save_plot("figs/at_home_quad_preds.pdf", plt, base_aspect_ratio = 1.4)

plt <- ggplot(at_school_quad_preds,
       aes(test_age + 17, school_frac_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Predicted fraction of time",
    title = "Quadratic growth model of time at school")
save_plot("figs/at_school_quad_preds.pdf", plt, base_aspect_ratio = 1.4)

plt <- ggplot(par_mon_quad_preds,
       aes(test_age + 17, max_monitor_score_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Predicted score",
    title = "Quadratic growth model of parental monitoring")
save_plot("figs/par_mon_quad_preds.pdf", plt, base_aspect_ratio = 1.4)

plt <- ggplot(dpw_quad_preds,
       aes(test_age + 17, drinks_per_week_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Predicted ln of use",
    title = "Quadratic growth model of drinks per week")
save_plot("figs/dpw_quad_preds.pdf", plt, base_aspect_ratio = 1.4)

plt <- ggplot(mpw_quad_preds,
       aes(test_age + 17, mar_per_week_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Predicted ln of use",
    title = "Quadratic growth model of marijuana uses per week")
save_plot("figs/mpw_quad_preds.pdf", plt, base_aspect_ratio = 1.4)

plt <- ggplot(ecig_quad_preds,
       aes(test_age + 17, ecig_per_week_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Predicted ln of use",
    title = "Quadratic growth model of ecig uses per week")
save_plot("figs/ecig_quad_preds.pdf", plt, base_aspect_ratio = 1.4)

plt <- ggplot(at_home_quad_preds,
       aes(test_age + 17, home_frac_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Residual of fraction of time",
    title = "Quadratic growth model of time at home at night")
save_plot("figs/at_home_quad_resids.pdf", plt, base_aspect_ratio = 1.4)

plt <- ggplot(at_school_quad_preds,
       aes(test_age + 17, school_frac_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Residual of fraction of time",
    title = "Quadratic growth model of time at school")
save_plot("figs/at_school_quad_resids.pdf", plt, base_aspect_ratio = 1.4)

plt <- ggplot(par_mon_quad_preds,
       aes(test_age + 17, max_monitor_score_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Residual score",
    title = "Quadratic growth model of parental monitoring")
save_plot("figs/par_mon_quad_resids.pdf", plt, base_aspect_ratio = 1.4)

plt <- ggplot(dpw_quad_preds,
       aes(test_age + 17, drinks_per_week_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Residual ln of use",
    title = "Quadratic growth model of drinks per week")
save_plot("figs/dpw_quad_resids.pdf", plt, base_aspect_ratio = 1.4)

plt <- ggplot(mpw_quad_preds,
       aes(test_age + 17, mar_per_week_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Residual ln of use",
    title = "Quadratic growth model of marijuana uses per week")
save_plot("figs/mpw_quad_resids.pdf", plt, base_aspect_ratio = 1.4)

plt <- ggplot(ecig_quad_preds,
       aes(test_age + 17, ecig_per_week_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age (years)",
    y = "Residual ln of use",
    title = "Quadratic growth model of ecig uses per week")
save_plot("figs/ecig_quad_resids.pdf", plt, base_aspect_ratio = 1.4)
