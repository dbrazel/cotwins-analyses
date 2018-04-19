# Plot the predicted trajectories and residuals from the growth models

library(readr)
library(ggplot2)

# Up the font size
theme_set(theme_gray(base_size = 16))

dpw_linear_preds <- read_rds("data/models/dpw_linear_predictions.rds")
dpw_quad_preds <- read_rds("data/models/dpw_quadratic_predictions.rds")
mpw_linear_preds <- read_rds("data/models/mpw_linear_predictions.rds")
mpw_quad_preds <- read_rds("data/models/mpw_quadratic_predictions.rds")
ecig_linear_preds <- read_rds("data/models/ecig_linear_predictions.rds")
ecig_quad_preds <- read_rds("data/models/ecig_quadratic_predictions.rds")

ggplot(dpw_linear_preds,
       aes(test_age + 17, drinks_per_week_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Predicted log DPW",
    title = "Linear growth model of drinks per week")
ggsave("figs/dpw_linear_preds.pdf", width = 7, height = 4.67)

ggplot(dpw_quad_preds,
       aes(test_age + 17, drinks_per_week_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Predicted log DPW",
    title = "Quadratic growth model of drinks per week")
ggsave("figs/dpw_quad_preds.pdf", width = 7, height = 4.67)

ggplot(mpw_linear_preds,
       aes(test_age + 17, mar_per_week_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Predicted log MPW",
    title = "Linear growth model of marijuana uses per week")
ggsave("figs/mpw_linear_preds.pdf", width = 7, height = 4.67)

ggplot(mpw_quad_preds,
       aes(test_age + 17, mar_per_week_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Predicted log MPW",
    title = "Quadratic growth model of marijuana uses per week")
ggsave("figs/mpw_quad_preds.pdf", width = 7, height = 4.67)

ggplot(ecig_linear_preds,
       aes(test_age + 17, puffs_per_week_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Predicted log PPW",
    title = "Linear growth model of ecig puffs per week")
ggsave("figs/ecig_linear_preds.pdf", width = 7, height = 4.67)

ggplot(ecig_quad_preds,
       aes(test_age + 17, puffs_per_week_pred, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Predicted log PPW",
    title = "Quadratic growth model of ecig puffs per week")
ggsave("figs/ecig_quad_preds.pdf", width = 7, height = 4.67)

ggplot(dpw_linear_preds,
       aes(test_age + 17, drinks_per_week_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Residual log DPW",
    title = "Linear growth model of drinks per week")
ggsave("figs/dpw_linear_resids.pdf", width = 7, height = 4.67)

ggplot(dpw_quad_preds,
       aes(test_age + 17, drinks_per_week_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Residual log DPW",
    title = "Quadratic growth model of drinks per week")
ggsave("figs/dpw_quad_resids.pdf", width = 7, height = 4.67)

ggplot(mpw_linear_preds,
       aes(test_age + 17, mar_per_week_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Residual log MPW",
    title = "Linear growth model of marijuana uses per week")
ggsave("figs/mpw_linear_resids.pdf", width = 7, height = 4.67)

ggplot(mpw_quad_preds,
       aes(test_age + 17, mar_per_week_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Residual log MPW",
    title = "Quadratic growth model of marijuana uses per week")
ggsave("figs/mpw_quad_resids.pdf", width = 7, height = 4.67)

ggplot(ecig_linear_preds,
       aes(test_age + 17, puffs_per_week_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Residual log PPW",
    title = "Linear growth model of ecig puffs per week")
ggsave("figs/ecig_linear_resids.pdf", width = 7, height = 4.67)

ggplot(ecig_quad_preds,
       aes(test_age + 17, puffs_per_week_resid, group = user_id)) +
  geom_line(alpha = 0.5) +
  labs(
    x = "Age in years",
    y = "Residual log PPW",
    title = "Quadratic growth model of ecig puffs per week")
ggsave("figs/ecig_quad_resids.pdf", width = 7, height = 4.67)
