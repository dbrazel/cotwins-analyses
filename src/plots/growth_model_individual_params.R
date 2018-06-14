# Plot the bootstrapped individual growth model CIs

library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(forcats)

all_cis <- read_rds("data/models/all_phenos_cis.rds")

all_cis <-
  mutate(all_cis,
         term = factor(term, levels = all_cis$term[1:13]),
         pheno = factor(pheno, levels = c("Alcohol", "Marijuana", "E-Cigarettes", "Parents", "Home", "School")))

levels(all_cis$term) <- c(
  "Twin SD Intercept",
  "Twin SD Slope",
  "Twin SD Quadratic",
  "Twin Intercept <-> Slope",
  "Twin Intercept <-> Quadratic",
  "Twin Slope <-> Quadratic",
  "Family SD Intercept",
  "Family SD Slope",
  "Family SD Quadratic",
  "Family Intercept <-> Slope",
  "Family Intercept <-> Quadratic",
  "Family Slope <-> Quadratic",
  "Residual Variance"
)

plt_all <- ggplot(all_cis,
       aes(
         term,
         statistic,
         fill = pheno,
         ymax = conf.high,
         ymin = conf.low
       )) +
  geom_col(position = "dodge") +
  geom_errorbar(position = "dodge") +
  labs(y = "Estimate") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 50, hjust = 1))

save_plot("figs/growth_model_individual_params.pdf", plt_all, base_aspect_ratio = 2.2, base_height = 6)

# Plot just the std dev estimates
all_cis_std <- filter(all_cis, str_detect(term, "SD|Residual"))
plt_std <- ggplot(all_cis_std,
       aes(
         pheno,
         statistic,
         fill = term,
         ymax = conf.high,
         ymin = conf.low
       )) +
  geom_col(position = "dodge") +
  geom_errorbar(position = "dodge") +
  labs(y = "Estimate") +
  theme(axis.title.x = element_blank(), legend.title = element_blank())

save_plot("figs/growth_model_individual_params_std_devs.pdf", plt_std, base_height = 8)

# Plot just the correlations
all_cis_cor <- filter(all_cis, str_detect(term, "<->"))
plt_cor <- ggplot(all_cis_cor,
                  aes(
                    pheno,
                    statistic,
                    fill = term,
                    ymax = conf.high,
                    ymin = conf.low
                  )) +
  geom_col(position = "dodge") +
  geom_errorbar(position = "dodge") +
  labs(y = "Estimate") +
  theme(axis.title.x = element_blank(), legend.title = element_blank())

save_plot("figs/growth_model_individual_params_cors.pdf", plt_cor, base_aspect_ratio = 1.8, base_height = 6)
