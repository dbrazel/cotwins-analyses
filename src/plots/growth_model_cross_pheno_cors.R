# Plot the bootstrapped cross phenotype correlations for the growth model parameters

library(readr)
library(ggplot2)
library(cowplot)

all_cis <- read_rds("data/models/cross_pheno_cis.rds")

all_cis$pheno <- factor(all_cis$pheno, levels = unique(all_cis$pheno))
all_cis$term <- factor(
  all_cis$term,
  levels = unique(all_cis$term),
  labels = c(
    "Intercept ↔ Intercept",
    "Intercept ↔ Slope",
    "Intercept ↔ Quadratic",
    "Slope ↔ Intercept",
    "Slope ↔ Slope",
    "Slope ↔ Quadratic",
    "Quadratic ↔ Intercept",
    "Quadratic ↔ Slope",
    "Quadratic ↔ Quadratic"
  ))

ggplot(all_cis, aes(pheno, statistic, fill = term, ymax = conf.high, ymin = conf.low)) +
  geom_col(position = "dodge") +
  geom_errorbar(position = "dodge") +
  labs(y = "Estimate") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 50, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

# cairo_pdf is required because the default device can't handle multibyte characters (the arrows)
ggsave("figs/growth_model_cross_pheno_cors.pdf", width = 17, height = 8, device = cairo_pdf)
