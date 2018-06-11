# Plot the bootstrapped individual growth model CIs

library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)

all_cis <- read_rds("data/models/all_phenos_cis.rds")

all_cis <-
  mutate(all_cis,
         term = factor(term, levels = all_cis$term[1:13]),
         pheno = factor(pheno, levels = c("Home", "School", "Parents", "Alcohol", "Marijuana", "E-Cigarettes")))

ggplot(all_cis,
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

ggsave("figs/growth_model_individual_params.pdf", width = 17, height = 8)

# Plot just the std dev estimates
all_cis_std <- filter(all_cis, str_detect(term, "std_dev"))
ggplot(all_cis_std,
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

ggsave("figs/growth_model_individual_params_std_devs.pdf", width = 10, height = 8)
