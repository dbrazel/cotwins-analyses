# Convert SPSS files in data/raw to RDS format

library(haven)
library(readr)
library(magrittr)

get_path <- function(fname) {
  file.path("data", "raw", fname)
}

prefixes <- c("Robin_kid-Q-about-parent_12-1-16.", 
              "Robin_paper-entry_12-6-16.", 
              "Robin_PhenX_12-6-16.",
              "Robin_kid-Q-general_12-7-16.", 
              "Robin_parent-Q-monitoring_12-9-16_conv.",
              "Robin_parent-Q-monitoring_12-14-16_conv.",
              "Robin_kid-Q-life-events_12-19-16.",
              "Robin_kid-Q-1_12-19-16.",
              "Robin_parent-Q-monitoring_2-21-17_conv.",
              "Robin_paper-entry_2-22-17.",
              "Robin_parent-Q-other_2-21-17_conv.")

for (f in prefixes) {
  read_sav(get_path(paste0(f, "sav"))) %>% write_rds(get_path(paste0(f, "rds")))
}
