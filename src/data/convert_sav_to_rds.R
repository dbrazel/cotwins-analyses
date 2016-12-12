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
              "Robin_parent-Q-monitoring_12-9-16_conv.")

for (f in prefixes) {
  read_sav(get_path(paste0(f, "sav"))) %>% write_rds(get_path(paste0(f, "rds")))
}
