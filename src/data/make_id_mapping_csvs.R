# Create the CSVs required for the python script to generate the ID mapping

library(readr)
library(dplyr)

paper <- read_rds(file.path("data", "raw", "Robin_paper-entry_2-22-17.rds"))
users <- read_rds(file.path("data", "raw", "Michigan_DB_users_05_31_17.rds"))

# Rearrange paper to produce one row per twin pair
paper <- paper %>% select(ID = ID1, bestzygos)
paper2 <- tbl_df(data.frame(c(paper[seq(1, nrow(paper), by = 2), "ID"]), 
                 c(paper[seq(2, nrow(paper), by = 2), "ID"]),
                 "bestzygos" = c(paper[seq(1, nrow(paper), by = 2), "bestzygos"]),
                 stringsAsFactors = FALSE))
paper2 <- select(paper2, T1 = ID, T2 = `ID.1`, bestzygos)

write_csv(paper2, file.path("data", "raw", "Robin_paper-entry_2-22-17.csv"))
write_csv(users, file.path("data", "raw", "Michigan_DB_users_05-31-17.csv"))