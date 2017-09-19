# Remove errors from the paper data in the dates and add Dates

library(readr)
library(dplyr)
library(lubridate)

paper_data <- read_rds("data/raw/Robin_paper-entry_2-22-17.rds")

paper_data$Test_Year <- recode(as.numeric(paper_data$Test_Year),
                               `15` = 2015,
                               `16` = 2016)

paper_data$Birth_Year <- recode(
  as.numeric(paper_data$Birth_Year),
  `0` = 2000,
  `1` = 2001,
  `99` = 1999,
  `98` = 1998
)

paper_data <-
  mutate(paper_data,
         Test_Date = make_date(year = Test_Year,
                               month = Test_Month,
                               day = Test_Day),
         Birth_Date = make_date(year = Birth_Year,
                                month = Birth_Month,
                                day = Birth_Day))

paper_data <- mutate(paper_data,
                     Test_Age = Test_Date - Birth_Date)

write_rds(paper_data, "data/processed/Robin_paper-entry_2-22-17_cleaned.rds")
