This directory contains data dictionaries and other documentation.

---

`CoTwinsDataModel_12_12_16.docx.pdf`

Documents the schema for both Michigan MySQL databases. Exported from Google Drive on December 12, 2016.

---

`phenx_column_labels`

Contains the column names and labels for `data/raw/Robin_PhenX_12-6-16.rds`, extracted using this mildly horrifying one-liner:

`Rscript -e 'library(readr);library(Hmisc);phenx <- read_rds("data/raw/Robin_PhenX_12-6-16.rds");label(phenx)' | xargs | sed 's/ PHXQ/;PHXQ/g' | tr ';' '\n' > references/phenx_column_labels`

xargs strips the white space, sed replaces the spaces between columns with semicolons, tr replaces the semicolons with newlines. I used this workaround because sed, at least on macOS, will not accept escaped newlines on the right hand side of the expression.

---

`HealthyKidsColoradoExecutiveSummary.pdf`

The 2015 Executive Summary for the Healthy Kids Colorado Survey. Information on substance use rates in high school students in Colorado. Downloaded from http://www.ucdenver.edu/academics/colleges/PublicHealth/community/CEPEG/UnifYouth/Pages/HealthyKidsSurvey.aspx

`2015HKCS_HS_Tables.pdf` and `2015HKCS_HS_Table of Contents.pdf`

Detailed results from the Healthy Kids Colorado Survey on high school students in Colorado. Downloaded from https://www.cohealthdata.dphe.state.co.us/

---

`DPS*calendar.pdf`

The Denver Public Schools calendars
