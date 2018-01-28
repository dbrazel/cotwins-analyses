# Data cleaning
data/processed/Robin_paper-entry_2-22-17_cleaned.rds : data/raw/Robin_paper-entry_2-22-17.rds src/data/clean_paper_data.R
	Rscript src/data/clean_paper_data.R
data/processed/in_person_parents.rds data/processed/in_person_parents.csv : data/raw/Robin_kid-Q-about-parent_12-1-16.rds src/data/get_in_person_parents.R
	Rscript src/data/get_in_person_parents.R
data/processed/PhenX_diagnoses.rds data/processed/PhenX_diagnoses.csv : data/raw/Robin_PhenX_12-6-16.rds src/data/get_phenx_diagnoses.R
	Rscript src/data/get_phenx_diagnoses.R
data/processed/remote_parents.rds data/processed/remote_parents.csv : data/raw/Michigan_LS_parents_5_31_17.csv data/raw/Michigan_DB_surveyentries_05_31_17.rds src/data/get_remote_parents.R
	Rscript src/data/get_remote_parents.R
data/processed/remote_substance_use.rds data/processed/remote_substance_use.csv : data/raw/Michigan_LS_checking_in_11_3_17.csv data/raw/Michigan_DB_surveyentries_09_25_17.rds src/data/get_remote_substance_use.R
	Rscript src/data/get_remote_substance_use.R
data/processed/remote_substance_use_summarized.csv : data/processed/remote_substance_use.rds data/processed/Robin_paper-entry_2-22-17_cleaned.rds data/processed/id_mapping_long.csv src/data/remote_sub_summarize_naomi.R
	Rscript src/data/remote_sub_summarize_naomi.R

# Location processing

