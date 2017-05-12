# The PhenX substance use working group defined substance dependence and abuse
# measures based on the DSM IV and on the National Epidemiologic Survey on Alcohol
# and Related Conditions (NESARC). The purpose of this script is to extract 
# symptom counts and diagnoses for alcohol, tobacco, marijuana, and other
# drugs from the in-person PhenX questionnaire data for each participant.
# Broadly, each substance and diagnosis pair has a set of symptoms associated
# with it. Individuals with more than a certain number of symptoms are
# diagnosed with the disorder. Symptoms are assigned based on the answers to
# specific questions or sets of questions.
# 
# The definitions of the symptoms and the cut-offs for each disorder can be 
# found at the following pages, under "Protocol":
# 
# Alcohol: https://www.phenxtoolkit.org/index.php?pageLink=browse.protocoldetails&id=510401
# Current Cigarette Dependence: https://www.phenxtoolkit.org/index.php?pageLink=browse.protocoldetails&id=31001
# Tobacco: https://www.phenxtoolkit.org/index.php?pageLink=browse.protocoldetails&id=510403
# Other Drugs: https://www.phenxtoolkit.org/index.php?pageLink=browse.protocoldetails&id=510402

library(readr)
library(dplyr)

phenx <- read_rds('data/raw/Robin_PhenX_12-6-16.rds')
results <- phenx %>% select(
	SVID = id, 
	tester_ID = PHXTESTER, 
	tester_comments = PHXQ0353
	)

# Life as a variable prefix indicates that a symptom or diagnosis is
# present at some point in an individual's life. Year indicates that
# it is present in the 12 months before assessment

# 1 codes for true, 2 for false, in these data

# Alcohol abuse symptoms
life_alc_failure <- (phenx$PHXQ0068 == 1) | (phenx$PHXQ0070 == 1)
year_alc_failure <- life_alc_failure & ((phenx$PHXQ0069 == 1) | (phenx$PHXQ0071 == 1))

life_alc_hazard <- (phenx$PHXQ0072 == 1) | (phenx$PHXQ0076 == 1) | (phenx$PHXQ0078 == 1)
year_alc_hazard <- life_alc_hazard & ((phenx$PHXQ0073 == 1) | (phenx$PHXQ0077 == 1) | (phenx$PHXQ0079 == 1))

life_alc_continue <- (phenx$PHXQ0080 == 1) | (phenx$PHXQ0082 == 1)
year_alc_continue <- life_alc_continue & ((phenx$PHXQ0081 == 1) | (phenx$PHXQ0083 == 1))

life_alc_legal <- phenx$PHXQ0084 == 1
year_alc_legal <- life_alc_legal & (phenx$PHXQ0085 == 1)

# Alcohol abuse diagnoses and symptom counts
results$life_alc_abuse_count <- rowSums(tibble(life_alc_failure, life_alc_hazard, life_alc_continue, life_alc_legal), na.rm = T)
results$life_alc_abuse <- results$life_alc_abuse_count >= 1

results$year_alc_abuse_count  <- rowSums(tibble(year_alc_failure, year_alc_hazard, year_alc_continue, year_alc_legal), na.rm = T)
results$year_alc_abuse <- results$year_alc_abuse_count >= 1

# Alcohol dependence symptoms
life_alc_tolerance <- (phenx$PHXQ0013 == 1) | (phenx$PHXQ0015 == 1) | (phenx$PHXQ0017 == 1) | (phenx$PHXQ0019 == 1)
year_alc_tolerance <- life_alc_tolerance & ((phenx$PHXQ0014 == 1) | (phenx$PHXQ0016 == 1) | (phenx$PHXQ0018 == 1) | (phenx$PHXQ0020 == 1))

life_alc_quit <- (phenx$PHXQ0021 == 1) | (phenx$PHXQ0023 == 1)
year_alc_quit <- life_alc_quit & ((phenx$PHXQ0022 == 1) | (phenx$PHXQ0024 == 1))

life_alc_intend <- (phenx$PHXQ0025 == 1) | (phenx$PHXQ0027 == 1)
year_alc_intend <- life_alc_intend & ((phenx$PHXQ0026 == 1) | (phenx$PHXQ0028 == 1))

life_alc_withdrawal_count <- rowSums(tibble((phenx$PHXQ0031 == 1), (phenx$PHXQ0033 == 1), (phenx$PHXQ0035 == 1), (phenx$PHXQ0037 == 1), 
	(phenx$PHXQ0039 == 1), (phenx$PHXQ0041 == 1), (phenx$PHXQ0043 == 1), (phenx$PHXQ0045 == 1)), na.rm = T)
year_alc_withdrawal_count <- rowSums(tibble((phenx$PHXQ0031 == 1 & phenx$PHXQ0032 == 1), (phenx$PHXQ0033 == 1 & phenx$PHXQ0034 == 1), 
	(phenx$PHXQ0035 == 1 & phenx$PHXQ0036 == 1), (phenx$PHXQ0037 == 1 & phenx$PHXQ0038 == 1), 
	(phenx$PHXQ0039 == 1 & phenx$PHXQ0040 == 1), (phenx$PHXQ0041 == 1 & phenx$PHXQ0042 == 1), 
	(phenx$PHXQ0043 == 1 & phenx$PHXQ0044 == 1), (phenx$PHXQ0045 == 1 & phenx$PHXQ0046 == 1)), na.rm = T)

life_alc_withdrawal <- ((life_alc_withdrawal_count >= 2) & (life_alc_withdrawal_count <= 8)) |
	(phenx$PHXQ0050 == 1) | (phenx$PHXQ0052 == 1)
year_alc_withdrawal <- ((year_alc_withdrawal_count >= 2) & (year_alc_withdrawal_count <= 8)) |
	(phenx$PHXQ0050 == 1 & phenx$PHXQ0051 == 1) | (phenx$PHXQ0052 == 1 & phenx$PHXQ0053 == 1)

life_alc_time <- (phenx$PHXQ0054 == 1) | (phenx$PHXQ0056 == 1)
year_alc_time <- life_alc_time & ((phenx$PHXQ0055 == 1) | (phenx$PHXQ0057 == 1))

life_alc_giveup <- (phenx$PHXQ0058 == 1) | (phenx$PHXQ0060 == 1)
year_alc_giveup <- life_alc_giveup & ((phenx$PHXQ0059 == 1) | (phenx$PHXQ0061 == 1))

life_alc_problems <- (phenx$PHXQ0062 == 1) | (phenx$PHXQ0064 == 1) | (phenx$PHXQ0066 == 1)
year_alc_problems <- life_alc_problems & ((phenx$PHXQ0063 == 1) | (phenx$PHXQ0065 == 1) | (phenx$PHXQ0067 == 1))

# Alcohol dependence diagnoses and symptom counts
results$life_alc_depend_count <- rowSums(tibble(life_alc_tolerance, life_alc_quit, life_alc_intend, 
	life_alc_withdrawal, life_alc_time, life_alc_giveup, life_alc_problems), na.rm = T)
results$life_alc_depend <- results$life_alc_depend_count >= 3

results$year_alc_depend_count <- rowSums(tibble(year_alc_tolerance, year_alc_quit, year_alc_intend, 
	year_alc_withdrawal, year_alc_time, year_alc_giveup, year_alc_problems), na.rm = T)
results$year_alc_depend <- results$year_alc_depend_count >= 3

# Current cigarette dependence



# Tobacco dependence symptoms
life_tob_tolerance <- (phenx$PHXQ0151 == 1) | (phenx$PHXQ0153 == 1)
year_tob_tolerance <- life_tob_tolerance & ((phenx$PHXQ0152 == 1) | (phenx$PHXQ0154 == 1))

life_tob_withdrawal_count <- rowSums(tibble((phenx$PHXQ0127 == 1), (phenx$PHXQ0129 == 1), (phenx$PHXQ0131 == 1), (phenx$PHXQ0133 == 1), 
	(phenx$PHXQ0135 == 1), (phenx$PHXQ0137 == 1), (phenx$PHXQ0139 == 1), (phenx$PHXQ0141 == 1)), na.rm = T)
year_tob_withdrawal_count <- rowSums(tibble((phenx$PHXQ0127 == 1 & phenx$PHXQ0128 == 1), (phenx$PHXQ0129 == 1 & phenx$PHXQ0130 == 1), 
	(phenx$PHXQ0131 == 1 & phenx$PHXQ0132 == 1), (phenx$PHXQ0133 == 1 & phenx$PHXQ0134 == 1), 
	(phenx$PHXQ0135 == 1 & phenx$PHXQ0136 == 1), (phenx$PHXQ0137 == 1 & phenx$PHXQ0138 == 1), 
	(phenx$PHXQ0139 == 1 & phenx$PHXQ0140 == 1), (phenx$PHXQ0141 == 1 & phenx$PHXQ0142 == 1)), na.rm = T)

life_tob_withdrawal <- (life_tob_withdrawal_count > 3) | (phenx$PHXQ0144 == 1) | (phenx$PHXQ0145 == 1) | 
	(phenx$PHXQ0147 == 1) | (phenx$PHXQ0149 == 1)
year_tob_withdrawal <- (year_tob_withdrawal_count > 3) | (phenx$PHXQ0144 == 1) | (phenx$PHXQ0145 == 1 & phenx$PHXQ0146) | 
	(phenx$PHXQ0147 == 1 & phenx$PHXQ0148) | (phenx$PHXQ0149 == 1 & phenx$PHXQ0150)

life_tob_intend <- phenx$PHXQ0155 == 1
year_tob_intend <- life_tob_intend & (phenx$PHXQ0156 == 1)

life_tob_quit <- (phenx$PHXQ0113 == 1) | (phenx$PHXQ0123 == 1)
year_tob_quit <- life_tob_quit & ((phenx$PHXQ0114 == 1) | (phenx$PHXQ0124 == 1))

life_tob_giveup <- (phenx$PHXQ0115 == 1) | (phenx$PHXQ0117 == 1)
year_tob_giveup <- life_tob_giveup & ((phenx$PHXQ0116 == 1) | (phenx$PHXQ0118 == 1))

life_tob_time <- phenx$PHXQ0121 == 1
year_tob_time <- life_tob_time & (phenx$PHXQ0122 == 1)

life_tob_problems <- (phenx$PHXQ0119 == 1) | (phenx$PHXQ0157 == 1)
year_tob_problems <- life_tob_problems & ((phenx$PHXQ0120 == 1) | (phenx$PHXQ0158 == 1))

# Tobacco dependence diagnoses and symptom counts
results$life_tob_depend_count <- rowSums(tibble(life_tob_tolerance, life_tob_withdrawal, life_tob_intend, life_tob_quit,
	life_tob_giveup, life_tob_time, life_tob_problems), na.rm = T)
results$life_tob_depend <- results$life_tob_depend_count >= 3

results$year_tob_depend_count <- rowSums(tibble(year_tob_tolerance, year_tob_withdrawal, year_tob_intend, year_tob_quit,
	year_tob_giveup, year_tob_time, year_tob_problems), na.rm = T)
results$year_tob_depend <- results$year_tob_depend_count >= 3

# Write out the results
write_rds(results, 'data/processed/PhenX_diagnoses.rds')
