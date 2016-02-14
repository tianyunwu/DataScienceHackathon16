# read the diabetes drug list and clinical trial data from AACT clinical trial government website
nct_drugs <- read.csv("Diabetes_drugs_NCT_map.csv", header = T, sep = ",", stringsAsFactors = F)

# tabulate information from results_outcomes, a data file form a comprehensive clinical study data 
results_outcomes <- read.table("results_outcomes.txt",
                              header = TRUE,
                              sep = "|",
                              na.strings = "",
                              stringsAsFactors = FALSE,
                              comment.char = "",
                              quote = "",
                              fill = TRUE)

# intersect data from large clinical study databse based on trial ID (named as NCT_ID) 
diabetes_results_outcomes <- results_outcomes[results_outcomes$NCT_ID %in% nct_drugs$NCT_ID,]


# tabulate information from results_outcomes_measure, a data file form a comprehensive clinical study data 
results_outcome_measure <- read.table("results_outcome_measure.txt",
                                               header = TRUE,
                                               sep = "|",
                                               na.strings = "",
                                               stringsAsFactors = FALSE,
                                               comment.char = "",
                                               quote = "",
                                               fill = TRUE)
# intersect data from large clinical study databse and results_outcomes_measure based on trial ID (named as NCT_ID) 
diabetes_results_outcome_measure <- results_outcome_measure[results_outcome_measure$OUTCOME_ID %in% diabetes_results_outcomes$OUTCOME_ID,]


# tabulate information from results_outcomes_measure, a data file form a comprehensive clinical study data 
results_outcome_measure_ctgy <- read.table("results_outcome_measure_ctgy.txt",
                                  header = TRUE,
                                  sep = "|",
                                  na.strings = "",
                                  stringsAsFactors = FALSE,
                                  comment.char = "",
                                  quote = "",
                                  fill = TRUE)
# intersect data from large clinical study databse and results_outcomes_measure_catgy based on trial ID (named as NCT_ID) 
diabetes_results_outcome_measure_ctgy <- results_outcome_measure_ctgy[results_outcome_measure_ctgy$OUTCOME_MEASURE_ID %in% diabetes_results_outcome_measure$OUTCOME_MEASURE_ID,]


# tabulate information from results_outcomes_measure, a data file form a comprehensive clinical study data 
results_outcomes_full_file <- read.table("clinical_study_noclob_nolf.txt",
                                         header = TRUE,
                                         sep = "|",
                                         na.strings = "",
                                         stringsAsFactors = FALSE,
                                         comment.char = "",
                                         quote = "",
                                         fill = TRUE)
# intersect data from large clinical study databse and clinical_study_noclob_nolf based on trial ID (named as NCT_ID) 
diabetes_results_outcomes_full_file <- results_outcomes_full_file[results_outcomes_full_file$NCT_ID %in% nct_drugs$NCT_ID,]


# append collected data to create a comprehensive table
names_to_leave <- c(names(nct_drugs), "OUTCOME_TITLE","OUTCOME_VALUE", "OUTCOME_DESCRIPTION","SAFETY_ISSUE", "CATEGORY_TITLE")
nct_drugs_results_outcome <- merge(nct_drugs, diabetes_results_outcomes, by = "NCT_ID")
nct_drugs_results_outcome <- merge(nct_drugs_results_outcome, diabetes_results_outcome_measure, by = "OUTCOME_ID")
nct_drugs_results_outcome <- merge(nct_drugs_results_outcome, diabetes_results_outcome_measure_ctgy, by = "OUTCOME_MEASURE_ID")
nct_drugs_results_outcome <- nct_drugs_results_outcome[,names_to_leave]

# write a rds file with collected data
write.rds(nct_drugs_results_outcome, file = "nct_drugs_results_outcome.")
