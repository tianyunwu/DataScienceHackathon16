nct_drugs <- read.csv("Diabetes_drugs_NDC_NCT_map.csv", header = T, sep = ",", stringsAsFactors = F)

reported_events <- read.table("reported_events.txt",
                              header = TRUE,
                              sep = "|",
                              na.strings = "",
                              stringsAsFactors = FALSE,
                              comment.char = "",
                              quote = "",
                              fill = TRUE)

diabetes_rep_events <- reported_events[reported_events$NCT_ID %in% nct_drugs$NCT_ID,]

reported_event_ctgy <- read.table("reported_event_ctgy.txt",
                                  header = TRUE,
                                  sep = "|",
                                  na.strings = "",
                                  stringsAsFactors = FALSE,
                                  comment.char = "",
                                  quote = "",
                                  fill = TRUE)

diabetes_rep_event_ctgy <- reported_event_ctgy[reported_event_ctgy$REPORTED_EVENT_ID %in% diabetes_rep_events$REPORTED_EVENT_ID,]

reported_event_ctgy_grp <- read.table("reported_event_ctgy_grp.txt",
                                      header = TRUE,
                                      sep = "|",
                                      na.strings = "",
                                      stringsAsFactors = FALSE,
                                      comment.char = "",
                                      quote = "",
                                      fill = TRUE)

diabetes_rep_event_ctgy_grp <- reported_event_ctgy_grp[reported_event_ctgy_grp$REPORTED_EVENT_CATEGORY_ID %in% diabetes_rep_event_ctgy$REPORTED_EVENT_CATEGORY_ID,]

names_to_leave <- c(names(nct_drugs), 
                    "EVENT_TYPE", "TIME_FRAME", "DESCRIPTION", 
                    "CATEGORY_TITLE", "CATEGORY_SUB_TITLE", 
                    "SUBJECTS_AFFECTED", "SUBJECTS_AT_RISK")
nct_drugs_rep_events <- merge(nct_drugs, diabetes_rep_events, by = "NCT_ID")
nct_drugs_rep_events <- merge(nct_drugs_rep_events, diabetes_rep_event_ctgy, by = "REPORTED_EVENT_ID")
nct_drugs_rep_events <- merge(nct_drugs_rep_events, diabetes_rep_event_ctgy_grp, by = "REPORTED_EVENT_CATEGORY_ID")
nct_drugs_rep_events <- nct_drugs_rep_events[,names_to_leave]

nct_drugs_rep_events$AFFECTED_PRCNT <- round(nct_drugs_rep_events$SUBJECTS_AFFECTED / nct_drugs_rep_events$SUBJECTS_AT_RISK, 2)