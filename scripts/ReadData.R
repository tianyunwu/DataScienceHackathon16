setwd("~/Desktop/AACT201509_pipe_delimited_txt")

# Look at Clinical Study files, which includes patient population demographics
fileName = "clinical_study.txt";
# Delete embeded soft returns
fileTransName = paste(unlist(strsplit(fileName,split='.',fixed = TRUE))[1],"_nolf.txt",sep="");
command = paste("tr -d '\012' <",fileName,"> ",fileTransName,sep="")
fileTrans = try(system(command))
# Count the number of lines in the file
command = paste("wc -l < ~/Desktop/AACT201509_pipe_delimited_txt/",fileName,sep="")
lineNumber = try(system(command))
# Read txt data file
study <- read.table(file=fileTransName,
header=TRUE,
sep="|",
na.strings="",
comment.char = "",
colClasses = classes,
quote="",
fill=TRUE,
nrows=lineNumber);
View(study);
# Read clinical_study.txt
study=read.table(file=fileTransName,sep="|",header=TRUE,fill=TRUE,stringsAsFactor=FALSE,quote="",na.strings="",
                 comment.char = "")
# check the length of clinical trial ID, should be 11 characters long
library(stringi)
length = sapply(study$NCT_ID,stri_length,USE.NAMES = FALSE)
which(is.na(length))
# remove the missing data
which(length>11)
# remove the NCT_IDs that are more than 11 characters
# import the latest list of clinial trials that we are interested in
Diabetes_drugs_NDC_NCT_map <- read.csv("~/Desktop/AACT201509_pipe_delimited_txt/Diabetes_drugs_NDC_NCT_map.csv", stringsAsFactors=FALSE)
NCT_list=unique(Diabetes_drugs_NDC_NCT_map$NCT_ID)
# subset clinical study data based on clinical trial list
study = subset(study,study$NCT_ID %in% NCT_list)
# save a narrower list of demographic information
demog = subset(study,select=c("NCT_ID","OVERALL_STATUS","PHASE","GENDER","MINIMUM_AGE","MAXIMUM_AGE","CRITERIA"))
write.csv(demog,file="demographic.csv",row.names = FALSE)

# Tried to calculate the length of clinical trial, not successful
# as.Date(study$FIRSTRECEIVED_DATE,"%b %d,%Y")
# this function does not work
# time = sapply(study,as.Date(study$LASTCHANGED_DATE,"%b %d,%Y")-as.Date(study$FIRSTRECEIVED_DATE,"%b %d,%Y"),USE.NAMES = FALSE)

# Read study outcome
fileName = "study_outcome.txt";
fileTransName = paste(unlist(strsplit(fileName,split='.',fixed = TRUE))[1],"_nolf.txt",sep="");
command = paste("tr -d '\012' <",fileName,"> ",fileTransName,sep="")
fileTrans = try(system(command))
command = paste("wc -l < ~/Desktop/AACT201509_pipe_delimited_txt/",fileName,sep="")
lineNumber = try(system(command))
study_outcome = read.table(file=fileTransName, header = TRUE, sep = "|", quote = "",
dec = ".", fill = TRUE, comment.char = "",na.strings="")
# Filter out the rows that incorrectedly read, i.e. NCT_ID is not at the correct length
length = sapply(study_outcome$NCT_ID,stri_length,USE.NAMES = FALSE)
which(is.na(length))
which(length>11)
study_outcome=study_outcome[c(-169144, -285922, -286468, -634851, -647716, -746911, -813455, -286400, -286402, -336708, -388709, -388711, -408894, -408909, -409133, -412667, -449219, -461619, -461638, -492312, -515939, -515941, -776764, -842944, -842946, -843194),]
# check how many study outcome types there are
outcome_type=unique(study_outcome$STUDY_OUTCOMES_TYPE)
# only select primary and secondary outcomes
study_outcome_reduced=subset(study_outcome,study_outcome$STUDY_OUTCOMES_TYPE=="primary outcome" | study_outcome$STUDY_OUTCOMES_TYPE=="secondary outcome")
write.csv(study_outcome_reduced,file="study_outcome2.csv",row.names=FALSE)
# construct wordcloud based on study outcomes
library(tm)
library(SnowballC)
library(wordcloud)
# reduce the study outcome table to only two rows, clinical trial ID and detailed description of measure
outcome_measure=study_outcome_reduced[,c("NCT_ID","MEASURE")]
list=readRDS("Diabetes_mapped_trial_ids.rds")
# match our clnical trial of interest to the study outcome
outcome_measure_reduced=subset(outcome_measure,outcome_measure$NCT_ID %in% list)
# group the "measure" column by the clinical trial ID
outcome_measure_combined = aggregate(outcome_measure_reduced$MEASURE~outcome_measure_reduced$NCT_ID,FUN=paste,collapse=" ")
names(outcome_measure_combined)<-c("NCT_ID","MEASURE")
measure_text = paste(outcome_measure_combined$MEASURE,sep = " ")
corpus = Corpus(VectorSource(measure_text))
corpus = tm_map(corpus,content_transformer(tolower))
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeNumbers)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords("english"))
# document term matrix and corresponding sorted frequent word list
dtm = DocumentTermMatrix(corpus)
dtm2=as.matrix(dtm)
frequency=colSums(dtm2)
frequency=sort(frequency,decreasing=TRUE)
head(frequency)
words=names(frequency)
# make the cloud!
wordcloud(words[1:100],frequency[1:100])

