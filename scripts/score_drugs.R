# Read in Data
diabetes_classes=read.csv("Diabetes_drugs_summary.csv",stringsAsFactors=FALSE)
efficacy=readRDS("nct_drugs_results_outcome_hbA1c_mg_per_dL.rds")
cost=read.csv("Diabetes_Generic_Brand_price.csv",stringsAsFactors=FALSE)
interactions=readRDS("diabetes_DDIs.rds")
adverse_events_weights=read.csv("adverse_weights.csv")
patients=read.csv("patient_profiles.csv",stringsAsFactors=FALSE)
indication_multiplier=read.csv("indicator_multiplier.csv")
all_adverse=readRDS("avg_adverse_effects.rds")

# Calculate patient specific multipliers
patient_drug_map=list(P1=P1_drugs,P2=P2_drugs,P3=P3_drugs,P4=P4_drugs)
P1_drugs=unique(unlist(strsplit(patients$Other_Drugs[1],";")))
P2_drugs=unique(unlist(strsplit(patients$Other_Drugs[2],";")))
P3_drugs=unique(unlist(strsplit(patients$Other_Drugs[3],";")))
P4_drugs=unique(unlist(strsplit(patients$Other_Drugs[4],";")))
P1_indication_multipliers= sapply(unlist(strsplit(as.vector(patients$Conditions[1]),";")),function(x) indication_multiplier$multiplier[indication_multiplier$indication==x])
P2_indication_multipliers= sapply(unlist(strsplit(as.vector(patients$Conditions[2]),";")),function(x) indication_multiplier$multiplier[indication_multiplier$indication==x])
P3_indication_multipliers= sapply(unlist(strsplit(as.vector(patients$Conditions[3]),";")),function(x) indication_multiplier$multiplier[indication_multiplier$indication==x])
P4_indication_multipliers= sapply(unlist(strsplit(as.vector(patients$Conditions[4]),";")),function(x) indication_multiplier$multiplier[indication_multiplier$indication==x])

P1_multipliers=c()
for(i in 1:nrow(adverse_events_weights)){
  ae=as.vector(adverse_events_weights$CATEGORY_TITLE)[i]
  overlap_inds=which(names(P1_indication_multipliers) %in% intersect(unlist(strsplit(as.vector(adverse_events_weights$matched_indication[i]),";")),names(P1_indication_multipliers)))
  if(length(overlap_inds)>0){
    P1_multipliers=c(P1_multipliers,max(P1_indication_multipliers[overlap_inds]))    
  }else{
    P1_multipliers=c(P1_multipliers,1)
  }
  names(P1_multipliers)[i]=ae
}
P2_multipliers=c()
for(i in 1:nrow(adverse_events_weights)){
  ae=as.vector(adverse_events_weights$CATEGORY_TITLE)[i]
  overlap_inds=which(names(P2_indication_multipliers) %in% intersect(unlist(strsplit(as.vector(adverse_events_weights$matched_indication[i]),";")),names(P2_indication_multipliers)))
  if(length(overlap_inds)>0){
    P2_multipliers=c(P2_multipliers,max(P2_indication_multipliers[overlap_inds]))    
  }else{
    P2_multipliers=c(P2_multipliers,1)
  }
  names(P2_multipliers)[i]=ae
}
P3_multipliers=c()
for(i in 1:nrow(adverse_events_weights)){
  ae=as.vector(adverse_events_weights$CATEGORY_TITLE)[i]
  overlap_inds=which(names(P3_indication_multipliers) %in% intersect(unlist(strsplit(as.vector(adverse_events_weights$matched_indication[i]),";")),names(P3_indication_multipliers)))
  if(length(overlap_inds)>0){
    P3_multipliers=c(P3_multipliers,max(P3_indication_multipliers[overlap_inds]))    
  }else{
    P3_multipliers=c(P3_multipliers,1)
  }
  names(P3_multipliers)[i]=ae
}
P4_multipliers=c()
for(i in 1:nrow(adverse_events_weights)){
  ae=as.vector(adverse_events_weights$CATEGORY_TITLE)[i]
  overlap_inds=which(names(P4_indication_multipliers) %in% intersect(unlist(strsplit(as.vector(adverse_events_weights$matched_indication[i]),";")),names(P4_indication_multipliers)))
  if(length(overlap_inds)>0){
    P4_multipliers=c(P4_multipliers,max(P4_indication_multipliers[overlap_inds]))    
  }else{
    P4_multipliers=c(P4_multipliers,1)
  }
  names(P4_multipliers)[i]=ae
}

# Calculate Patient Scores
patient_drug_scores=data.table()
for(drug in unique(all_adverse$drug)){
  cat(drug)
  aes_score=(all_adverse$avg_affected[all_adverse$drug==drug])
  keep.inds=which(!is.na(aes_score))
  aes_score=(all_adverse$avg_affected[all_adverse$drug==drug])[keep.inds]
  aes=(all_adverse$CATEGORY_TITLE[all_adverse$drug==drug])[keep.inds]
  aes_serious=(all_adverse$EVENT_TYPE[all_adverse$drug==drug])[keep.inds]
  general_weights=p1_weights=p2_weights=p3_weights=p4_weights=c()
  serious_weights=c()
  for(i in 1:length(aes)){
    if(aes[i] %in% adverse_events_weights$CATEGORY_TITLE){
      seriousness=aes_serious[i]
      if(seriousness=="Serious"){
        mult=2
        serious_weights=c(serious_weights,1)
      }else{
        mult=1
        serious_weights=c(serious_weights,0)
      }
      general_weights=c(general_weights,mult*adverse_events_weights$weights_scalar[adverse_events_weights$CATEGORY_TITLE==aes[i]])
    }else{
      general_weights=c(general_weights,0)
      serious_weights=c(serious_weights,0)
    }
  }
  nonserious_weights=1-serious_weights
  general_score=sum((general_weights/sum(general_weights) )*t(aes_score))*1000
  p1_mult=sapply(aes,function(x){val=P1_multipliers[names(P1_multipliers)==x];if(length(val)==0){val=1};return(val)})
  p2_mult=sapply(aes,function(x){val=P2_multipliers[names(P2_multipliers)==x];if(length(val)==0){val=1};return(val)})
  p3_mult=sapply(aes,function(x){val=P3_multipliers[names(P3_multipliers)==x];if(length(val)==0){val=1};return(val)})
  p4_mult=sapply(aes,function(x){val=P4_multipliers[names(P4_multipliers)==x];if(length(val)==0){val=1};return(val)})
  p1_score=sum(((general_weights*t(as.numeric(p1_mult)))/sum((general_weights*t(as.numeric(p1_mult)))))*t(aes_score))*1000
  p2_score=sum(((general_weights*t(as.numeric(p2_mult)))/sum((general_weights*t(as.numeric(p2_mult)))))*t(aes_score))*1000
  p3_score=sum(((general_weights*t(as.numeric(p3_mult)))/sum((general_weights*t(as.numeric(p3_mult)))))*t(aes_score))*1000
  p4_score=sum(((general_weights*t(as.numeric(p4_mult)))/sum((general_weights*t(as.numeric(p4_mult)))))*t(aes_score))*1000
  
  p1_serious_score=(general_weights*t(as.numeric(p1_mult)))*t(serious_weights);p1_serious_score=sum(p1_serious_score/sum(p1_serious_score)*t(aes_score))*1000
  p2_serious_score=(general_weights*t(as.numeric(p2_mult)))*t(serious_weights);p2_serious_score=sum(p2_serious_score/sum(p2_serious_score)*t(aes_score))*1000
  p3_serious_score=(general_weights*t(as.numeric(p3_mult)))*t(serious_weights);p3_serious_score=sum(p3_serious_score/sum(p3_serious_score)*t(aes_score))*1000
  p4_serious_score=(general_weights*t(as.numeric(p4_mult)))*t(serious_weights);p4_serious_score=sum(p4_serious_score/sum(p4_serious_score)*t(aes_score))*1000

  p1_nonserious_score=(general_weights*t(as.numeric(p1_mult)))*t(nonserious_weights);p1_nonserious_score=sum(p1_nonserious_score/sum(p1_nonserious_score)*t(aes_score))*1000
  p2_nonserious_score=(general_weights*t(as.numeric(p2_mult)))*t(nonserious_weights);p2_nonserious_score=sum(p2_nonserious_score/sum(p2_nonserious_score)*t(aes_score))*1000
  p3_nonserious_score=(general_weights*t(as.numeric(p3_mult)))*t(nonserious_weights);p3_nonserious_score=sum(p3_nonserious_score/sum(p3_nonserious_score)*t(aes_score))*1000
  p4_nonserious_score=(general_weights*t(as.numeric(p4_mult)))*t(nonserious_weights);p4_nonserious_score=sum(p4_nonserious_score/sum(p4_nonserious_score)*t(aes_score))*1000
  
  drug_scores=rbind(drug_scores,data.table(Drug=drug,General=general_score,P1=p1_score,P2=p2_score,P3=p3_score,P4=p4_score))
  patient_drug_scores=rbind(patient_drug_scores,data.table(Drug=drug,Patient="P1",AE_score=p1_score,seriousAE_score=p1_serious_score,nonseriousAE_score=p1_nonserious_score))
  patient_drug_scores=rbind(patient_drug_scores,data.table(Drug=drug,Patient="P2",AE_score=p2_score,seriousAE_score=p2_serious_score,nonseriousAE_score=p2_nonserious_score))
  patient_drug_scores=rbind(patient_drug_scores,data.table(Drug=drug,Patient="P3",AE_score=p3_score,seriousAE_score=p3_serious_score,nonseriousAE_score=p3_nonserious_score))
  patient_drug_scores=rbind(patient_drug_scores,data.table(Drug=drug,Patient="P4",AE_score=p4_score,seriousAE_score=p4_serious_score,nonseriousAE_score=p4_nonserious_score))
}

range0_10 <- function(x){10*(x-min(x))/(max(x)-min(x))};
range1_10 <- function(x){
  res=x
  res2=res[!is.nan(res)]
  res[!is.nan(res)]=9*((res2-min(res2))/(max(res2)-min(res2)))+1
  return(res)
};

diabetes_classes=cbind(diabetes_classes,drugname=toupper(gsub("[ ]","",gsub("[^[:alnum:] ]", "",diabetes_classes$Generic_name))))
diabetes_classes=cbind(diabetes_classes,drugname=toupper(gsub("[ ]","",gsub("[^[:alnum:] ]", "",diabetes_classes$Generic_name))))
cost$NADAC[cost$NADAC=="discontinued"]=NaN
drug_scores2=data.table(Drug=drug_scores$Drug,Diabetes_Type=sapply(unique(drug_scores2$Drug),function(x) unique(diabetes_classes$Class[diabetes_classes$drugname==x])),General_Score=range1_10(drug_scores$General),P1_score=range1_10(drug_scores$P1),P2_score=range1_10(drug_scores$P2),P3_score=range1_10(drug_scores$P3),P4_score=range1_10(drug_scores$P4))
patient_drug_scores2=data.table(Drug=patient_drug_scores$Drug,Diabetes_Type=sapply(patient_drug_scores$Drug,function(x) unique(diabetes_classes$Class[diabetes_classes$drugname==x])),AE_score=range1_10(patient_drug_scores$AE_score),seriousAE_score=range1_10(patient_drug_scores$seriousAE_score),nonseriousAE_score=range1_10(patient_drug_scores$nonseriousAE_score))

efficacy=cbind(efficacy,OUTCOME_VALUE10=range0_10(as.numeric(efficacy$OUTCOME_VALUE)))
generic_efficacy2=generic_efficacy=sapply(cost$Generic,function(x) mean(as.numeric(efficacy$OUTCOME_VALUE[efficacy$drug==x]),na.rm=T))
generic_efficacy2[!is.nan(generic_efficacy)]=range1_10(generic_efficacy[!is.nan(generic_efficacy)])

patient_drug_scores3=data.table()
for(i in 1:nrow(patient_drug_scores)){
  x=patient_drug_scores$Drug[i]
  brands=cost$Brand[cost$Generic==x]
  patient_id=patient_drug_scores$Patient[i]
  patient_drugs=patient_drug_map[[as.numeric(gsub("P","",patient_id))]]
  interacting_drugs=interactions$drug[interactions$intr_drug %in% patient_drugs]
  if(x %in% interacting_drugs){interaction_name=paste0("Interacts with ",paste(gsub("(^[[:alpha:]])", "\\U\\1", unique(tolower(intersect(interactions$intr_drug[interactions$drug==x],patient_drugs))),perl=TRUE),collapse=" and "));interaction_score=0}else{interaction_name="none";interaction_score=1}
  if(length(brands)>1){
    for(j in 1:length(brands)){
      patient_drug_scores3=rbind(patient_drug_scores3,data.table(Patient=patient_drug_scores$Patient[i],Diabetes_Type=unique(diabetes_classes$Class[diabetes_classes$drugname==x]),Generic=gsub("(^[[:alpha:]])", "\\U\\1", tolower(x)),Brand=(cost$Brand[cost$Generic==x])[j],drug_interaction=interaction_score,drug_interaction_name=interaction_name,AE_score=range1_10(patient_drug_scores$AE_score)[i],seriousAE_score=range1_10(patient_drug_scores$seriousAE_score)[i],nonseriousAE_score=range1_10(patient_drug_scores$nonseriousAE_score)[i],efficacy=unique(generic_efficacy2[names(generic_efficacy2)==x]),NADAC=unique(cost$NADAC[cost$Generic==x])[j],norm_NADAC=(range1_10(as.numeric(cost$NADAC))[cost$Generic==x])[j]))
    }    
  }else{
    patient_drug_scores3=rbind(patient_drug_scores3,data.table(Patient=patient_drug_scores$Patient[i],Diabetes_Type=unique(diabetes_classes$Class[diabetes_classes$drugname==x]),Generic=gsub("(^[[:alpha:]])", "\\U\\1", tolower(x)),Brand=cost$Brand[cost$Generic==x],drug_interaction=interaction_score,drug_interaction_name=interaction_name,AE_score=range1_10(patient_drug_scores$AE_score)[i],seriousAE_score=range1_10(patient_drug_scores$seriousAE_score)[i],nonseriousAE_score=range1_10(patient_drug_scores$nonseriousAE_score)[i],efficacy=unique(generic_efficacy2[names(generic_efficacy2)==x]),NADAC=unique(cost$NADAC[cost$Generic==x]),norm_NADAC=range1_10(as.numeric(cost$NADAC))[cost$Generic==x]))    
  }
  
}
(!is.nan(patient_drug_scores3$NADAC)
patient_drug_scores3=(patient_drug_scores3[(!is.nan(patient_drug_scores3$efficacy)),])[1:84,]
patient_drug_scores4=cbind(patient_drug_scores3,data.table(DROICE_score=sapply(1:nrow(patient_drug_scores3),function(i) patient_drug_scores3$drug_interaction[i]*(0.45*patient_drug_scores3$efficacy[i]+0.35*patient_drug_scores3$AE_score[i]+0.2*patient_drug_scores3$norm_NADAC[i]))))

write.table(patient_drug_scores4,file="DROICE_score_table.txt",row.names=FALSE,quote=FALSE,sep="\t")
write.csv(patient_drug_scores4,file="DROICE_score_table.csv",row.names=FALSE,quote=FALSE)

drug_scores3=data.table(Diabetes_Type=cost$Indication,Brand=cost$Brand,Generic=gsub("(^[[:alpha:]])", "\\U\\1", tolower(cost$Generic), perl=TRUE),General_Score=10-sapply(cost$Generic,function(x) drug_scores2$General_Score[drug_scores2$Drug==x]),P1_score=10-sapply(cost$Generic,function(x) drug_scores2$P1_score[drug_scores2$Drug==x]),P2_score=10-sapply(cost$Generic,function(x) drug_scores2$P2_score[drug_scores2$Drug==x]),P3_score=10-sapply(cost$Generic,function(x) drug_scores2$P3_score[drug_scores2$Drug==x]),P4_score=10-sapply(cost$Generic,function(x) drug_scores2$P4_score[drug_scores2$Drug==x]),efficacy=generic_efficacy2,NADAC=cost$NADAC)
drug_scores3=drug_scores3[!is.nan(drug_scores3$efficacy),]

