rm(list = ls(all = TRUE))
library(data.table)

indication_files=list.files(dir)
indications=gsub("[_]"," ",gsub("[.]txt","",indication_files))

drug_indication_lookup=data.table()
for(file in indication_files){
  this_drugs=toupper(gsub("[ ]","",gsub("[^[:alnum:] ]", "",read.table(paste0(dir,file),stringsAsFactors = FALSE,sep="\t")[,1])))
  drug_indication_lookup=rbind(drug_indication_lookup,data.table(drug=this_drugs,indication=gsub("[_]"," ",gsub("[.]txt","",file))))  
}
#save(drug_indication_lookup,file="~/Dropbox/DrugRank/drug_indications.rds")

interactions_df2=readRDS("~/Dropbox/DrugRank/interaction_table_redo2.rds")
interactions_df2$intr_drug=toupper(gsub("[ ]","",gsub("[^[:alnum:] ]", "",interactions_df2$intr_drug)))

interactions_df3=data.table()
for(i in 1:nrow(interactions_df2)){
  intr_drug=interactions_df2$intr_drug[i]
  this_indication=drug_indication_lookup$indication[drug_indication_lookup$drug==intr_drug]
  if(length(this_indication)>0){
    interactions_df3=rbind(interactions_df3,data.table(cbind(interactions_df2[i,],indication=this_indication)))
  }else{
    interactions_df3=rbind(interactions_df3,data.table(cbind(interactions_df2[i,],indication="Unknown")))    
  }
}

saveRDS(interactions_df3,"~/Dropbox/DrugRank/diabetes_DDIs.rds")

interactions_df3$drug[!duplicated(paste0(interactions_df3$drug,interactions_df3$indication))]
count(interactions_df3$drug[!duplicated(paste0(interactions_df3$drug,interactions_df3$indication))])

