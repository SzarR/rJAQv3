qualitycontrol_step1 <- function(datum, section) {

   #FLAG For I in scale_choices...
  if(section == 'task'){

     if("IMP" %in% Scale_Choices){

        QC_Analysis_Tasks_IMP <- ifelse(datum[,paste0("NA_",1:TaskNumbers)] == 1, 1,
                                 ifelse(datum[,paste0("NA_",1:TaskNumbers)] == 2
                                 & is.na(datum[,paste0("IMP_",1:TaskNumbers)]),1,0))

        QC_Replacements_Tasks_IMP <- which(QC_Analysis_Tasks_IMP == 0, arr.ind=TRUE)
        datum[,paste0("IMP_",1:TaskNumbers)] <- replace(x = datum[,paste0("IMP_", 1:TaskNumbers)],list = QC_Replacements_Tasks_IMP, values = NA)

     }

     if("FREQ" %in% Scale_Choices){

        QC_Analysis_Tasks_FREQ <- ifelse(datum[,paste0("NA_",1:TaskNumbers)] == 1, 1,
                                  ifelse(datum[,paste0("NA_",1:TaskNumbers)] == 2
                                  & is.na(datum[,paste0("FREQ_",1:TaskNumbers)]),1,0))

        QC_Replacements_Tasks_FREQ <- which(QC_Analysis_Tasks_FREQ == 0, arr.ind=TRUE)
        datum[,paste0("FREQ_",1:TaskNumbers)] <- replace(x = datum[,paste0("FREQ_", 1:TaskNumbers)],list = QC_Replacements_Tasks_FREQ, values = NA)

     }

     if("REQU" %in% Scale_Choices){

        QC_Analysis_Tasks_REQU <- ifelse(datum[,paste0("NA_",1:TaskNumbers)] == 1, 1,
                                  ifelse(datum[,paste0("NA_",1:TaskNumbers)] == 2
                                  & is.na(datum[,paste0("REQU_",1:TaskNumbers)]),1,0))

        QC_Replacements_Tasks_REQU <- which(QC_Analysis_Tasks_REQU == 0, arr.ind=TRUE)
        datum[,paste0("REQU_",1:TaskNumbers)] <- replace(x = datum[,paste0("REQU_", 1:TaskNumbers)],list = QC_Replacements_Tasks_REQU, values = NA)
     }

  }

   if(section == 'ksao'){

      if("IMP" %in% Scale_Choices_ksao){
      QC_Analysis_KSA_IMP <- ifelse(datum[,paste0("NA_",1:KSAONumbers)] == 1, 1,
                             ifelse(datum[,paste0("NA_",1:KSAONumbers)] == 2
                             & is.na(datum[,paste0("IMP_",1:KSAONumbers)]),1,0))

      QC_Replacements_KSA_IMP <- which(QC_Analysis_KSA_IMP == 0, arr.ind=TRUE)
      datum[,paste0("IMP_",1:KSAONumbers)] <- replace(x = datum[,paste0("IMP_", 1:KSAONumbers)],list = QC_Replacements_KSA_IMP, values = NA)

      }

      if("REQU" %in% Scale_Choices_ksao){

         QC_Analysis_KSA_REQU <- ifelse(datum[,paste0("NA_",1:KSAONumbers)] == 1, 1,
                                ifelse(datum[,paste0("NA_",1:KSAONumbers)] == 2
                                & is.na(datum[,paste0("REQU_",1:KSAONumbers)]),1,0))

         QC_Replacements_KSA_REQU <- which(QC_Analysis_KSA_REQU == 0, arr.ind=TRUE)
         datum[,paste0("REQU_",1:KSAONumbers)] <- replace(x = datum[,paste0("REQU_", 1:KSAONumbers)],list = QC_Replacements_KSA_REQU, values = NA)

      }

      if("DIFF" %in% Scale_Choices_ksao){
         
         QC_Analysis_KSA_DIFF <- ifelse(datum[,paste0("NA_",1:KSAONumbers)] == 1, 1,
                                 ifelse(datum[,paste0("NA_",1:KSAONumbers)] == 2
                                 & is.na(datum[,paste0("DIFF_",1:KSAONumbers)]),1,0))

         QC_Replacements_KSA_DIFF <- which(QC_Analysis_KSA_DIFF == 0, arr.ind=TRUE)
         datum[,paste0("DIFF_",1:KSAONumbers)] <- replace(x = datum[,paste0("DIFF_", 1:KSAONumbers)],list = QC_Replacements_KSA_DIFF, values = NA)

      }
      
      if("RvR" %in% Scale_Choices_ksao){

         QC_Analysis_KSA_RvR <<- ifelse(datum[,paste0("NA_",(SAONumbers+1):KSAONumbers)] == 1, 1,
                                ifelse(datum[,paste0("NA_",(SAONumbers+1):KSAONumbers)] == 2
                                & is.na(datum[,paste0("RvR_",(SAONumbers+1):KSAONumbers)]),1,0))

         QC_Replacements_KSA_RvR <<- which(QC_Analysis_KSA_RvR == 0, arr.ind=TRUE)
         datum[,paste0("RvR_",(SAONumbers+1):KSAONumbers)] <- replace(x = datum[,paste0("RvR_",(SAONumbers+1):KSAONumbers)],list = QC_Replacements_KSA_RvR, values = NA)
      }

   }
  return(datum) 
}