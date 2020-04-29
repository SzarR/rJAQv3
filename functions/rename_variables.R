rename_variables <- function(datum, section){
  
  # This function takes a user settable first variable name
  # and last variable name, and counts the number of scales
  # that is also user settable, and then automatically
  # renames all the variables between those two data points
  # in the df. 

  if (section == 'task') {
    
    if(rename_begin %in% colnames(datum) & rename_end %in% colnames(datum)) {
      
    First_Location <- which(colnames(datum) == rename_begin)
    Last_Location <- which(colnames(datum) == rename_end)

    if(((Last_Location - First_Location + 1) %% length(Scale_Choices)) != 0) {
  
      make_alert(title = "Error!", 
                 text = "Either your column variable limits or scale choices are incorrect!",
                 type = 'error')

    } else {
    
    TaskNumbers <<- ((Last_Location - First_Location + 1) / length(Scale_Choices))

    #Run the renaming.
    NewVariable_Names_Task <<- as.vector(outer(Scale_Choices, 1:TaskNumbers, paste0))
    colnames(datum)[First_Location:Last_Location] <- NewVariable_Names_Task

    }
    } else {
      make_alert(title = "Error!", 
                 text = "Specified column names not found in task analysis file!",
                 type = 'error')
}
  }

  if(section == 'ksao'){
    
    # Declarations for getting value labels later.
    Checker_1 <<- FALSE # SAO Execute?
    Checker_2 <<- FALSE # Knowledge Execute?
    SAONumbers <<- 0
    KnowNumbers <<- 0

    # Create a seperate file where we can remove RvR from for first
    # batch of analyses excluding RvR statements.
    KSAOStatementNames <- Scale_Choices_ksao
    
    if(rename_begin_sao %in% colnames(datum) & rename_end_sao %in% colnames(datum)) {
      
      # Exclude RvR From the first analysis.
      if("RvR_" %in% Scale_Choices_ksao){
        KSAOStatementNames <- Scale_Choices_ksao[!Scale_Choices_ksao %in% 'RvR_']
      }
      
      First_Location  <- which(colnames(datum) == rename_begin_sao)
      Last_Location   <- which(colnames(datum) == rename_end_sao)

      if(((Last_Location - First_Location + 1) %% length(KSAOStatementNames)) != 0) {
        
        make_alert(title = "Error!", 
                   text = "Either your column variable limits or scale choices are incorrect!
                 Reset this app and correct the issue.",
                   type = 'error')

      } else {

        Checker_1 <<- TRUE
        SAONumbers <<- (Last_Location - First_Location + 1) / length(KSAOStatementNames)
        KSAONumbers <<- SAONumbers

        #Run the renaming.
        NewVariable_Names <- as.vector(outer(KSAOStatementNames,1:SAONumbers, paste0))
        colnames(datum)[First_Location:Last_Location] <- NewVariable_Names

      }
    }

    if(rename_begin_know %in% colnames(datum) & rename_end_know %in% colnames(datum)) {

      # After we run the standard scales excluding RvR, we then do RvR.
      if("RvR_" %in% Scale_Choices_ksao){

        KSAOStatementNames <- Scale_Choices_ksao

        #Knowledge Renamer
        First_Know_Location  <<- which(colnames(datum) == rename_begin_know)
        Last_Know_Location <<- which(colnames(datum) == rename_end_know)

        if(((Last_Know_Location - First_Know_Location + 1) %% length(Scale_Choices_ksao)) != 0) {

          make_alert(title = "Error!", 
                     text = "Either your column variable limits or scale choices are incorrect!.
                   Reset this app and correct the issue.",
                     type = 'error')

        } else {

          Checker_2 <<- TRUE
          KnowNumbers <<- (Last_Know_Location - First_Know_Location + 1) / length(Scale_Choices_ksao)
          KSAONumbers <<- KnowNumbers + SAONumbers

          Know_NewVariable_Names <- as.vector(outer(Scale_Choices_ksao, (SAONumbers+1):(SAONumbers + KnowNumbers),paste0))
          colnames(datum)[First_Know_Location:Last_Know_Location] <- Know_NewVariable_Names
        }
      }
    }
  }

  if(section == 'link'){
    
    if(!exists("DutyAreaCount")) {
      
      make_alert(title = "Error!", 
                 text = "No duty area weights detected. Please correct and re-run.",
                 type = 'error')
      
    } else {
      
    if(rename_begin_link_sao != ""){
      
    First_LinkSAO_Location <<- which(colnames(datum) == rename_begin_link_sao) 
    Last_LinkSAO_Location <<- which(colnames(datum) == rename_end_link_sao)

    Link_SAONumbers <<- (Last_LinkSAO_Location - First_LinkSAO_Location + 1)
    SAONumbersForLinkage <<- Link_SAONumbers / DutyAreaCount

    LinkStatementNames_SAO <<- paste0("SAAL_IMP_", 1:Link_SAONumbers)

    colnames(datum)[First_LinkSAO_Location:Last_LinkSAO_Location] <- LinkStatementNames_SAO
    
    }

    if(rename_begin_link_know != ""){

      # The assumption here is that Know is after SAO. Further coding would be
      # required if that is NOT the case.
      First_LinkKNOW_Location <<- which(colnames(datum) == rename_begin_link_know) 
      Last_LinkKNOW_Location <<- which(colnames(datum) == rename_end_link_know)

      Link_KnowNumbers <<- (Last_LinkKNOW_Location - First_LinkKNOW_Location + 1)
      KnowNumbersForLinkage <<- Link_KnowNumbers / DutyAreaCount
      
      LinkStatementNames_KNOW <<- paste0("JDKL_IMP_", 1:Link_KnowNumbers)
      
      NewVariable_Names <- c(LinkStatementNames_SAO, LinkStatementNames_KNOW) 

      colnames(datum)[First_LinkSAO_Location:Last_LinkKNOW_Location] <- NewVariable_Names
 
    }
    }
  }
  
  return(datum)

}