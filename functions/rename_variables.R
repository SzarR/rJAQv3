rename_variables <- function(datum, section, knowledge){
  
  # Written by RWS
  # This function takes a user settable first variable name
  # and last variable name, and counts the number of scales
  # that is also user settable, and then automatically
  # renames all the variables between those two data points
  # in the df. 
  
  # More parameters are introduced as the JAQ task analysis
  # is completed. This includes SAO statements, and 
  # statements for knowledge areas. These are all 
  # very important because different scales drive
  # different analyses.
  
  # FLAG: Ensure the ordering of the variables is always
  # consistent. Not sure how to accomplish this just yet.

  if (section == 'task') {
    
    # Create renaming scales.
    TaskStatementNames <- NULL
    
    if ("APP" %in% Scale_Choices) {
      TaskStatementNames <- c("NA_")
    }
    
    if ("IMP" %in% Scale_Choices) {
      TaskStatementNames <- c(TaskStatementNames, "IMP_")
    }
    
    if ("FREQ" %in% Scale_Choices) {
      TaskStatementNames <- c(TaskStatementNames, "FREQ_")
    }
    
    if ("REQU" %in% Scale_Choices) {
      TaskStatementNames <- c(TaskStatementNames, "REQU_")
    }
    
    if ("DIFF" %in% Scale_Choices) {
      TaskStatementNames <- c(TaskStatementNames, "DIFF_")
    }
    
    # FLAG: If not found, throw an error!
    #####################################
    
    First_Location <- which(colnames(datum) == rename_begin) #
    Last_Location <- which(colnames(datum) == rename_end) #rename_end
    
    #Checker to ensure variables all match up mathematically.
    # ifelse((Last_Task_Location - First_Task_Location + 1) / length(TaskStatementNames) == TaskNumbers,
    #        print("Task Statement Check Good"),
    #        warning("Re-Check task information")
    
    #Run the renaming.
    NewVariable_Names <- as.vector(outer(TaskStatementNames, 1:TaskNumbers, paste0))
    colnames(datum)[First_Location:Last_Location] <- NewVariable_Names
    
  }
  
  if(section == 'ksao'){
    
    KSAOStatementNames <- NULL
    
    if("APP" %in% Scale_Choices_ksao){
      KSAOStatementNames <- c("NA_")
    }
    
    if("IMP" %in% Scale_Choices_ksao){
      KSAOStatementNames <- c(KSAOStatementNames, "IMP_")
    }
    
    if("REQU" %in% Scale_Choices_ksao){
      KSAOStatementNames <- c(KSAOStatementNames, "REQU_")
    }
    
    if("DIFF" %in% Scale_Choices_ksao){
      KSAOStatementNames <- c(KSAOStatementNames, "DIFF_")
    }
    
    First_Location  <- which(colnames(datum) == rename_begin_sao)
    Last_Location   <- which(colnames(datum) == rename_end_sao)
    
    SAONumbers <<- (Last_Location - First_Location + 1) / length(KSAOStatementNames)
    
    #Run the renaming.
    NewVariable_Names <- as.vector(outer(KSAOStatementNames,1:SAONumbers, paste0))
    colnames(datum)[First_Location:Last_Location] <- NewVariable_Names

    if(knowledge==TRUE){

      if("RvR" %in% Scale_Choices_ksao){
        KSAOStatementNames <- c(KSAOStatementNames, "RvR_")
      }

      #Knowledge Renamer
      First_Know_Location  <- which(colnames(datum) == rename_begin_know)
      Last_Know_Location <- which(colnames(datum) == rename_end_know)

      KnowNumbers <<- (Last_Know_Location - First_Know_Location + 1) / length(KSAOStatementNames)

      Know_NewVariable_Names <<- as.vector(outer(KSAOStatementNames, (SAONumbers+1):(SAONumbers + KnowNumbers),paste0))
      colnames(datum)[First_Know_Location:Last_Know_Location] <- Know_NewVariable_Names
    }
  }
  
  return(datum)
  
}