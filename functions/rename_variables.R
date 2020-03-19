rename_variables <- function(datum){
  
  # Written by RWS
  # This function takes a user settable first variable name
  # and last variable name, and counts the number of scales
  # that is also user settable, and then automatically
  # renames all the variables between those two data points
  # in the df. 
  
  # FLAG: Ensure the ordering of teh variables is always
  # consistent. Not sure how to accomplish this just yet.
  
  # Three different theoretical sections, task, ksa, or
  # linkage. Specify here to call appropriate numbers.

  # Create renaming scales.
  StatementNames <- NULL

  if("Applicability" %in% Scale_Choices){
    StatementNames <- c("NA_")
  }

  if("Importance" %in% Scale_Choices){
    StatementNames <- c(StatementNames, "IMP_")
  }

  if("Frequency" %in% Scale_Choices){
    StatementNames <- c(StatementNames, "FREQ_")
  }

  if("Required upon Promotion" %in% Scale_Choices){
    StatementNames <- c(StatementNames, "REQU_")
  }

  # FLAG: If not found, throw an error!
  #####################################
  
  First_Location  <- which(colnames(datum) == rename_begin)
  Last_Location   <- which(colnames(datum) == rename_end)

  #Checker to ensure variables all match up mathematically.
  # ifelse((Last_Task_Location - First_Task_Location + 1) / length(TaskStatementNames) == TaskNumbers,
  #        print("Task Statement Check Good"),
  #        warning("Re-Check task information")

  #Run the renaming.
  NewVariable_Names <- as.vector(outer(StatementNames,1:TaskNumbers, paste0))
  colnames(datum)[First_Location:Last_Location] <- NewVariable_Names

  return(datum)
  
}