jaq_analyze <- function(datum, section) {
  
  withProgress(message = 'Analyzing Data',value = 0, {

  if(section == 'task'){
    Output.Frame.Task <<- data.frame(Description.Frame)

    if ("NA_" %in% Scale_Choices) {
      incProgress(amount = 0.2, message = "Calculating Applicability")
      get_applicability(datum, section = 'task')
    }

    if ("IMP_" %in% Scale_Choices) {
      incProgress(amount = 0.4, message = "Calculating Importance")
      get_importance(datum, section = 'task')
    }

    if ("FREQ_" %in% Scale_Choices) {
      incProgress(amount = 0.6, message = "Calculating Frequency")
      get_frequency(datum)
    }

    if ("REQU_" %in% Scale_Choices) {
      incProgress(amount = 0.8, message = "Calculating Requirements")
      get_required(datum, section = 'task')
    }

    if ("IMP_" %in% Scale_Choices & "FREQ_" %in% Scale_Choices){
      incProgress(amount = 1, message = "Calculating Composite Scores")
      get_composite(datum)
    }
    
    # Set row names.
    row.names(Output.Frame.Task) <- c(1:TaskNumbers)
    return(Output.Frame.Task)
  }

  if (section == 'ksao') {

    Output.Frame.KSAO <<- data.frame(Description.Frame)

    if ("NA_" %in% Scale_Choices_ksao) {
      incProgress(amount = 0.2, message = "Calculating Applicability")
      get_applicability(datum, section = 'ksao')
    }

    if ("IMP_" %in% Scale_Choices_ksao) {
      incProgress(amount = 0.3, message = "Calculating Importance")
      get_importance(datum, section = 'ksao')
    }

    if ("REQU_" %in% Scale_Choices_ksao) {
      incProgress(amount = 0.6, message = "Calculating Requirements")
      get_required(datum, section = 'ksao')
    }

    if ("DIFF_" %in% Scale_Choices_ksao) {
      incProgress(amount = 0.7, message = "Calculating Differentiation")
      get_differentiation(datum)
    }

    if ("RvR_" %in% Scale_Choices_ksao) {
      incProgress(amount = 0.8, message = "Calculating Reference v. Recall")
      get_rvr(datum)
    }
    
    if ("IMP_" %in% Scale_Choices_ksao & "NA_" %in% Scale_Choices_ksao){
      incProgress(amount = 0.9, message = "Calculating Composite Scores")
      get_composite(datum, ksao = TRUE)
    }

    row.names(Output.Frame.KSAO) <- c(1:KSAONumbers)
    return(Output.Frame.KSAO)

  }
  })
}