jaq_analyze <- function(datum, section) {

  if(section == 'task'){
    Output.Frame.Task <<- data.frame(Description.Frame)

    if ("NA_" %in% Scale_Choices) {
      get_applicability(datum, section = 'task')
    }

    if ("IMP_" %in% Scale_Choices) {
      get_importance(datum, section = 'task')
    }

    if ("FREQ_" %in% Scale_Choices) {
      get_frequency(datum)
    }

    if ("REQU_" %in% Scale_Choices) {
      get_required(datum, section = 'task')
    }

    if ("IMP_" %in% Scale_Choices & "FREQ_" %in% Scale_Choices){
      get_composite(datum)
    }
    
    # Set row names.
    row.names(Output.Frame.Task) <- c(1:TaskNumbers)

    return(Output.Frame.Task)
  }

  if (section == 'ksao') {

    Output.Frame.KSAO <<- data.frame(Description.Frame)

    if ("NA_" %in% Scale_Choices_ksao) {
      get_applicability(datum, section = 'ksao')
    }

    if ("IMP_" %in% Scale_Choices_ksao) {
      get_importance(datum, section = 'ksao')
    }

    if ("REQU_" %in% Scale_Choices_ksao) {
      get_required(datum, section = 'ksao')
    }

    if ("DIFF_" %in% Scale_Choices_ksao) {
      get_differentiation(datum)
    }

    if ("RvR_" %in% Scale_Choices_ksao) {
      get_rvr(datum)
    }
    
    if ("IMP_" %in% Scale_Choices_ksao & "NA_" %in% Scale_Choices_ksao){
      get_composite(datum, ksao = TRUE)
    }

    row.names(Output.Frame.KSAO) <- c(1:KSAONumbers)
    return(Output.Frame.KSAO)

  }
}