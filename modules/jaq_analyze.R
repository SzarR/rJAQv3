jaq_analyze <- function(datum, section) {

  if(section == 'task'){
    Output.Frame.Task <<- data.frame(Description.Frame)

    if ("APP" %in% Scale_Choices) {
      get_applicability(datum, section = 'task')
    }

    if ("IMP" %in% Scale_Choices) {
      get_importance(datum, section = 'task')
    }

    if ("FREQ" %in% Scale_Choices) {
      get_frequency(datum)
    }

    if ("REQU" %in% Scale_Choices) {
      get_required(datum, section = 'task')
    }

    if ("IMP" %in% Scale_Choices & "FREQ" %in% Scale_Choices){
      get_composite(datum)
    }

    return(Output.Frame.Task)
  }

  if (section == 'ksao') {

    Output.Frame.KSAO <<- data.frame(Description.Frame)

    if ("APP" %in% Scale_Choices_ksao) {
      get_applicability(datum, section = 'ksao')
    }

    if ("IMP" %in% Scale_Choices_ksao) {
      get_importance(datum, section = 'ksao')
    }

    if ("REQU" %in% Scale_Choices_ksao) {
      get_required(datum, section = 'ksao')
    }

    if ("DIFF" %in% Scale_Choices_ksao) {
      get_differentiation(datum)
    }

    if ("RvR" %in% Scale_Choices_ksao) {
      get_rvr(datum)
    }

    return(Output.Frame.KSAO)

  }
}