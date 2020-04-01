jaq_analyze <- function(datum, section) {

  if(section =='task'){

    Output.Frame.Task <<- data.frame(Description.Frame)

    if ("APP" %in% Scale_Choices) {
      get_applicability(dat_task_renamed(), section = 'task')
    }

    if ("IMP" %in% Scale_Choices) {
      get_importance(dat_task_renamed(), section = 'task')
    }

    if ("FREQ" %in% Scale_Choices) {
      get_frequency(dat_task_renamed())
    }

    if ("REQU" %in% Scale_Choices) {
      get_required(dat_task_renamed(), section = 'task')
    }

    if ("IMP" %in% Scale_Choices & "FREQ" %in% Scale_Choices){
      get_composite(dat_task_renamed())
    }

    return(Output.Frame.Task)

  }

  if (section == 'ksao') {

    Output.Frame.KSAO <<- data.frame(Description.Frame)

    if ("APP" %in% Scale_Choices_ksao) {
      get_applicability(dat_ksao_renamed(), section = 'ksao')
    }

    if ("IMP" %in% Scale_Choices_ksao) {
      get_importance(dat_ksao_renamed(), section = 'ksao')
    }

    if ("REQU" %in% Scale_Choices_ksao) {
      get_required(dat_ksao_renamed(), section = 'ksao')
    }

    if ("DIFF" %in% Scale_Choices_ksao) {
      get_differentiation(dat_ksao_renamed())
    }

    if ("RvR" %in% Scale_Choices_ksao) {
      get_rvr(dat_ksao_renamed())
    }

    return(Output.Frame.KSAO)

  }

}
