jaq_analyze <- function(datum, section) {

# Specify section conditions: task/ksa/link. ------------------------------
  # if (section == "task") {
  #   Scale_Choices = Scale_Choices
  # } else if (section == "ksa") {
  #   Scale_Choices = KSA_Scale_Choices
  # } else if (section == "link") {
  #   Scale_Choices = Link_Scale_Choices
  # }
  
# Output container --------------------------------------------------------
  Output.Frame <<- data.frame(Description.Frame)
  
    if ("Applicability" %in% Scale_Choices) {
      get_applicability(dat_task_renamed())
    }
    
    if ("Importance" %in% Scale_Choices) {
      get_importance(dat_task_renamed())
    }

    if ("Frequency" %in% Scale_Choices) {
      get_frequency(dat_task_renamed())
    }

    if ("Required upon Promotion" %in% Scale_Choices) {
      get_required(dat_task_renamed())
    }
  
  # Run final calculations for composite/essentiality.
  # Frequency NOT in KSAO frame.
  if ("Frequency" %in% Scale_Choices) {
    get_composite(dat_task_renamed())
  }
  
  return(Output.Frame)
  }