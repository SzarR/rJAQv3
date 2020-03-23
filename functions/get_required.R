get_required <- function(datum, section) {

  # Specify variable names.
  if(section == 'task'){
  VarLab_REQU_Task <- paste0("REQU_", 1:TaskNumbers)
  }
  
  if(section == 'ksao'){
  VarLab_REQU_Task <- paste0("REQU_", 1:KSAONumbers)
  }
  
  # Run calculations.
  REQU <- dichot_scale(datum, scale = VarLab_REQU_Task, rounding = 2)
  
  REQU_SD <- standard_deviation(datum, scale = VarLab_REQU_Task, rounding = 2)

  # Save output.
  if(section == 'task'){
  Output.Frame.Task <<- cbind(Output.Frame.Task, REQU, REQU_SD)
  }
  
  if(section == 'ksao'){
  Output.Frame.KSAO <<- cbind(Output.Frame.KSAO, REQU, REQU_SD)
    
  }
}



