get_applicability <- function(datum, section) {
  
  # Specify variable names.
  if(section == 'task'){
  VarLab_NA_Task <- paste0("NA_", 1:TaskNumbers)
  }
  
  if(section == 'ksao'){
    VarLab_NA_Task <- paste0("NA_", 1:KSAONumbers)
  }
  
  # Run calculations.
  APP <<- dichot_scale(datum, scale = VarLab_NA_Task,yes = 1,no = 2,rounding = 2)
  APP_SD <- round(sapply(datum[, VarLab_NA_Task], sd, 2), digits = 2)
  
  if(section == 'task'){
  Output.Frame.Task <<- cbind(Output.Frame.Task, APP, APP_SD)
  }
  
  if(section == 'ksao'){
    Output.Frame.KSAO <<- cbind(Output.Frame.KSAO, APP, APP_SD)
  }
  
}