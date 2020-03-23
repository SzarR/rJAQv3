get_importance <- function(datum, section) {
  
  # Specify variable names.
  if(section == 'task'){
  VarLab_IMP_Task <- paste0("IMP_", 1:TaskNumbers)
  }
  
  if(section == 'ksao'){
    VarLab_IMP_Task <- paste0("IMP_", 1:KSAONumbers)
  }
  
  # Meta-data stuff.
  Max_IMP <<- max(datum[, VarLab_IMP_Task], na.rm = T)
  
  # Run calculations.
  IMP <<- round(colMeans(datum[, VarLab_IMP_Task], na.rm = T), digits = 2)
  IMP_SD <- round(sapply(datum[, VarLab_IMP_Task], sd, 2), digits = 2)
  
  # Save output.
  if(section == 'task'){
    Output.Frame.Task <<- cbind(Output.Frame.Task, IMP, IMP_SD)
  }
  
  if(section == 'ksao'){
    Output.Frame.KSAO <<- cbind(Output.Frame.KSAO, IMP, IMP_SD)
  }
  
}