get_importance <- function(datum) {

  # Specify variable names.
  VarLab_IMP_Task <- paste0("IMP_", 1:TaskNumbers)
  
  # Meta-data stuff.
  Max_IMP <<- max(datum[,VarLab_IMP_Task],na.rm=T)
  
  # Run calculations.
  IMP <<- round(colMeans(datum[, VarLab_IMP_Task], na.rm = T), digits = 2)
  IMP_SD <- round(sapply(datum[, VarLab_IMP_Task], sd, 2), digits = 2)
  
  # Save output.
  Output.Frame <<- cbind(Output.Frame, IMP, IMP_SD)
  
}