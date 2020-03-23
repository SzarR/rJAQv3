get_differentiation <- function(datum){
  
  VarLab_DIFF_Task <- paste0("DIFF_", 1:KSAONumbers)
  
  # Run calculations.
  DIFF <<- round(colMeans(datum[, VarLab_DIFF_Task], na.rm = T), digits = 2)
  DIFF_SD <- round(sapply(datum[, VarLab_DIFF_Task], sd, 2), digits = 2)
  
  Output.Frame.KSAO <<- cbind(Output.Frame.KSAO, DIFF, DIFF_SD)
  
}