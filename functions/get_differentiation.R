get_differentiation <- function(datum){
  
  VarLab_DIFF_Task <- paste0("DIFF_", 1:KSAONumbers)
  
  # Run calculations.
  DIFF <<- likert_scale(datum, scale = VarLab_DIFF_Task, rounding = 2)
  DIFF_SD <- round(sapply(datum[, VarLab_DIFF_Task], sd, 2), digits = 2)
  
  Output.Frame.KSAO <<- cbind(Output.Frame.KSAO, DIFF, DIFF_SD)
  
}