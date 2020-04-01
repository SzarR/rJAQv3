get_frequency <- function(datum) {
  
  # Do not need section argument as no
  # frequency for KSAO statements.
  
  # Specify variable names.
  VarLab_FREQ_Task <<- paste0("FREQ_", 1:TaskNumbers)

  # Meta-data stuff.
  Max_FREQ <<- max(datum[,VarLab_FREQ_Task],na.rm=T)
  
  # Run calculations.
  FREQ <- likert_scale(datum, VarLab_FREQ_Task, rounding = 2)
  FREQ_SD <- round(sapply(datum[, VarLab_FREQ_Task], sd, 2), digits = 2)
  
  # Save output.
  Output.Frame.Task <<- cbind(Output.Frame.Task, FREQ, FREQ_SD)

}