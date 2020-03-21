get_frequency <- function(datum) {
  
  # Specify variable names.
  VarLab_FREQ_Task <<- paste0("FREQ_", 1:TaskNumbers)
  
  # Meta-data stuff.
  Max_FREQ <<- max(datum[,VarLab_FREQ_Task],na.rm=T)
  
  # Run calculations.
  FREQ <- round(colMeans(datum[, VarLab_FREQ_Task], na.rm = T), digits = 2)
  FREQ_SD <- round(sapply(datum[, VarLab_FREQ_Task], sd, 2), digits = 2)
  
  # Save output.
  Output.Frame <<- cbind(Output.Frame, FREQ, FREQ_SD)
  
}