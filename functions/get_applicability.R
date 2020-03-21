get_applicability <- function(datum) {
  
  # Specify variable names.
  VarLab_NA_Task <- paste0("NA_", 1:TaskNumbers)

  # Run calculations.
  APP <<-
    round((apply(datum[, VarLab_NA_Task], 2, function(x)
      (sum(x == 1, na.rm = T)) / (sum(x == 2, na.rm = T) + sum(x == 1, na.rm =
                                                                 T))) * 100), digits = 2)
  APP_SD <- round(sapply(datum[, VarLab_NA_Task], sd, 2), digits = 2)
  
  #return(data.frame(Task_NA_Sum, Task_NA_SD))
  Output.Frame <<- cbind(Output.Frame, APP, APP_SD)
  
}