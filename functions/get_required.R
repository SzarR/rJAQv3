get_required <- function(datum) {

  # Specify variable names.
  VarLab_REQU_Task <- paste0("REQU_", 1:TaskNumbers)

  # Run calculations.
  REQU <-
    round((apply(datum[, VarLab_REQU_Task], 2, function(x)
      (sum(x == 1, na.rm = T)) / (sum(x == 2, na.rm = T) + sum(x == 1, na.rm =
                                                                 T))) * 100), digits = 2)
  REQU_SD <- round(sapply(datum[, VarLab_REQU_Task], sd, 2), digits = 2)
  
  # Save output.
  Output.Frame <<- cbind(Output.Frame, REQU, REQU_SD)

}



