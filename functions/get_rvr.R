get_rvr <- function(datum) {
  
  # Specify variable names.
  VarLab_RvR_Task <- paste0("RvR_",  (SAONumbers+1):(SAONumbers + KnowNumbers))
  
  # FLAG: Trigger here for if Knowledge area comes BEFORE SAO's?
  
  # Specify NA for the fields before the knowledge statements
  REF <- rep("", SAONumbers)
  REF_SD <- rep("", SAONumbers)

  REF[(SAONumbers + 1):(SAONumbers + KnowNumbers)] <-
    round((apply(datum[, VarLab_RvR_Task], 2, function(x)
      (sum(
        x == 1, na.rm = T
      )) / (
        sum(x == 2, na.rm = T) + sum(x == 1, na.rm =
                                       T)
      )) * 100), digits = 2)
  
  REF_SD[(SAONumbers+1):(SAONumbers+KnowNumbers)] <- round(sapply(datum[, VarLab_RvR_Task], sd, 2), digits = 2)
  
  Output.Frame.KSAO <<- cbind(Output.Frame.KSAO, REF, REF_SD)
  
  }