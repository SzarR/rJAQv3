get_differentiation <- function(datum){

  VarLab_DIFF_Task <- paste0("DIFF_", 1:KSAONumbers)

  # Determine scale type.
  Scale_Type <- ifelse(max(datum[,VarLab_DIFF_Task],na.rm=T) - 
                         min(datum[,VarLab_DIFF_Task],na.rm=T) == 1, 
                       "DICHOT",
                       "LIKERT")

  if(Scale_Type == "LIKERT"){

  # Run calculations.
  DIFF <<- likert_scale(datum, scale = VarLab_DIFF_Task, rounding = 2)
  DIFF_SD <- round(sapply(datum[, VarLab_DIFF_Task], sd, 2), digits = 2)

  Output.Frame.KSAO <<- cbind(Output.Frame.KSAO, DIFF, DIFF_SD)

  } else {

    DIFF <<- dichot_scale(datum, scale = VarLab_DIFF_Task, rounding = 2)
    DIFF_SD <- round(sapply(datum[, VarLab_DIFF_Task], sd, 2), digits = 2)

    Output.Frame.KSAO <<- cbind(Output.Frame.KSAO, DIFF, DIFF_SD)

  }
}