get_composite <- function(datum, ksao = FALSE){

  if(ksao == FALSE){
  # Create a composite data frame.
  Composite_Frame <- (datum[,paste0("IMP_",Description.Frame$Number)] * 2 + datum[,paste0("FREQ_",Description.Frame$Number)])/3
  names(Composite_Frame) <- paste0("C_", Description.Frame$Number)

  # Calculation of Essentially is dependent on whetehr 4/5 point scale used.
  if (Max_IMP == 4 & Max_FREQ == 4) {
    Essentiality_Frame <-
      as.data.frame(ifelse(Composite_Frame[1:length(Composite_Frame)] >= 2.33, 1, 0))
  } else if (Max_IMP == 5 & Max_FREQ == 5) {
    Essentiality_Frame <-
      as.data.frame(ifelse(Composite_Frame[1:length(Composite_Frame)] >= 3.00, 1, 0))
  } else { #FLAG! Dangerous?
    Essentiality_Frame <-
      as.data.frame(ifelse(Composite_Frame[1:length(Composite_Frame)] >= 2.66, 1, 0))
  }

  # Get composite statistics.
  COMP <- round(colMeans(Composite_Frame, na.rm=TRUE), digits = 2)
  COMP_SD <- round(sapply(Composite_Frame, sd, 2), digits = 2)

  # Get essentiality statistics.
  ESS <- round(colMeans(Essentiality_Frame, na.rm=T) * 100, digits = 2)
  ESS_SD <- round(sapply(Essentiality_Frame, sd, 2), digits = 2)

  Output.Frame.Task <<- cbind(Output.Frame.Task, COMP, COMP_SD, ESS, ESS_SD)

  ZONE <- ifelse(APP >= 66.67 & ESS >= 66.67, 1.1,
                ifelse((APP >= 66.67 & ESS >= 50.00 & ESS < 66.67), 1.2,
                ifelse((APP >= 50.00 & APP < 66.67 & ESS >= 66.67), 1.3,
                ifelse((APP >= 50.00 & APP < 66.67 & ESS >= 50.00 & ESS < 66.67), 1.4,
                ifelse((APP >= 66.67 & ESS >= 33.33 & ESS < 50), 2.1,
                ifelse((APP >= 66.67 & ESS >= 0.00 & ESS < 33.32), 2.2,
                ifelse((APP >= 50.00 & APP < 66.67 & ESS >= 33.33 & ESS < 50.00), 2.3,
                ifelse((APP >= 50.00 & APP < 66.67 & ESS >= 0 & ESS < 33.33), 2.4,
                ifelse((APP >= 33.33 & APP < 50.00 & ESS >= 66.67), 3.1,
                ifelse((APP >= 33.33 & APP < 50.00 & ESS >= 50.00 & ESS < 66.67), 3.2,
                ifelse((APP >= 0 & APP < 33.33 & ESS >= 66.67), 3.3,
                ifelse((APP >= 0 & APP < 33.33 & ESS >= 50.00 & ESS < 66.67), 3.4,
                ifelse((APP >= 33.33 & APP < 50.00 & ESS >= 33.33 & ESS < 50.00), 4.1,
                ifelse((APP >= 33.33 & APP < 50.00 & ESS >= 0 & ESS < 33.33), 4.2,
                ifelse((APP >= 0 & APP < 33.33 & ESS >= 33.33 & ESS < 50.00), 4.3,
                ifelse((APP >= 0 & APP < 33.33 & ESS >= 0 & ESS < 33.33), 4.4, NA
                )))))))))))))))) #BOOM Matrix created by Bob. 
  
    Output.Frame.Task <<- cbind(Output.Frame.Task, ZONE)
  }
  
    if(ksao == TRUE){
      
      # Make this a seperate function that is user-editable?
      IMP_Cutter <- if(Max_IMP == 5) {
        IMP_Cutter <- 3
      } else if(Max_IMP == 4) {
        IMP_Cutter <- 2.5
      } else if(Max_IMP == 3) {
        IMP_Cutter <- 1.5
      }
  
      Essentiality <- ifelse(Output.Frame.KSAO$APP >= 66.67 & Output.Frame.KSAO$IMP >= IMP_Cutter,1,0)

      Output.Frame.KSAO <<- cbind(Output.Frame.KSAO, Essentiality)
    }
}