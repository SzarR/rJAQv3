get_weightings <- function(datum){
  
  if(dutyarea_end %in% colnames(datum)){

  # Vector of duty area names
  DutyAreaVars <-
    datum %>%
    select(dutyarea_begin:dutyarea_end) %>%
    names()
  
  # Count of duty areas
  DutyAreaCount <<-  length(DutyAreaVars)

  # Duty Area Labels/Names
  DutyAreaLabels <<- 
    datum %>% 
    select(DutyAreaVars) %>% 
    get_label() %>% 
    unname()

  # Remove number from label beginning.
  DutyAreaLabels <- trimws(gsub("^\\d+|\\.|\\t|^\\s|-|,|;|:","",DutyAreaLabels))
  
  # Calculate Average values.
  Corrected_DAR <- (colMeans(datum[,DutyAreaVars], na.rm=T))
  Ratio_DAR <<- (round((Corrected_DAR / 100), digits = 2))
  Weight_Percent <- paste0(Ratio_DAR * 100, "%")

  Weighting.Frame <- tibble('Label' = DutyAreaLabels,
                            'Weight' = Weight_Percent)
  
  if(length(DA_Limits == DutyAreaCount)) {
    
    # Create a vector of duty area labels to cbind to task analysis.
    da_df <<- get_da_limits(DA_Limits)
    
    Weighting.Frame <- tibble('Label' = DutyAreaLabels,
                              'Weight' = Weight_Percent,
                              'Begin' = da_df[['Begin']],
                              'End' = da_df[['End']],
                              'Count' = da_df[['Count']])
    
    DutyAreaDefinitions <<- rep(Weighting.Frame$Label,
                                Weighting.Frame$Count)
    
  } else {
    make_alert(title = "Error!",
               text = "Check your duty area limits and try again.",
               type = 'error')
    
  }
  
  return(Weighting.Frame)
  
  } else {
    make_alert(title = "Error!",
               text = "Cannot locate columns in specified file.",
               type = 'error')
  }
}