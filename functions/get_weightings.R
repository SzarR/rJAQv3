get_weightings <- function(datum){
  
  req(DA_Limits)

  if(dutyarea_end %in% colnames(datum)){

  # Vector of duty area names
  DutyAreaVars <-
    datum %>%
    select(dutyarea_begin:dutyarea_end) %>%
    names()
  
  # Count of duty areas
  DutyAreaCount <<-  length(DutyAreaVars)

  # Duty Area Labels/Names
  DutyAreaLabels <- 
    datum %>% 
    select(DutyAreaVars) %>% 
    get_label() %>% 
    unname()

  # Remove number from label beginning.
  DutyAreaLabels <<- trimws(gsub("^\\d+|\\.|\\t|^\\s|-|,|;|:","",DutyAreaLabels))
  

  # Calculate Average values.
  Corrected_DAR <- (colMeans(datum[,DutyAreaVars], na.rm=T))
  Ratio_DAR <<- (round((Corrected_DAR / 100), digits = 2))
  Weight_Percent <- paste0(Ratio_DAR * 100, "%")

  Weighting.Frame <- tibble(`Duty Area` = 1:DutyAreaCount,
                            'Label' = DutyAreaLabels,
                            'Weight' = Weight_Percent,
                            'Begin' = da_df[['Begin']],
                            'End' = da_df[['End']])
    
  #   
  #   cbind(1:DutyAreaCount, DutyAreaLabels, Weight_Percent, da_df$Begin, da_df$End, da_df$Count)
  # colnames(Weighting.Frame) <- c("Duty Area", "Label", "Weight", "Begin", "End")
  # 
  return(Weighting.Frame)
  
  } else {
    make_alert(title = "Error!",
               text = "Cannot locate columns in specified file.",
               type = 'error')
  }
  
}