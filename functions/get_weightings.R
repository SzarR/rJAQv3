get_weightings <- function(datum){
  
  # First, exttract the duty area labels from
  # the dat_task_renamed() file.
  # 
  # dutyarea_begin <- c("q0023_0001")
  # dutyarea_end <- c("q0023_0011")
  
  #FLAG Create Error code where the duty areas
  #DO not match to the linkage analysis.
  
  First_DA_Location  <- which(colnames(datum) == dutyarea_begin)
  Last_DA_Location <- which(colnames(datum) == dutyarea_end)
  
  DutyAreaCount <<-  (Last_DA_Location - First_DA_Location + 1)
  
  Number <- names(datum)[First_DA_Location:Last_DA_Location]

  Description <- datum %>% 
    select(Number) %>% 
    get_label() %>% 
    unname() 
  
  #remove number from beginning of label
  Description <- gsub("^\\d+","",Description)
  DutyAreaLabel <<- Description
  
  #create new df.
  Weighting.Frame <- cbind(1:DutyAreaCount, Description)

  # Calculate Average values.
  Corrected_DAR <- (colMeans(datum[,Number], na.rm=T))
  Ratio_DAR     <<- (round((Corrected_DAR / 100), digits = 2))
  Weight_Percent <- paste0(Ratio_DAR, "%")

  # Empty for later
  Start_DA <- rep("", DutyAreaCount)
  
  Weighting.Frame <- cbind(Weighting.Frame, Weight_Percent, Start_DA, Start_DA)
  colnames(Weighting.Frame) <- c("Duty Area", "Label", "Weight", "Start", "End")

  return(Weighting.Frame)
  
}