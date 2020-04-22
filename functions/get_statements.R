get_statements <- function(datum, section) {
  
  # Written by Alexa.
  # This function takes a given SPSS file from Survey Monkey
  # and extracts the statement labels from the Value Labels
  # column so that the manual XLSX sheets no longer need be
  # generated as they were in rJAQv2. 
  # Detect columns with q_####_####_0001 format. This pulls the first scale for the variable.
  
  #Number <- str_subset(colnames(datum),"q\\d\\d\\d\\d_\\d\\d\\d\\d_0001")
  
  if(section == 'task'){
  TaskTemp <- paste0("NA_", 1:TaskNumbers)
  
  Description <- datum %>% 
    dplyr::select(TaskTemp) %>% 
    get_label() %>% 
    unname() 
  }
  
  if (section == 'ksao') {
    KSAOTemp <- paste0("IMP_", 1:KSAONumbers)
    
    Description <- datum %>% 
      dplyr::select(KSAOTemp) %>% 
      get_label() %>% 
      unname() 
  }
  
  #remove number from beginning of label
  Description <- gsub("^\\d+","",Description)

  #remove tab or space from beginning of label
  Description <- gsub("^\\t|^\\s","",Description)

  #remove everything from last " - " on
  Description <- gsub("(.*)\\s-\\s.*","\\1",Description)
  Number <- paste0(1:length(Description))

  #create new df.
  Description.Frame <<- data.frame(Number,Description)

  # #Obtain numbers
  # if (section == 'task') {
  #   TaskNumbers <<- nrow(Description.Frame)
  # }
  
  return(Description.Frame)

}