laq_analyze <- function(datum, skills=FALSE, knowledge=FALSE) {
  
if (skills == TRUE & knowledge == FALSE) {
  
  if(rename_begin_link_sao == "" | rename_end_link_sao == "") {
    
    make_alert(title = "Error!", 
               text = "You did not specify column limits for skill/ability areas above!",
               type = 'error')
    
  } else {
  
  # Obtain column means.
  SAAL_ALI <- colMeans(datum[,LinkStatementNames_SAO], na.rm=TRUE)
  
  SAAL_Matrix <<- matrix(SAAL_ALI, nrow = SAONumbersForLinkage)

  # Do multiplication on the created matrix
  SAAL_Weighted_Matrix    <- as.data.frame(sapply(1:ncol(SAAL_Matrix),function(x) Ratio_DAR[x] * SAAL_Matrix[,x]))

  # Run summation and standardization
  SAAL_Total_Row        <- rowSums(SAAL_Weighted_Matrix)
  SAAL_Total_Row_Z      <- scale(SAAL_Total_Row)
  SAAL_Total_Row_STD    <- (((SAAL_Total_Row_Z) * 1) + 3)
  SAAL_Weighted_Matrix  <- cbind(SAAL_Weighted_Matrix,SAAL_Total_Row, SAAL_Total_Row_Z, SAAL_Total_Row_STD)
  SAAL_Total_Col        <- colSums(SAAL_Weighted_Matrix)
  SAAL_Weighted_Matrix  <- rbind(SAAL_Weighted_Matrix,SAAL_Total_Col) 

  Description_link_sao <<- datum %>%
    dplyr::select(paste0("SAAL_IMP_", 1:SAONumbersForLinkage)) %>%
    get_label() %>%
    unname()

  # FLAG RESEARCH THIS FURTHER - BROKEN?
  #if theres a -, –, or : plus a space within the first 30 characters, drop everything afte the colon. Otherwise, keep the first 20 characters.
  Description_link_sao <<- ifelse(grepl("–\\s", substr(Description_link_sao,1,30)), gsub("(.*)–\\s.*","\\1",Description_link_sao),
                 ifelse(grepl("-\\s", substr(Description_link_sao,1,30)), gsub("(.*)-\\s.*","\\1",Description_link_sao),
                        ifelse(grepl(":\\s", substr(Description_link_sao,1,30)), gsub("(.*):\\s.*","\\1",Description_link_sao), substr(Description_link_sao,1,20))))     

  # Name the columns and rows.
  colnames(SAAL_Weighted_Matrix) <- c(as.character(DutyAreaLabels[1:length(Ratio_DAR)]),"Total","Z_Score","Standardized")
  rownames(SAAL_Weighted_Matrix) <- c(Description_link_sao, "Total")

  # Order results top-down.
  SAAL_Weighted_Matrix <- round(SAAL_Weighted_Matrix[with(SAAL_Weighted_Matrix,order(-SAAL_Total_Row_STD)),],digits = 2)
  
  # Create data matrix
  SAAL_Matrix <<- matrix(SAAL_ALI, nrow = SAONumbersForLinkage)
  SAAL_Matrix <<- round(SAAL_Matrix, digits = 2)
  colnames(SAAL_Matrix)   <<- c(as.character(DutyAreaLabels[1:length(Ratio_DAR)]))
  rownames(SAAL_Matrix)   <<- c(Description_link_sao)

  return(SAAL_Weighted_Matrix)
}
}

  if (knowledge == TRUE & skills == FALSE){
    
    if(rename_begin_link_know == "" | rename_end_link_know == "") {
      
      make_alert(title = "Error!", 
                 text = "You did not specify column limits for knowledge areas above!",
                 type = 'error')
      
    } else {

    # Obtain column means.
    JDKL_ALI <- colMeans(datum[,LinkStatementNames_KNOW], na.rm=TRUE)

    # Create data matrix
    JDKL_Matrix <<- matrix(JDKL_ALI, nrow = KnowNumbersForLinkage)

    # Do multiplication on the created matrix
    JDKL_Weighted_Matrix    <- as.data.frame(sapply(1:ncol(JDKL_Matrix),function(x) Ratio_DAR[x] * JDKL_Matrix[,x]))

    # Run summation and standardization
    JDKL_Total_Row        <- rowSums(JDKL_Weighted_Matrix)
    JDKL_Total_Row_Z      <- scale(JDKL_Total_Row)
    JDKL_Total_Row_STD    <- (((JDKL_Total_Row_Z) * 1) + 3)
    JDKL_Weighted_Matrix  <- cbind(JDKL_Weighted_Matrix,JDKL_Total_Row, JDKL_Total_Row_Z, JDKL_Total_Row_STD)
    JDKL_Total_Col        <- colSums(JDKL_Weighted_Matrix)
    JDKL_Weighted_Matrix  <- rbind(JDKL_Weighted_Matrix,JDKL_Total_Col) 

      Description_link_know <<- datum %>%
        dplyr::select(paste0("JDKL_IMP_", 1:KnowNumbersForLinkage)) %>%
        get_label() %>%
        unname() %>%
        substr(1, 35)

    # Name the columns and rows.
    colnames(JDKL_Weighted_Matrix) <- c(as.character(DutyAreaLabels[1:length(Ratio_DAR)]),"Total","Z_Score","Standardized")
    rownames(JDKL_Weighted_Matrix) <- c(Description_link_know, "Total")

    # Order results top-down.
    JDKL_Weighted_Matrix <- round(JDKL_Weighted_Matrix[with(JDKL_Weighted_Matrix,order(-JDKL_Total_Row_STD)),],digits = 2)

    # Create data matrix
    JDKL_Matrix <<- matrix(JDKL_ALI, nrow = KnowNumbersForLinkage)
    JDKL_Matrix <<- round(JDKL_Matrix, digits = 2)
    colnames(JDKL_Matrix)   <<- c(as.character(DutyAreaLabels[1:length(Ratio_DAR)]))
    rownames(JDKL_Matrix)   <<- c(Description_link_know)

    return(JDKL_Weighted_Matrix)
    }
  }
}