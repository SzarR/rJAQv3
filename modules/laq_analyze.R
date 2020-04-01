laq_analyze <- function(datum, skills=FALSE, knowledge=FALSE) {

if (skills == TRUE & knowledge == FALSE) {

  # Obtain column means.
  SAAL_ALI <- colMeans(datum[,LinkStatementNames_SAO], na.rm=TRUE)

  # Create data matrix
  SAAL_Matrix <- matrix(SAAL_ALI, nrow = SAONumbersForLinkage)

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
    unname() %>%
    substr(1, 20)
  
  # Name the columns and rows.
  colnames(SAAL_Weighted_Matrix) <- c(as.character(DutyAreaLabel[1:length(Ratio_DAR)]),"Total","Z_Score","Standardized")
  rownames(SAAL_Weighted_Matrix) <- c(Description_link_sao, "Total")

  # Order results top-down.
  SAAL_Weighted_Matrix <- round(SAAL_Weighted_Matrix[with(SAAL_Weighted_Matrix,order(-SAAL_Total_Row_STD)),],digits = 2)
  
  # #XLSX Output Stuff
  # SAAL_Raw_Weightings  <- createSheet(LAQ_Workbook, sheetName = "SAAL_Raw_Weightings")
  # SAAL_Calc_Weightings <- createSheet(LAQ_Workbook,sheetName = "SAAL_Weighted_Matrix")
  # #Row Styles for SAAL_Weighted_Matrix
  # dfColIndex_SAAL_W           <- rep(list(ROWS_LAQ), dim(SAAL_Weighted_Matrix)[2]) 
  # names(dfColIndex_SAAL_W)    <- seq(1, dim(SAAL_Weighted_Matrix)[2], by = 1)
  # #Row Styles for SAAL_Matrix (Raw)
  # dfColIndex_SAAL_R           <- rep(list(ROWS_LAQ), dim(SAAL_Matrix)[2]) 
  # names(dfColIndex_SAAL_R)    <- seq(1, dim(SAAL_Matrix)[2], by = 1)

  return(SAAL_Weighted_Matrix)

}

  if (knowledge == TRUE & skills == FALSE){
 
    # Obtain column means.
    JDKL_ALI <- colMeans(datum[,LinkStatementNames_KNOW], na.rm=TRUE)

    # Create data matrix
    JDKL_Matrix <- matrix(JDKL_ALI, nrow = KnowNumbersForLinkage)

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
    colnames(JDKL_Weighted_Matrix) <- c(as.character(DutyAreaLabel[1:length(Ratio_DAR)]),"Total","Z_Score","Standardized")
    rownames(JDKL_Weighted_Matrix) <- c(Description_link_know, "Total")

    # Order results top-down.
    JDKL_Weighted_Matrix <- round(JDKL_Weighted_Matrix[with(JDKL_Weighted_Matrix,order(-JDKL_Total_Row_STD)),],digits = 2)

    # #XLSX Output Stuff
    # SAAL_Raw_Weightings  <- createSheet(LAQ_Workbook, sheetName = "SAAL_Raw_Weightings")
    # SAAL_Calc_Weightings <- createSheet(LAQ_Workbook,sheetName = "SAAL_Weighted_Matrix")
    # #Row Styles for SAAL_Weighted_Matrix
    # dfColIndex_SAAL_W           <- rep(list(ROWS_LAQ), dim(SAAL_Weighted_Matrix)[2]) 
    # names(dfColIndex_SAAL_W)    <- seq(1, dim(SAAL_Weighted_Matrix)[2], by = 1)
    # #Row Styles for SAAL_Matrix (Raw)
    # dfColIndex_SAAL_R           <- rep(list(ROWS_LAQ), dim(SAAL_Matrix)[2]) 
    # names(dfColIndex_SAAL_R)    <- seq(1, dim(SAAL_Matrix)[2], by = 1)

    return(JDKL_Weighted_Matrix)

  }

}