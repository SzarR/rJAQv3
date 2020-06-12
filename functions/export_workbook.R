export_workbook <- function() {

  Results_Workbook <<- xlsx::createWorkbook(type = "xlsx")

  # Excel table option settings -----------------
  TABLE_ROWNAMES_STYLE <-
    CellStyle(Results_Workbook) + Font(Results_Workbook, isBold = TRUE) + Alignment(horizontal = "ALIGN_CENTER") +
    Border(
      color = "black",
      position = c("TOP", "BOTTOM", "LEFT", "RIGHT"),
      pen = c("BORDER_THIN")
    )

  TABLE_COLNAMES_STYLE <-
    CellStyle(Results_Workbook) + Fill(foregroundColor = "black",backgroundColor = 'white') + Font(Results_Workbook,
                                                                           isBold = TRUE,
                                                                           name = "Calibri",
                                                                           color = "azure") +
    Alignment(wrapText = FALSE, horizontal = "ALIGN_CENTER") + Border(
      color = "lightgrey",
      position = c("TOP", "BOTTOM", "LEFT", "RIGHT"),
      pen =
        c("BORDER_THIN", "BORDER_THICK", "BORDER_THIN", "BORDER_THIN")
    )

  ROWS <-
    CellStyle(Results_Workbook) + Font(wb = Results_Workbook,
                                   name = "Calibri",
                                   heightInPoints = 10) + Alignment(horizontal = "ALIGN_CENTER",
                                                                    wrapText = TRUE,
                                                                    vertical = "VERTICAL_CENTER") +
    Border(
      color = "black",
      position = c("TOP", "LEFT", "RIGHT", "BOTTOM"),
      pen = c("BORDER_THIN")
    )

# Task Analysis Export Piece ----------------------------------------------

  if(!is.null(Tasks_Analyzed())){
    outs_1 <- Tasks_Analyzed()[,-1]
    
    # Append Duty area Labels
    outs_1 <- cbind(outs_1, DutyAreaLabel)

    Task_Analysis <-
      xlsx::createSheet(wb = Results_Workbook, sheetName = "Task_Analysis")

    dfColIndex           <- rep(list(ROWS), dim(outs_1)[2])
    names(dfColIndex)    <- seq(1, dim(outs_1)[2], by = 1)
    
    addDataFrame(
      x = outs_1,
      sheet = Task_Analysis,
      startRow = 1,
      startColumn = 1,
      colStyle = dfColIndex,
      showNA = FALSE,
      colnamesStyle = TABLE_COLNAMES_STYLE,
      rownamesStyle = TABLE_ROWNAMES_STYLE)

    setColumnWidth(sheet = Task_Analysis,
                   colIndex = 2,
                   colWidth = 90) # Change column width

    setColumnWidth(sheet = Task_Analysis,
                   colIndex = 16,
                   colWidth = 60) # Change column width
    
    createFreezePane(sheet = Task_Analysis,
                     colSplit = 3,
                     rowSplit = 2) # Freeze Panes
  }
  
# KSAO Analysis Output ----------------------------------------------------
  
   if (rename_begin_sao != "" |
       rename_begin_know != "") {
  #if(!is.null(KSAOs_Analyzed())){

    outs_2 <<- KSAOs_Analyzed()[,-1]

    KSAO_Analysis <-
      xlsx::createSheet(wb = Results_Workbook, sheetName = "KSAO_Analysis")
# 
#     dfColIndex           <- rep(list(ROWS), dim(outs_2)[2])
#     names(dfColIndex)    <- seq(1, dim(outs_2)[2], by = 1)

    addDataFrame(
      x = outs_2,
      sheet = KSAO_Analysis,
      startRow = 1,
      startColumn = 1,
      #colStyle = dfColIndex,
      showNA = FALSE,
      colnamesStyle = TABLE_COLNAMES_STYLE,
      rownamesStyle = TABLE_ROWNAMES_STYLE
    )
    
    setColumnWidth(sheet = KSAO_Analysis,
                   colIndex = 2,
                   colWidth = 90) # Change column width
    createFreezePane(sheet = KSAO_Analysis,
                     colSplit = 3,
                     rowSplit = 2) # Freeze Panes
  }

# Linkage SAO  ---------------------------------------------

  if (rename_begin_link_sao != "") {
    outs_3 <- Link_SAO_Analyzed()

    Weighted_SAO <-
      xlsx::createSheet(wb = Results_Workbook, sheetName = "Weighted_SAO")

    dfColIndex           <- rep(list(ROWS), dim(outs_3)[2])
    names(dfColIndex)    <- seq(1, dim(outs_3)[2], by = 1)

    addDataFrame(
      x = outs_3,
      sheet = Weighted_SAO,
      startRow = 1,
      colStyle = dfColIndex,
      startColumn = 1,
      showNA = FALSE,
      colnamesStyle = TABLE_COLNAMES_STYLE,
      rownamesStyle = TABLE_ROWNAMES_STYLE
    )

    setColumnWidth(sheet = Weighted_SAO,
                   colIndex = 1,
                   colWidth = 37)

    Raw_SAO <-
      xlsx::createSheet(wb = Results_Workbook, sheetName = "Raw_SAO")
    
    dfColIndex           <- rep(list(ROWS), dim(SAAL_Matrix)[2])
    names(dfColIndex)    <- seq(1, dim(SAAL_Matrix)[2], by = 1)

    addDataFrame(
      x = SAAL_Matrix,
      sheet = Raw_SAO,
      startRow = 1,
      colStyle = dfColIndex,
      startColumn = 1,
      showNA = FALSE,
      colnamesStyle = TABLE_COLNAMES_STYLE,
      rownamesStyle = TABLE_ROWNAMES_STYLE
    )
      setColumnWidth(sheet = Raw_SAO, colIndex = 1, colWidth = 37)
  }

  # Linkage Knowledge  ---------------------------------------------

  if (rename_begin_link_know != "") {
    outs_4 <- Link_KNOW_Analyzed()

    Weighted_KNOW <-
      xlsx::createSheet(wb = Results_Workbook, sheetName = "Weighted_KNOW")

    dfColIndex           <- rep(list(ROWS), dim(outs_4)[2])
    names(dfColIndex)    <- seq(1, dim(outs_4)[2], by = 1)

    addDataFrame(
      x = outs_4,
      sheet = Weighted_KNOW,
      startRow = 1,
      startColumn = 1,
      colStyle = dfColIndex,
      showNA = FALSE,
      colnamesStyle = TABLE_COLNAMES_STYLE,
      rownamesStyle = TABLE_ROWNAMES_STYLE
    )

    setColumnWidth(sheet = Weighted_KNOW,
                   colIndex = 1,
                   colWidth = 37) # Change column width

    Raw_KNOW <-
      xlsx::createSheet(wb = Results_Workbook, sheetName = "Raw_KNOW")

    dfColIndex           <- rep(list(ROWS), dim(JDKL_Matrix)[2])
    names(dfColIndex)    <- seq(1, dim(JDKL_Matrix)[2], by = 1)

    addDataFrame(
      x = JDKL_Matrix,
      sheet = Raw_KNOW,
      startRow = 1,
      colStyle = dfColIndex,
      startColumn = 1,
      showNA = FALSE,
      colnamesStyle = TABLE_COLNAMES_STYLE,
      rownamesStyle = TABLE_ROWNAMES_STYLE
    )
    setColumnWidth(sheet = Raw_KNOW, colIndex = 1, colWidth = 37)
  }

  # Duty Area Weightings  ---------------------------------------------

  if (dutyarea_begin != "") {
    outs_da <- DutyAreas()

    DutyAreaz <-
      xlsx::createSheet(wb = Results_Workbook, sheetName = "Duty_Areas")

    dfColIndex           <- rep(list(ROWS), dim(outs_da)[2])
    names(dfColIndex)    <- seq(1, dim(outs_da)[2], by = 1)

    addDataFrame(
      x = outs_da,
      sheet = DutyAreaz,
      startRow = 1,
      startColumn = 1,
      colStyle = dfColIndex,
      showNA = FALSE,
      colnamesStyle = TABLE_COLNAMES_STYLE,
      rownamesStyle = TABLE_ROWNAMES_STYLE
    )

    setColumnWidth(sheet = DutyAreaz,
                   colIndex = 3,
                   colWidth = 50) # Change column width
  }
  
# Demographics ------------------------------------------------------------

if (nrow(demo_task_df) > 0) {
  
  if(Which_Demographic_File == 'Task Analysis'){
  xyz <<- get_demographics(datum=values$dat_task)
  } else if(Which_Demographic_File == 'KSAO Analysis') {
    xyz <<- get_demographics(datum=values$dat_ksao)
  } else if(Which_Demographic_File == 'Linkage Analysis'){
    xyz <<- get_demographics(datum=values$dat_link)
  }

  Demoz <-
    xlsx::createSheet(wb = Results_Workbook, sheetName = "Demographics")
  
  for (row in 1:length(xyz)) {
    addDataFrame(
      x = xyz[[row]],
      sheet = Demoz,
      startRow = row * 12,
      startColumn = 2,
      showNA = FALSE,
      colnamesStyle = TABLE_COLNAMES_STYLE,
      rownamesStyle = TABLE_ROWNAMES_STYLE
    )
  }

}
#return(Results_Workbook) # yes?
}