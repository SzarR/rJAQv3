server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30 * 1024 ^ 2) 

# Task Analysis Panel -----------------------------------------------------
  
  values <<- reactiveValues(dat_task = NULL)
  
  observeEvent(input$task, {
    values$dat_task <- haven::read_sav(input$task$datapath)
  })

  # Display Raw SPSS File Table.
  output$pr_table_task <-
    DT::renderDataTable({
      values$dat_task
    },
    style = "bootstrap",
    server = TRUE,
    options = list(searching = FALSE,
                   paging = FALSE,
                   autoWidth = TRUE))

# Renaming SPSS File Variable Names ---------------------------------------

  # Obtain start of rename column.
  observeEvent(input$rename_begin, {
    rename_begin <<- input$rename_begin
  })

  # Obtain start of rename column.
  observeEvent(input$ClientName, {
    ClientName <<- input$ClientName
  })

  # Obtain start of rename column.
  observeEvent(input$AnalysisRank, {
    AnalysisRank <<- input$AnalysisRank
  })

  # Obtain end of rename column.
  observeEvent(input$rename_end, {
    rename_end <<- input$rename_end
  })

  # Obtain scales utilized.
  observeEvent(input$Scale_Choices, {
    Scale_Choices <<- input$Scale_Choices
  })

  # Obtain scales utilized.
  observeEvent(input$Which_Demographic_File, {
    Which_Demographic_File <<- input$Which_Demographic_File
  })

  observeEvent(input$Rename_Variables, {

    message("The scale order for task analysis was as follows: ", 
            Scale_Choices)

    temp <- rename_variables(values$dat_task, section = 'task')
    values$dat_task <- temp
    rm(temp)
    
    if(exists("TaskNumbers") == TRUE){
    get_statements(values$dat_task, section = 'task')
    }
    
    updateTabsetPanel(session, "task_tabs",
                      selected = "Task Results")
    
  })

# Analysis of Task JAQ Statements ---------------------------------
  
  observeEvent(input$QC1_Task, {
    QC1_Task <<- input$QC1_Task
  })

  Tasks_Analyzed <<- eventReactive(input$Analyze_Stuff, {
    
    if(QC1_Task == TRUE){
      values$dat_task <- qualitycontrol_step1(datum = values$dat_task, section = 'task')
    }
    jaq_analyze(values$dat_task, section = 'task')
  })
  
  # Display task analysis results.
  output$pr_task_analysis <-
    DT::renderDataTable({
      Tasks_Analyzed()
    },
    style = "bootstrap",
    editable = 'cell',
    server = TRUE,
    extensions = c("Buttons"),
    rownames = FALSE,
    options = list(
      dom = 'Bfrtip',
      searching = FALSE,
      paging = FALSE,
      autoWidth = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  
# KSAO Analysis Panel ---------------------------------------------
  
  observeEvent(input$QC1_KSAO, {
    QC1_KSAO <<- input$QC1_KSAO
  })
  
  # Obtain start of SAO rename column.
  observeEvent(input$rename_begin_sao, {
    rename_begin_sao <<- input$rename_begin_sao
  })
  
  # Obtain end of SAO rename column.
  observeEvent(input$rename_end_sao, {
    rename_end_sao <<- input$rename_end_sao
  })
  
  # Obtain start of rename column for knowledge.
  observeEvent(input$rename_begin_know, {
    rename_begin_know <<- input$rename_begin_know
  })
  
  # Obtain end of rename column for knowledge.
  observeEvent(input$rename_end_know, {
    rename_end_know <<- input$rename_end_know
  })
  
  # Obtain scales utilized.
  observeEvent(input$Scale_Choices_ksao, {
    Scale_Choices_ksao <<- input$Scale_Choices_ksao
  })

  observeEvent(input$ksao, {
    values$dat_ksao <- haven::read_sav(input$ksao$datapath)
  })
  
  # Display KSAO Table
  output$pr_table_ksao <-
    DT::renderDataTable({
      values$dat_ksao
    },
    style = "bootstrap",
    server = TRUE,
    options = list(
      dom = 'Bfrtip',
      searching = FALSE,
      paging = FALSE,
      autoWidth = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  
  observeEvent(input$Rename_Variables_KSAO, {

    temp <- rename_variables(values$dat_ksao, section = 'ksao')
    values$dat_ksao <- temp

    get_statements(values$dat_ksao,section = 'ksao')
    
    updateTabsetPanel(session, "ksao_tabs",
                      selected = "KSAO Results")

  })

  # Analysis of KSAO Statements ----------------------------------
  
  KSAOs_Analyzed <<- eventReactive(input$Analyze_Stuff_ksao, {
    shiny::req(Description.Frame, values$dat_ksao)
    
    # First check if QC1_KSAO active
    if(QC1_KSAO == TRUE){
    values$dat_ksao <- qualitycontrol_step1(datum = values$dat_ksao, section = 'ksao')
     }
    jaq_analyze(values$dat_ksao, section = 'ksao')
  })

  # Display KSAO analysis results.
  output$pr_ksao_analysis <-
    DT::renderDataTable({
      KSAOs_Analyzed()
    },
    style = "bootstrap",
    editable = 'cell',
    server = TRUE,
    extensions = c("Buttons"),
    rownames = FALSE,
    options = list(dom = 'Bfrtip',
                   searching = FALSE,
                   paging = FALSE,
                   autoWidth = TRUE,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ),
    class = 'display')

# Duty Area Panel ---------------------------------------------------------
  
  # Obtain duty area limits
  observeEvent(input$DA_Limits, {
    DA_Limits <<- as.numeric(unlist(strsplit(input$DA_Limits,",")))
  })
  
  # Get duty area weightings
  DutyAreas <<- eventReactive(input$Calculate_Weights, {

    # This can be automated in the future. Where the string match is, 
    # there you shall extract from.
    if (input$Where_DA == 'Task Analysis File') {
      get_weightings(values$dat_task)
    } else {
      get_weightings(values$dat_ksao)
    }
    
  })

  # Obtain start of rename column for duty areas
  observeEvent(input$dutyarea_begin, {
    dutyarea_begin <<- input$dutyarea_begin
  })
  
  # Obtain end of rename column for duty areas
  observeEvent(input$dutyarea_end, {
    dutyarea_end <<- input$dutyarea_end
  })
  
  output$pr_dutyarea_weightings <- DT::renderDataTable({
    DutyAreas()
  },
  style = "bootstrap",
  server = FALSE,
  options = list(
    dom = 'Bfrtip',
    pageLength = 25,
    searching = FALSE,
    paging = FALSE,
    ordering = FALSE,
    rownames = FALSE,
    autoWidth = FALSE,
    escape = FALSE,
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))

# Demographics Data Download ------------------------------------------------
  
  demo_task_df <<- data.frame(Demographic_Name=character(0),
                              Column_Name=character(0),
                              Analysis_Type=integer(0))
  
  output$table3 <- renderDataTable( df3(),
                                    options = list(pageLength = 25,
                                                   searching = FALSE,
                                                   paging = FALSE,
                                                   ordering = FALSE,
                                                   rownames = FALSE,
                                                   autoWidth = FALSE,
                                                   escape = FALSE))
  
  output$dynamic_ui <- renderUI({
    req(demo_task_df)
    
    my_cols <- colnames(demo_task_df)
    
    ui_elems <- purrr::map(my_cols, ~{
      if (class(demo_task_df[[.x]]) %in% c("factor", "character")){
        output <- textInput(
          inputId = paste("input", .x, sep = "_"),
          label = .x,
          value = NULL
        )
      } else if (class(demo_task_df[[.x]]) %in% c("integer", "numeric")){
        output <- selectInput(
          inputId = paste("input", .x, sep = "_"),
          label = .x,
          choices = list("Calculate Mean/Average" = 1,
                         "Calculate Frequencies" = 2, 
                         selected = NULL)
        )
      } else output <- NULL
      
      return(output)
    })
    
    return(tagList(ui_elems))
  })
  
  df3 <- eventReactive(input$goButton, {
    my_cols <- colnames(demo_task_df)
    my_inputs <- paste("input", my_cols, sep = "_")
    
    req(demo_task_df)
    walk(my_inputs, ~req(input[[.x]]))
    
    new_data <- purrr::map2_dfc(
      my_cols, 
      my_inputs,
      ~{
        output <- data.frame(input[[.y]])
        colnames(output) <- .x

        return(output)
      }
    )

    demo_task_df <<- rbind(demo_task_df, new_data)
    demo_task_df

  }, ignoreNULL = FALSE) 

# Linkage Analysis Panel --------------------------------------------------

# Upload link SPSS File ---------------------------------------------------

  observeEvent(input$link, {

    if (input$rename_begin_link_sao != "" &
        input$rename_begin_link_know != "") {
      values$dat_link <- haven::read_sav(input$link$datapath) %>%
        dplyr::select(rename_begin_link_sao:rename_end_link_sao,
                      rename_begin_link_know:rename_end_link_know)

      # If only Skil/Abil Present, then this.
    } else if (input$rename_begin_link_sao != "" &
               input$rename_begin_link_know == "") {
      values$dat_link <- haven::read_sav(input$link$datapath) %>%
        dplyr::select(rename_begin_link_sao:rename_end_link_sao)

      # If only Knowledge present, then this.  
    } else if (input$rename_begin_link_know != "" &
               input$rename_begin_link_sao == "") {
      values$dat_link <- haven::read_sav(input$link$datapath) %>%
        dplyr::select(rename_begin_link_know:rename_end_link_know)
    }
  })
  
  # Display Raw SPSS File.
  output$pr_table_link <-
    DT::renderDataTable({
      values$dat_link
    },
    style = "bootstrap",
    server = TRUE,
    options = list(pageLength = 25,
                   autoWidth = TRUE))
  
  # Obtain first column of SAAL Linkage
  observeEvent(input$rename_begin_link_sao, {
    rename_begin_link_sao <<- input$rename_begin_link_sao
  })
  
  # Obtain last column of SAAL Linkage
  observeEvent(input$rename_end_link_sao, {
    rename_end_link_sao <<- input$rename_end_link_sao
  })
  
  # Obtain first column of KNOW Linkage
  observeEvent(input$rename_begin_link_know, {
    rename_begin_link_know <<- input$rename_begin_link_know
  })
  
  # Obtain first column of KNOW Linkage
  observeEvent(input$rename_end_link_know, {
    rename_end_link_know <<- input$rename_end_link_know
  })

  observeEvent(input$Rename_Variables_LINK, {
    temp <- rename_variables(values$dat_link, section = 'link')
    values$dat_link <- temp
  })

  Link_SAO_Analyzed <<- eventReactive(input$Analyze_Stuff_link_SAO, {
    laq_analyze(values$dat_link, skills = TRUE, knowledge = FALSE)
  })

  Link_KNOW_Analyzed <<- eventReactive(input$Analyze_Stuff_link_KNOW, {
    laq_analyze(values$dat_link, skills = FALSE, knowledge = TRUE)
  })
  
  # Render Raw SAAL Table.
  output$pr_linkage_raw_sao <-
    DT::renderDataTable({
      SAAL_Matrix
    },
    style = "bootstrap",
    server = TRUE,
    options = list(
      paging = FALSE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))

  # Render Weighted SAAL Table.
  output$pr_linkage_sao <-
    DT::renderDataTable({
      Link_SAO_Analyzed()
    },
    style = "bootstrap",
    server = TRUE,
    options = list(
      paging = FALSE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  
  # Render Raw JDKL Table.
  output$pr_linkage_raw_know <-
    DT::renderDataTable({
      JDKL_Matrix
    },
    style = "bootstrap",
    server = TRUE,
    options = list(
      paging = FALSE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  
  # Render Weighted JDKL Table.
  output$pr_linkage_know <-
    DT::renderDataTable({
      Link_KNOW_Analyzed()
    },
    style = "bootstrap",
    server = TRUE,
    options = list(
      paging = FALSE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  
# Data Download Parameters ------------------------------------------------
  
  # Result from clicking the Download File.
  output$downloadData <- downloadHandler(
    filename = "JAQ_Workbook.xlsx",
    content = function(file) {
      switch(
        input$ExporterFormat,
        XLSX = {
          export_workbook()
          saveWorkbook(wb = Results_Workbook, file = file)
        }
      )
    }
   )

# Quit Button -------------------------------------------------------------

  observe({
    if (input$navbar == "stop")
      stopApp()
  })

} # Server close.