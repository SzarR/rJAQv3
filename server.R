tserver <- function(input, output, session) {

# Task Analysis Panel -----------------------------------------------------
# Upload Task SPSS File -----------------------------------------------------

  dat_task <- reactive({
    if(is.null(input$task)) return(NULL)
    haven::read_sav(input$task$datapath)
  })
 
  # Display Raw SPSS File Table.
  output$table_task <-
    DT::renderDataTable({
      dat_task()
    },
    style = "bootstrap",
    server = TRUE,
    options = list(pageLength = 25,
                   autoWidth = TRUE))
  
# Renaming SPSS File Variable Names ---------------------------------------
  
  # Obtain start of rename column.
  observeEvent(input$rename_begin, {
    rename_begin <<- input$rename_begin
  })

  # Obtain end of rename column.
  observeEvent(input$rename_end, {
    rename_end <<- input$rename_end
  })

  # Obtain scales utilized.
  observeEvent(input$Scale_Choices, {
    Scale_Choices <<- input$Scale_Choices
  })

  # Re-name variables.
  dat_task_renamed <<- eventReactive(input$Rename_Variables, {
    rename_variables(dat_task(),section = 'task', knowledge = FALSE)
  })
  
  # Display re-named variables
  output$renamed_tasks <-
    DT::renderDataTable({
      dat_task_renamed()
    },
    style = "bootstrap",
    editable = 'cell',
    server = TRUE,
    rownames = FALSE,
    options = list(pageLength = 50,
                   autoWidth = TRUE))

# Parse Task Statements from SPSS Labels ----------------------------------

  # Extract task labels from SPSS file.
  Statements_Task <<- eventReactive(input$Parse_Tasks, {
    get_statements(dat_task(), section = 'task')
  })

  # Display Task Statements/Description.
  output$pr_statements_task <-
    DT::renderDataTable({
      Statements_Task()
    },
    style = "bootstrap",
    editable = 'cell',
    server = TRUE,
    rownames = FALSE,
    options = list(pageLength = 50,
                   autoWidth = TRUE))
  
# Analysis of Task JAQ Statements ----------------------------------
  Tasks_Analyzed <- eventReactive(input$Analyze_Stuff, {
    jaq_analyze(dat_task_renamed(),section = 'task')
  })
  
  # Display task analysis results.
  output$pr_task_analysis <-
    DT::renderDataTable({
      Tasks_Analyzed()
    },
    style = "bootstrap",
    editable = 'cell',
    server = TRUE,
    rownames = FALSE,
    options = list(pageLength = 50,
                   autoWidth = TRUE))
  
# KSAO Analysis Panel ---------------------------------------------
  
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
    rename_end_know<<- input$rename_end_know
  })
  
  # Obtain scales utilized.
  observeEvent(input$Scale_Choices_ksao, {
    Scale_Choices_ksao <<- input$Scale_Choices_ksao
  })
  
  # Extract KSAO labels from SPSS file.
  Statements_KSAO <<- eventReactive(input$Parse_KSAO, {
    get_statements(dat_ksao(),section = 'ksao')
  })
  
  dat_ksao <- reactive({
    if(is.null(input$ksao)) return(NULL)
    haven::read_sav(input$ksao$datapath)
  })
  
  # Display Raw SPSS File Table.
  output$table_ksao <-
    DT::renderDataTable({
      dat_ksao()
    },
    style = "bootstrap",
    server = TRUE,
    options = list(pageLength = 25,
                   autoWidth = TRUE))
  
  # Display KSAO Statements/Description.
  output$pr_statements_ksao <-
    DT::renderDataTable({
      Statements_KSAO()
    },
    style = "bootstrap",
    editable = 'cell',
    server = TRUE,
    rownames = FALSE,
    options = list(pageLength = 50,
                   autoWidth = TRUE))
  
  # Display re-named variables
  output$renamed_ksao <-
    DT::renderDataTable({
      dat_ksao_renamed()
    },
    style = "bootstrap",
    editable = 'cell',
    server = TRUE,
    rownames = FALSE,
    options = list(pageLength = 50,
                   autoWidth = TRUE))

  # Re-name variables.
  dat_ksao_renamed <<- eventReactive(input$Rename_Variables_KSAO, {
    rename_variables(dat_ksao(), section = 'ksao', knowledge = TRUE) #Knowledge statements attached? That will trigger the KNOW argument.
  })

  # Analysis of KSAO JAQ Statements ----------------------------------
  KSAOs_Analyzed <- eventReactive(input$Analyze_Stuff_ksao, {
    jaq_analyze(dat_ksao_renamed(), section = 'ksao')
  })

  # Display KSAO analysis results.
  output$pr_ksao_analysis <-
    DT::renderDataTable({
      KSAOs_Analyzed()
    },
    style = "bootstrap",
    editable = 'cell',
    server = TRUE,
    rownames = FALSE,
    options = list(pageLength = 50,
                   autoWidth = TRUE))




# Duty Area Panel ---------------------------------------------------------

# Get duty area weightings
  DutyAreas <- eventReactive(input$Calculate_Weights, {
    get_weightings(dat_task())
  })
  
  # Obtain start of rename column.
  observeEvent(input$dutyarea_begin, {
    dutyarea_begin <<- input$dutyarea_begin
  })
  
  # Obtain end of rename column.
  observeEvent(input$dutyarea_end, {
    dutyarea_end <<- input$dutyarea_end
  })
  
  output$test_1 <- DT::renderDataTable({
    DutyAreas()
  },
  style = "bootstrap",
  editable = 'cell',
  server = FALSE,
  options = list(pageLength = 25,
                 searching = FALSE,
                 paging = FALSE,
                 ordering = FALSE,
                 rownames = FALSE,
                 autoWidth = FALSE,
                 escape = FALSE))
  
  output$test_2 <- DT::renderDataTable({
    DutyAreas()
  },
  style = "bootstrap",
  editable = 'cell',
  server = FALSE,
  options = list(pageLength = 25,
                 searching = FALSE,
                 paging = FALSE,
                 ordering = FALSE,
                 rownames = FALSE,
                 autoWidth = FALSE,
                 escape = FALSE))
  
# Linkage Analysis Panel --------------------------------------------------
  
# Upload link SPSS File -----------------------------------------------------
  
  dat_link <- reactive({
    if(is.null(input$link)) return(NULL)
    haven::read_sav(input$link$datapath)
  })
  
  # Display Raw SPSS File Table.
  output$table_link <-
    DT::renderDataTable({
      dat_link()
    },
    style = "bootstrap",
    server = TRUE,
    options = list(pageLength = 25,
                   autoWidth = TRUE))
  
  # Obtain end of rename column.
  observeEvent(input$rename_begin_link_sao, {
    rename_begin_link_sao <<- input$rename_begin_link_sao
  })
  
  # Obtain end of rename column.
  observeEvent(input$rename_end_link_sao, {
    rename_end_link_sao <<- input$rename_end_link_sao
  })
  
  # Obtain end of rename column.
  observeEvent(input$rename_begin_link_know, {
    rename_begin_link_know <<- input$rename_begin_link_know
  })
  
  # Obtain end of rename column.
  observeEvent(input$rename_end_link_know, {
    rename_end_link_know <<- input$rename_end_link_know
  })
  
  # # Extract link labels from SPSS file.
  # Statements_link <<- eventReactive(input$Parse_link, {
  #   get_statements(dat_link_renamed(),section = 'link', knowledge = TRUE)
  # })

  # Re-name variables.
  dat_link_renamed <<- eventReactive(input$Rename_Variables_LINK, {
    rename_variables(dat_link(),section = 'link', knowledge = TRUE)
  })

  Link_SAO_Analyzed <<- eventReactive(input$Analyze_Stuff_link_SAO, {
    laq_analyze(dat_link_renamed(), skills = TRUE, knowledge = FALSE)
  })

  Link_KNOW_Analyzed <<- eventReactive(input$Analyze_Stuff_link_KNOW, {
    laq_analyze(dat_link_renamed(), skills = FALSE, knowledge = TRUE)
  })

  # Display Raw SPSS File Table.
  output$table_link_renamed <-
    DT::renderDataTable({
      dat_link_renamed()
    },
    style = "bootstrap",
    server = TRUE,
    options = list(pageLength = 25,
                   autoWidth = TRUE))

  # Display Raw SPSS File Table.
  output$pr_linkage_sao <-
    DT::renderDataTable({
      Link_SAO_Analyzed()
    },
    style = "bootstrap",
    server = TRUE,
    options = list(dom = 't'))

  # Display Raw SPSS File Table.
  output$pr_linkage_know <-
    DT::renderDataTable({
      Link_KNOW_Analyzed()
    },
    style = "bootstrap",
    server = TRUE,
    options = list(dom = 't'))

} # Server close.