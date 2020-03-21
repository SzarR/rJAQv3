server <- function(input, output, session) {

# Task Analysis Panel -----------------------------------------------------

# Upload SM SPSS File -----------------------------------------------------

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
    rename_variables(dat_task())
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

  # Extract labels from SPSS file.
  Statements_Task <<- eventReactive(input$Parse_Tasks, {
    get_statements(dat_task())
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

} # Server close.