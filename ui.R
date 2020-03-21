ui <-
  navbarPage(
    title = tags$strong("rJAQv3"),
    selected = "Task Analysis",
    windowTitle = "rJAQv3",
    inverse = TRUE,
    theme = shinytheme("yeti"),
    
    tabPanel(
      tags$strong(h3("Job Analysis Initilization Panel")),
      title = "Initilization",
      sidebarLayout(
        sidebarPanel(
          "User Customizable Information",
          br(),
          br(),
          textInput(
            inputId = "ClientName",
            label = "Client Name",
            value = "Rochester, NY",
            width = 425
          ),
          textInput(
            inputId = "AnalysisRank",
            label = "Rank",
            value = "Firefighter/Driver",
            width = 375
          ),
          br(),
          br(),
          "Quality Control Parameters"
        ),
        mainPanel(DT::dataTableOutput("X"))
      )
    ),
    
    tabPanel(
      title = "Task Analysis",
      sidebarLayout(
        sidebarPanel(
          "Please upload a valid SPSS file below. The file should be a direct download from the
          Survey Monkey website. Leave the raw variable labels and confirm the pre-filled areas
          below match your particular survey upload.",
          fileInput(inputId = "task", label = "", accept = ".sav",width = 475, 
                    placeholder = "Upload a valid SPSS file"),
          "Click the button below to allow R to extract statements from the Value Label variable
          column from the SPSS file. Caution, if the length exceeds 325 characters, the statement
          will be cut off.",
          br(),
          br(),
          actionButton(inputId = "Parse_Tasks",label = "Extract Task Statements", width = 225,
                       ),
          br(),
          h3("Variable Renamer"),
          "In the fields below, please indicate the variable in the SPSS file name that corresponds to
          NA_1, and again, indicate the last task statement that must be renamed, for example, REQU_194.",
          textInput("rename_begin", label = "", placeholder = "Paste firt task variable name",value = "q0011_0001_0001", width = 225),
          textInput("rename_end", label = "", placeholder = "Paste last task variable name", value = "q0021_0008_0004", width = 225),
          "In the table below, please indicate which scales have been  utilized for the JAQ. Select all
          that apply.",
          br(),
          br(),

          selectizeInput(
            'Scale_Choices',
            label = NULL,
            choices = c(
              "Applicability",
              "Importance",
              "Frequency",
              "Required upon Promotion"
            ),
            selected = NULL,
            width = 340,
            options = list(create = TRUE, maxItems = 4)
          ),
          
          actionButton(
            inputId = "Rename_Variables",
            label = "Rename Variables",
            width = 170
          ), 
          h3("Quality Control"),
          checkboxGroupInput(
            "checkGroup",
            label = "",
            choices = list(
              "Quality Control Checkpoint #1" = 1,
              "Quality Control Checkpoint #2" = 2,
              "Quality Control Checkpoint #3" = 3
            ),
            selected = NULL
          ),
          actionButton(
            inputId = "Analyze_Stuff",
            label = "Analyze!",
            width = 170
          )), 
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Raw Data",  DT::dataTableOutput("table_task")),
                              tabPanel("Renamed Data",  DT::dataTableOutput("renamed_tasks")),
                              tabPanel("Task Statements", DT::dataTableOutput("pr_statements_task")),
                              tabPanel("Task Results", DT::dataTableOutput("pr_task_analysis"))
                              ))
        ) #sidebar layout
        
    ) #tab panel
    
  ) #overall UI
