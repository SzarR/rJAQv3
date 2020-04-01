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
          textInput("rename_begin", label = "", placeholder = "Paste firt task variable name",value = "q0011_0001_0001", width = 160),
          textInput("rename_end", label = "", placeholder = "Paste last task variable name", value = "q0021_0008_0004", width = 160),
          "In the table below, please indicate which scales have been  utilized for the JAQ. Select all
          that apply.",
          br(),
          br(),

          selectizeInput(
            'Scale_Choices',
            label = NULL,
            choices = c(
              "APP",
              "IMP",
              "FREQ",
              "REQU"
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
        
    ), #tab panel
    
    tabPanel(
      title = "KSAO Analysis",
      sidebarLayout(
        sidebarPanel(
          "Please upload a valid SPSS file below. The file should be a direct download from the
          Survey Monkey website. Leave the raw variable labels and confirm the pre-filled areas
          below match your particular survey upload.",
          fileInput(inputId = "ksao", label = "", accept = ".sav",width = 475, 
                    placeholder = "Upload a valid SPSS file"),
          "Click the button below to allow R to extract statements from the Value Label variable
          column from the SPSS file. Caution, if the length exceeds 325 characters, the statement
          will be cut off.",
          br(),
          br(),
          actionButton(inputId = "Parse_KSAO",label = "Extract KSAO Statements", width = 225,
          ),
          br(),
          h3("Variable Renamer"),
          "In the fields below, please indicate the variable in the SPSS file name that corresponds to
          the applicability scale for the first SAO statement and the last SAO statement.",
          textInput("rename_begin_sao", label = "", placeholder = "Paste firt task variable name",value = "q0011_0001_0001", width = 160),
          textInput("rename_end_sao", label = "", placeholder = "Paste last task variable name", value = "q0022_0006_0004", width = 160),
          "In the field below, please indicate the variable in the SPSS file that corresponds to
          the first knowledge area in your file",
          textInput("rename_begin_know", label = "", placeholder = "Paste firt task variable name",value = "q0023_0001_0001", width = 160),
          textInput("rename_end_know", label = "", placeholder = "Paste last task variable name", value = "q0031_0004_0005", width = 160),
          "In the table below, please indicate which scales have been  utilized for the JAQ. Select all
          that apply.",
          br(),
          br(),
          selectizeInput(
            'Scale_Choices_ksao',
            label = NULL,
            choices = c(
              "APP",
              "IMP",
              "REQU",
              "DIFF",
              "RvR"
            ),
            selected = NULL,
            width = 340,
            options = list(create = TRUE, maxItems = 5)
          ),
          actionButton(
            inputId = "Rename_Variables_KSAO",
            label = "Rename Variables",
            width = 220
          ), 
          actionButton(
            inputId = "Analyze_Stuff_ksao",
            label = "Analyze!",
            width = 170
          )), 
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Raw Data",  DT::dataTableOutput("table_ksao")),
                              tabPanel("KSAO Statements", DT::dataTableOutput("pr_statements_ksao")),
                              tabPanel("Renamed Data",  DT::dataTableOutput("renamed_ksao")),
                              tabPanel("KSAO Results", DT::dataTableOutput("pr_ksao_analysis"))
        ))
      ) #sidebar layout
      
    ), #tab panel
    
    tabPanel(
      title = "Duty Areas",
      sidebarLayout(
        sidebarPanel(
          "Please specify the first and last duty area column in your SPSS file.
          This will extract the duty area labels in conjunction with the weights
          to calculate the average DA weights.",
          textInput("dutyarea_begin", label = "", placeholder = "Paste firt duty area column",value = "q0023_0001", width = 160),
          textInput("dutyarea_end", label = "", placeholder = "Paste last duty area column", value = "q0023_0011", width = 160),
          actionButton(
            inputId = "Calculate_Weights",
            label = "Get Weights",
            width = 145
          )), 
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Weightings",  DT::dataTableOutput("test_1")),
                              tabPanel("Testy", DT::dataTableOutput("test_2"))
                              #tabPanel("Renamed Data",  DT::dataTableOutput("renamed_ksao")),
                              #tabPanel("KSAO Results", DT::dataTableOutput("pr_ksao_analysis"))
        ))
      ) #sidebar layout
      
    ),
    
    tabPanel(
      title = "Linkage Analysis",
      sidebarLayout(
        sidebarPanel(
          "Please upload a valid SPSS file below. The file should be a direct download from the
          Survey Monkey website. Leave the raw variable labels and confirm the pre-filled areas
          below match your particular survey upload.",
          fileInput(inputId = "link", label = "", accept = ".sav",width = 475,
                    placeholder = "Upload a valid SPSS file"),
          br(),
          h3("Variable Renamer"),
          "In the fields below, please indicate the variable in the SPSS file name that corresponds to
          the applicability scale for the first SAO statement and the last SAO statement.",
          textInput("rename_begin_link_sao", label = "", placeholder = "Paste firt task variable name",value = "q0011_0001", width = 160),
          textInput("rename_end_link_sao", label = "", placeholder = "Paste last task variable name", value = "q0021_0011", width = 160),
          "In the field below, please indicate the variable in the SPSS file that corresponds to
          the first knowledge area in your file",
          textInput("rename_begin_link_know", label = "", placeholder = "Paste firt task variable name",value = "q0022_0001_0001", width = 160),
          textInput("rename_end_link_know", label = "", placeholder = "Paste last task variable name", value = "q0032_0009_0001", width = 160),
          "In the table below, please indicate which scales have been  utilized for the JAQ. Select all
          that apply.",
          br(),
          br(),
          selectizeInput(
            'Scale_Choices_link',
            label = NULL,
            choices = c(
              "IMP"
            ),
            selected = 'IMP',
            width = 120,
            options = list(create = TRUE, maxItems = 1)
          ),
          actionButton(
            inputId = "Rename_Variables_LINK",
            label = "Rename Variables",
            width = 220
          ),
          br(),
          br(),
          actionButton(
            inputId = "Parse_link",
            label = "Parse Labels",
            width = 220
          ),
          actionButton(
            inputId = "Analyze_Stuff_link_SAO",
            label = "Analyze!",
            width = 170
          ),
          actionButton(
            inputId = "Analyze_Stuff_link_KNOW",
            label = "Analyze!",
            width = 170
            )),
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Raw Data",  DT::dataTableOutput("table_link")),
                              tabPanel("Renamed Data",  DT::dataTableOutput("table_link_renamed")),
                              tabPanel("Skil/Abil Matrix", DT::dataTableOutput("pr_linkage_sao")),
                              tabPanel("Knowledge Matrix", DT::dataTableOutput("pr_linkage_know"))
        ))
      ) #sidebar layout

    ) #tab panel
    
    
  ) #overall UI
