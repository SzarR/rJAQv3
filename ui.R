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
          "The order of the task analysis is as follows. First you must specify the start/stop of 
          the column/variable limits, so that the appropriate SPSS columns are read into the data
          sheet. Then, we'll read in the SPSS file, parse the task statements, rename variables
          and then finally run the analysis.",
          h3("Specify Variable Limits"),
          "In the fields below, please indicate the first and last column name that correspond to 
          the task analysis area of the job analysis. These two fields are mandatory.",
          splitLayout(
          textInput("rename_begin", label = "First scale, first task statement column name", value = "", width = 160),
          textInput("rename_end", label = "Last scale, last task statement column name", value = "", width = 160)),
          "If you captured duty area weightings, please specify the first and last column of the duty area
          weightings. If not applicable, leave these fields blank.",
          splitLayout(
            textInput("dutyarea_begin", label = "First duty area column name",value = "", width = 160),
            textInput("dutyarea_end", label = "Last duty area column name", value = "", width = 160)
          ),
          "Next, please upload the task, or task/ksao analysis file. The file should be a direct 
          download from the Survey Monkey website.",
          fileInput(inputId = "task", label = "", accept = ".sav",width = 475, 
                    placeholder = "Upload a valid SPSS file"),
          h3("Parse Statement Labels"),
          "Click the button below to allow R to extract statements from the Value Label variable
          column from the SPSS file. Caution, if the length exceeds 325 characters, the statement
          will be cut off. Results should be populated in the Task Statements tab to the right.",
          br(),
          actionButton(inputId = "Parse_Tasks",
                       label = "Extract Task Statements",
                       width = 225,),
          br(),
          h3("Specify Scales"),
          "In the drop-down below, indicate which scale(s) have been  utilized for the JAQ. Select ALL
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
          checkboxInput(
            inputId = 'QC1_Task',
            "This option converts to missing all data cases where a SME indicates NA but 
            continues to fill out the remaining scales. (QC1)",
            value = FALSE,
            width = 400
          ),
          actionButton(
            inputId = "Analyze_Stuff",
            label = "Task Analyze",
            width = 170
          )), 
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Data",  DT::dataTableOutput("pr_table_task")),
                              tabPanel("Task Statements", DT::dataTableOutput("pr_statements_task")),
                              #tabPanel("Renamed Data",  DT::dataTableOutput("pr_renamed_tasks")),
                              tabPanel("Task Results", DT::dataTableOutput("pr_task_analysis"))
                              ))
        ) #sidebar layout

    ), #tab panel

    tabPanel(
      title = "KSAO Analysis",
      sidebarLayout(
        sidebarPanel(
          "We now continue with the KSAO analysis. Please specify first the variables for the various
          columns exactly as you did with the task analysis. This will help import the correct columns
          for analysis.",
          h3("Specify Variable Limits"),
          "In the fields below, please indicate the first and last column name that correspond to 
          the KSAO analysis area of the job analysis.",
          splitLayout(
          textInput("rename_begin_sao", label = "First scale, first Skil/Abil statement",value = "", width = 160),
          textInput("rename_end_sao", label = "Last scale, last Skil/Abil statement", value = "", width = 160)),
          "Do the same for the knowledge statements. If there were no knowledge statements, just
          leave both fields blank.",
          splitLayout(
          textInput("rename_begin_know", label = "First scale, first knowledge statement",value = "", width = 160),
          textInput("rename_end_know", label = "Last scale, last knowledge statement", value = "", width = 160)),
          h3("Parse Statement Labels"),
          "The button below will automatically download the KSAO statements from your SPSS file. This will
          populate the KSAO Statements tab on the right.",
          "Next, please upload a valid SPSS file below for the KSAO analysis. If your Task and KSAOs are
          in one SPSS file, simply re-upload that file again.",
          fileInput(inputId = "ksao", label = "", accept = ".sav",width = 475, 
                    placeholder = "Upload a valid SPSS file"),
          "Click the button below to allow R to extract KSAO statements from the value label variable
          column from the SPSS file. Caution, if the length exceeds 325 characters, the statement
          will be cut off. Results should be populated in the KSAO Statements tab to the right.",
          actionButton(inputId = "Parse_KSAO",label = "Extract KSAO Statements", width = 225,
          ),
          h3("Specify Scales"),
          "In the table below, please indicate which scales have been  utilized for the KSAO analysis. 
          Select all that apply.",
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
          checkboxInput(
            inputId = 'QC1_KSAO',
            "This option converts to missing all data cases where a SME indicates NA but 
            continues to fill out the remaining scales. (QC1)",
            value = FALSE,
            width = 400
          ),
          actionButton(
            inputId = "Analyze_Stuff_ksao",
            label = "Analyze!",
            width = 170
          )), 
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Data",  DT::dataTableOutput("pr_table_ksao")),
                              tabPanel("KSAO Statements", DT::dataTableOutput("pr_statements_ksao")),
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
          actionButton(
            inputId = "Calculate_Weights",
            label = "Get Weights",
            width = 145
          )), 
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Weightings",  DT::dataTableOutput("pr_dutyarea_weightings")),
                              tabPanel("Test Frame", DT::dataTableOutput("test_2"))
                              #tabPanel("Renamed Data",  DT::dataTableOutput("renamed_ksao")),
                              #tabPanel("KSAO Results", DT::dataTableOutput("pr_ksao_analysis"))
        ))
      ) #sidebar layout

    ),

    tabPanel(
      title = "Linkage Analysis",
      sidebarLayout(
        sidebarPanel(
          "Finally, for the linkage analysis, please specify first the variables for the various
          columns exactly as you did with the task/KSAO analysis. This will help import the correct columns
          for analysis.",
          h3("Specify Variable Limits"),
          "In the fields below, please indicate the first and last column name that correspond to 
          the skills portion of the linkage analysis.",
          splitLayout(
            textInput("rename_begin_link_sao", label = "First Skil/Abil linkage statement", width = 160),
            textInput("rename_end_link_sao", label = "Last Skil/Abil linkage statement", width = 160)),
          "In the fields below, please indicate the first and last column name that correspond to 
          the knowledge portion of the linkage analysis.",
          splitLayout(
            textInput("rename_begin_link_know", label = "First knowledge linkage statement", width = 160),
            textInput("rename_end_link_know", label = "last knowledge linkage statement", width = 160)),
          "Next, please upload a valid SPSS file below for the linkage analysis. If your linkage analysis
          is combined with your task/ksao analysis, simply upload the same file again.",
          fileInput(inputId = "link", label = "", accept = ".sav",width = 475,
                    placeholder = "Upload a valid SPSS file"),
          "Next, we rename our columns for easier analysis. By clicking the button, the table on the right
          should automatically update with the new column names.",
          actionButton(
            inputId = "Rename_Variables_LINK",
            label = "Rename Variables",
            width = 220
          ),
          br(),
          h3("Run Analyses"),
          "Both buttons below will execute the matrix calculations and populate the respective tabs
          on the right side of the screen.",
          splitLayout(
          actionButton(
            inputId = "Analyze_Stuff_link_SAO",
            label = "Run Skills/Abilities Analysis",
            width = 225
          ),
          actionButton(
            inputId = "Analyze_Stuff_link_KNOW",
            #style=('padding:4px; font-size:80%'),
            label = "Run Knowledge Areas Analysis",
            width = 240
            ))),
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Data",  DT::dataTableOutput("pr_table_link")),
                              tabPanel("Skil/Abil Matrix", DT::dataTableOutput("pr_linkage_sao")),
                              tabPanel("Knowledge Matrix", DT::dataTableOutput("pr_linkage_know"))
        ))
      ) #sidebar layout

    ) #tab panel

  ) #overall UI