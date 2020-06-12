ui <-
  navbarPage(
    useShinyalert(),
    title = tags$strong("rJAQv3"),
    selected = "Task Analysis",
    windowTitle = "rJAQv3",
    id = 'navbar',
    inverse = TRUE,
    theme = shinytheme("yeti"),
    
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
          "Next, please upload the task, or task/ksao analysis file. The file should be a direct 
          download from the Survey Monkey website.",
          fileInput(inputId = "task", label = "", accept = ".sav",width = 475, 
                    placeholder = "Upload a valid SPSS file"),
          h3("Specify Scales"),
          "In the drop-down below, indicate which scale(s) have been  utilized for the JAQ. Select ALL
          that apply.",
          br(),
          br(),
          selectizeInput(
            'Scale_Choices',
            label = NULL,
            choices = c(
              "NA_",
              "IMP_",
              "FREQ_",
              "REQU_"
            ),
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
            value = TRUE,
            width = 400
          ),
          actionButton(
            inputId = "Analyze_Stuff",
            label = "Task Analyze",
            width = 170
          )), 
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Data",  DT::dataTableOutput("pr_table_task")),
                              tabPanel("Task Results", DT::dataTableOutput("pr_task_analysis"))
                              ))
        ) #sidebar layout

    ), #tab panel

    tabPanel(
      title = "KSAO Analysis",
      sidebarLayout(
        sidebarPanel(
          "We now continue with the KSAO analysis. You must first specify the first scale of the first 
          KSAO statement column name of the first KSAO along with the column name of the last scale of 
          the last KSAO statement.
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
          "Next, please upload a valid SPSS file below for the KSAO analysis. If your Task and KSAOs are
          in one SPSS file, simply re-upload that file again.",
          fileInput(inputId = "ksao", label = "", accept = ".sav",width = 475, 
                    placeholder = "Upload a valid SPSS file"),
          h3("Specify Scales"),
          "In the table below, please indicate which scales have been  utilized for the KSAO analysis. 
          Select all that apply.",
          selectizeInput(
            'Scale_Choices_ksao',
            label = NULL,
            choices = c(
              "NA_",
              "IMP_",
              "REQU_",
              "DIFF_",
              "RvR_"
            ),
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
            value = TRUE,
            width = 400
          ),
          actionButton(
            inputId = "Analyze_Stuff_ksao",
            label = "KSAO Analyze",
            width = 170
          )), 
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Data",  DT::dataTableOutput("pr_table_ksao")),
                              tabPanel("KSAO Results", DT::dataTableOutput("pr_ksao_analysis"))
        ))
      ) #sidebar layout

    ), #tab panel
    tabPanel(
      title = "Duty Areas",
      sidebarLayout(
        sidebarPanel(
          "Please specify the column limits for duty areas.",
          splitLayout(
            textInput("dutyarea_begin", label = "First duty area column name",value = "", width = 160),
            textInput("dutyarea_end", label = "Last duty area column name", value = "", width = 160)
          ),
          "Now, specify if these columns are in the task or KSAO analysis SPSS file that
          you uploaded in the previous chapter.",
          selectInput(inputId = 'Where_DA',
                      label = "",
                      choices = c("Task Analysis File", "KSAO Analysis File"),
                      selected = 0,
                      width = 200),
          "Finally, specify the last task statement number for each duty area
          in your JAQ. For example, if task 25 is the last statement
          in duty area 1, and 54 is the last statement of duty area 2, you would
          include in the box: 26, 54, etc. There should be a total of numbers
          equivalent to the number of duty areas in your JAQ. All numbers must
          be seperated with a comma.",
          textAreaInput("DA_Limits", label = h3("Duty Area Limits"), placeholder = "26, 54, etc.",width = 300),
          "Click the button below to extract the duty area weightings and set
          limits on the duty area start/stop locations.",
          br(),
          br(),
          actionButton(
            inputId = "Calculate_Weights",
            label = "Get Weights",
            width = 145
          )), 
         mainPanel(tabsetPanel(type = "tabs",
                               tabPanel("Weightings",
                                        DT::dataTableOutput("pr_dutyarea_weightings"))
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
          h3("Parse Statement Labels"),
          "Click the button below to allow R to extract the SAOs/Knowledge areas from the Value Label variable
          column from the SPSS file. Caution, if the length exceeds 325 characters, the statement
          will be cut off. Results should be populated in the Statements tab to the right.",
          br(),
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
            label = "Run Knowledge Areas Analysis",
            width = 240
            ))),
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Data",  DT::dataTableOutput("pr_table_link")),
                              tabPanel("SAAL_Raw", DT::dataTableOutput("pr_linkage_raw_sao")),
                              tabPanel("SAAL_Weighted", DT::dataTableOutput("pr_linkage_sao")),
                              tabPanel("JDKL_Raw", DT::dataTableOutput("pr_xr_sao")),
                              tabPanel("JDKL_Weighted", DT::dataTableOutput("pr_linkage_know"))
        ))
      ) #sidebar layout

    ), #tab panel
               tabPanel(
                 "Demographics",
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     selectInput(inputId = 'Which_Demographic_File',
                                 label = "Pull Variables From:",
                                 choices = c("Task Analysis", "KSAO Analysis", "Linkage Analysis"),
                                 selected = 0,
                                 width = 200),
                     uiOutput('dynamic_ui'),
                     actionButton('goButton', 'Add to Table', icon('cloud-upload'))
                   ),
                   mainPanel(id = 'dataset02', dataTableOutput('table3'))
                   )
), 

    tabPanel(title = "Save Results", 
             "This chapter provides an interface for the user to download the results from
             their job analysis that was run in the previous four chapters.",
             br(), 
             br(),
             "If rJAQ detects the existence of an analysis, it will
             automatically include that analysis in the downloaded XLSX file.",
             br(),
             selectInput(inputId = "ExporterFormat", 
                         label="Select File Format", 
                         choices = c("XLSX"), 
                         selected = "XLSX", 
                         width = 150),
             downloadButton('downloadData', 
                            'Download File', 
                            width = 125)
),
    tabPanel(title = "Quit", 
             value="stop", 
             icon = icon("circle-o-notch"))

  ) #overall UI