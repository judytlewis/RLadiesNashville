####################################################################################
# This Shiny application is designed to demonstrate some features                   #
# incorporated in the IeDEA Harmonist Toolkit:                                      #
#   1. Summarizing a dataframe in tabbed tables (dynamically)                       #
#   2. Adding functioning buttons in each table row                                 #
#   3. Presenting a modal containing the details related to the clicked row         #
#                                                                                   #
# Jeremy Stephens https://bit.ly/2Ki9r9B contributed the tabbed table method        #
# The idea for clicking on summary tables for detail came from Barbara              #
# Borges at Rstudio::conf2018 https://github.com/bborgesr/rstudio-conf-2018         #
# but that approach doesn't work for tabbed tables.                                 #
# I learned about adding functioning buttons in tables here: https://bit.ly/2IkKuwZ # 
# Note that "tables" in this code refers to IeDEA observational data tables.        #
# Refer to https://ideades.org for information about IeDEA tables and variables.    #
#                                                                                   #
# Judy Lewis, PhD                                                                   #
# May 14, 2018                                                                      #
#####################################################################################

library(shinydashboard)
library(shinyjs)
library(dashboardthemes)
library(tidyverse)
library(tools)
library(DT)
library(filesstrings)



shinyUI <- dashboardPage(title = "RLadies Nashville",
                         dashboardHeader(title = "RLadies Nashville"),
                         
                         # Sidebar panel
                         dashboardSidebar(width = 260,
                                          sidebarMenu(
                                            id = "tabs",
                                            menuItem("STEP 1: Upload file", 
                                                     tabName = "upload"), 
                                            menuItem("STEP 2: Review Errors",
                                                     tabName = "errors")
                                          )
                         ),
                         
                         #main Panel
                         dashboardBody(
                           useShinyjs(),
                           # delete the shinyDashboardThemes line if you prefer not to install the dashboardthemes package
                           #  To install: library(devtools)
                           #              install_github("nik01010/dashboardthemes")
                            shinyDashboardThemes(theme = "grey_light"),
                           
                           tabItems(
                             tabItem(tabName = "upload",
                                     uiOutput("welcomeInfo"),
                                     tags$h3(tags$strong("STEP 1: Upload Files")),
                                     uiOutput("selectFiles")
                                     
                             ),
                             tabItem(tabName = "errors",
                                     fluidRow(uiOutput("errorSummarySep"))
                                    # fluidRow(dataTableOutput("allerrors"))
                             )
                           )
                         )
)


shinyServer <- function(input, output, session){
  # helpers.R contains some variable defitions and helper functions
  source("helpers.R")
 
  # welcomeInfo: UI to describe demo and give link to iedea data model description ------------------------
  output$welcomeInfo <- renderUI({
    fluidRow(
      box(
        width = 7, 
        title = span("RLadies Nashville: Dynamic, interactive, tabbed tables in Shiny", style = "color: #4682B4"),
        tagList(
          tags$h4("This demo is based on a portion of the ", tags$a("IeDEA", href = "https://www.iedea.org/", target="_blank"),
                  " Harmonist Toolkit, a Shiny application designed to facilitate data quality checking, visualization,",
                  " and sharing of observational HIV/AIDS clinical data from around the globe."),
          tags$h4("See ", tags$a("iedeades.org", href = "http://iedeades.org", target="_blank" ),"for a description of IeDEA tables and variables.",
                  "This data model is maintained in ", tags$a("REDCap", href = "https://projectredcap.org/about/", target="_blank" ))
        )
      )
    )
  })
  
  # UI to prompt user to select a Data Quality Error file in csv format----------------------------
  output$selectFiles <- renderUI({
    uploadFileUI <- 
      fluidRow(box(
        solidHeader = TRUE,
        status = "primary",
        width = 4, 
        title = span("Select files", style = "color: white"),
        tagList(
          tags$h4("Please select a .csv file containing errors detected in an IeDEA dataset"),
          fileInput("loaded",
                    "Data file")
        )
      )
      )
    
    if  (resetFileInput$reset){
      # hide the sidebar if the user is uploading a new file;the 2nd tab will not apply until upload complete
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      
      return(
        uploadFileUI
      )
    }
    else if (!is.null(errorTable())){
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      return(
        fluidRow(box(
          width = 4, 
          title = span("Upload complete", style = "color: white"),
          solidHeader = TRUE,
          status = "primary",
          tagList(
            tags$h3(span(icon("check-square-o"),"IeDEA error file uploaded successfully ")),
            actionButton("step2","Continue to Step 2")
          )
        ),
        box(
          width = 4,
          solidHeader = TRUE,
          status = "primary",
          title = span("Restart session", style = "color: white"),
          tagList(
            tags$h3(span("I would like to upload a different IeDEA error file")),
            actionButton("uploadNew","Upload new error file")
          )
        )
        )
      )
    } else if (is.null(errorTable())){
      return(
        uploadFileUI
      )
    }
  })
  
  # resetFileInput---------------------------------------------------------------
  # reactive value -- mechanism to reset states 
  # if user chooses to upload new error file OR if invalid file format
  resetFileInput <- reactiveValues(
    reset = FALSE
  )
  
  observeEvent(input$uploadNew,{
    resetFileInput$reset <- TRUE
  })
  
  observeEvent(input$loaded,{
    resetFileInput$reset <- FALSE
  })
  
  # infile ---------------------------------------------------------------------
  # reactive value: path and name of file chosen by user
  infile <- reactive({
     if (is.null(input$loaded)) {
       return(NULL)
     }
    # user uploaded a file
    file = input$loaded
  })

  # errorTable ---------------------------------------------------------------
  # reactive value: if infile() is valid format, this returns a dataframe of
  # all errors
  errorTable <- reactive({
    if (is.null(infile())) return(NULL)
    inputLink <- infile()
    fileExt <- file_ext(inputLink$name)
    if (fileExt == "csv"){
      myfile <- tryCatch(read_csv(inputLink$datapath,
                                  col_types = cols(.default = "c"),
                                  na = character()),
                         warning = function(w) return(NULL),
                         error = function(e) return(NULL))
    } else {
      errorMessageModal("Invalid file type. Valid file type: .csv")
      resetFileInput$reset <- TRUE
      return(NULL)
    }
    
    if (!is.null(myfile)){
      myfile <- as.data.frame(myfile)
      if (any(!(names(myfile) %in% errorFrameColumnNames))){
        errorMessageModal("Please upload a file conforming to IeDEA Data Quality output format")
        resetFileInput$reset <- TRUE
        return(NULL)
      }
    }
    return(myfile)
  })
  
  output$allerrors <- renderDataTable({
    req(errorTable())
    datatable(errorTable(), rownames=FALSE)
  })
  
  # tableNames --------------------------------------------------------------
  # reactive value: list of names of all tables in errorTable
  tableNames <- reactive({
    req(errorTable())
    tableNames <- sort(unique(errorTable()$table))
  })
  
  # step2 action button: change active tab to "errors"
  observeEvent(input$step2,{
    updateTabItems(session,"tabs", "errors")
  })
  
  
  ###################################################################################################
  ### Code for tabbed table detail UI -- View Detail
  ###################################################################################################
  
  # summaryToShow: generate list of summary tables of errors by tableName, with badges and functioning buttons added
  summaryToShow <- reactive({
    req(tableNames())
    summaryByTable <- lapply(tableNames(), function(x){ 
      summary <- errorTable() %>% 
        filter(table == x) %>% 
        arrange(severity, errorType) %>% 
        group_by(severity, errorType, errorVariable) %>% 
        summarise(number = n()) %>% 
        ungroup() %>% 
        mutate(Error = paste(errorType,": ", errorVariable)) %>% 
        rename(variable = errorVariable) %>% 
        mutate(Notification = makeBadges(severity)) %>% 
        mutate(Option = addShinyInput(actionButton, "button_", 1:n(),
                                      label = "View Detail", 
                                      onclick = 'Shiny.onInputChange(\"select_button\", this.id);
                                         Shiny.onInputChange(\"lastClick\", Math.random())')) 
      return(summary)
    })
    names(summaryByTable) <- tableNames()
    return(summaryByTable)
  })
  
  # display error summary in separate tables, by tableName ------------------------------------------------
  output$errorSummarySep <- renderUI({
    if (resetFileInput$reset) return(NULL)
    req(summaryToShow())
    boxArgs <- lapply(all_iedea_tables, function(x) {  
      if (x %in% tableNames()){
        errors <- errorTable() %>% filter(table == x)
        numberOfErrors <- nrow(errors)
        summary <- summaryToShow()[[x]][,c("Error","Notification", "number", "Option")]
        showTable <- DT::datatable(summary, selection = "single", rownames = FALSE,
                                   colnames = c("Error" = "Error"," " = "Notification", "Count" = "number"," " = "Option"), 
                                   escape = FALSE)
        # designate this table as a Shiny output object with the name "summary_(tableName)"
        output[[paste0("summary_",x)]] <- DT::renderDataTable({showTable})
        
        tabPanel(title = tagList(span(x, style="font-weight: bold"), 
                                 span(numberOfErrors, class="badge", style="background-color: red")), 
                 dataTableOutput(paste0("summary_",x)))
      } else{
        tabPanel(title = tagList(span(x, style="font-weight: bold; color: green"), shiny::icon("check-square-o")), 
                 renderText(paste0("No errors in ",x)))
      } 
    })
    boxArgs$width <- 12
    boxArgs$height <- 20
    boxArgs$id <- "tabset1"  # id of currently selected tab
    
    tagList(  
      tags$h3("Error Summary by Table"),
      fluidRow(
        do.call(tabBox, boxArgs)
      )
    )
  })
  
  # currentTable: reactive variable to determine current tab selected in tabbed set of tables -----
  currentTable <- reactive({
    req(input$tabset1)
    req(errorTable())
    input$tablset1
    tablePlusString <- str_after_first(input$tabset1,"tbl")
    tableName <- paste0("tbl",str_before_first(tablePlusString,"<"))
  })
  
  # Generate error detail modal when "View Detail" clicked ---------------------------------------
  observeEvent(input$lastClick, {
    # first determine which tab is active; which table's summary is displayed
    tableName <- currentTable()
    tableSummary <- summaryToShow()[[tableName]]
    # determine which row was selected in the summary
    selectedRow <- as.numeric(str_after_first(input$select_button,"button_"))
    
    rowData <- tableSummary[selectedRow,]

    details <- errorTable() %>% 
      filter(table== tableName) %>% 
      filter(errorVariable == rowData$variable) %>% 
      filter(errorType == rowData$errorType)
    
    detailsForModal <- makeDetailsPretty(details, rowData$variable)
    
    showModal(modalDialog(
      size = "l",
      title = div(tags$b(paste0("Error detail for variable ",rowData$variable, " in ",tableName)), style = "color: #605ea6;"),
      
      wellPanel(tags$h4(detailsForModal$errorDesc)),
      tags$h4(paste0(rowData$number, " instance(s) of ", rowData$errorType)),
      wellPanel(
        DT::renderDataTable({
          detailsForModal$toShow
        },
        rownames = FALSE,
        selection = "none"
        ) 
      )
    ))
  }, ignoreInit = TRUE)
  
}

# Run the application 
shinyApp(ui=shinyUI, server = shinyServer)
