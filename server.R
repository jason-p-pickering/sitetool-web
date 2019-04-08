library(shiny)
library(shinyjs)
require(magrittr)
require(purrr)
require(dplyr)
require(datimvalidation)
require(ggplot2)


source("./utils.R")

shinyServer(function(input, output, session) {
  
  ready <- reactiveValues(ok = FALSE)
  
  observeEvent(input$file1, {
    shinyjs::enable("validate")
    ready$ok <- FALSE
  }) 
  
  observeEvent(input$validate, {
    shinyjs::disable("validate")
    ready$ok <- TRUE
  })  
  
  
  output$ui <- renderUI({
    
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
        )
      )
    } else {
      fluidPage(
        tags$head(tags$style(".shiny-notification {
                             position: fixed;
                             top: 10%;
                             left: 33%;
                             right: 33%;}")),
        sidebarLayout(
          sidebarPanel(
            shinyjs::useShinyjs(),
            fileInput(
              "file1",
              "Choose SiteTool (Must be XLSX!):",
              accept = c(
                "application/xlsx",
                ".xlsx"
              )
            ),
            tags$hr(),
            actionButton("validate","Validate"),
            tags$hr(),
            downloadButton("downloadFlatPack", "Download FlatPacked SiteTool")
          ),
          mainPanel(tabsetPanel(
            type = "tabs",
            tabPanel("Messages",   tags$ul(uiOutput('messages'))),
            tabPanel("Indicator summary", dataTableOutput("indicator_summary")),
            tabPanel("HTS Modality Summary", plotOutput("modality_summary")),
            tabPanel("Validation rules", dataTableOutput("vr_rules"))
          ))
        ))
  }
})
  
  
  user_input <- reactiveValues(authenticated = FALSE, status = "")
  
  observeEvent(input$login_button, {
    is_logged_in<-FALSE
    user_input$authenticated <-DHISLogin(input$server,input$user_name,input$password)
    flog.info(paste0("User ",input$user_name, " logged in."), name="sitetool")
  })   
  
  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    
    wellPanel(fluidRow(
      img(src='pepfar.png', align = "center"),
      h4("Welcome to the SiteTool Validation App. Please login with your DATIM credentials:")
    ),
    fluidRow(
      textInput("user_name", "Username: ",width = "600px"),
      passwordInput("password", "Password:",width = "600px"),
      actionButton("login_button", "Log in!")
    ))
  })
  
  
  validate<-function() {
    
    shinyjs::hide("downloadFlatPack")
    
    if (!ready$ok) {return(NULL)}
  
    inFile <- input$file1
    messages<-""
    
    if ( is.null( inFile ) ) return( NULL )
    
    messages<-list()
    
    withProgress(message = 'Validating file', value = 0,{
      
      incProgress(0.1, detail = ("Validating your SiteTool"))
      d<-tryCatch({
        datapackr::unPackSiteToolData(inFile$datapath)},
        error = function(e){
          return(e)
        })
      
      
      if (!inherits(d,"error") & !is.null(d)) {
        flog.info(paste0("Initiating validation of ",d$info$datapack_name, "SiteTool."), name="sitetool")
        d$datim$site_data_pretty <- NULL 
        
        incProgress(0.1, detail = ("Checking validation rules"))
        
        d <- validateSiteData(d)
        
        incProgress(0.1, detail = ("Checking data element/ disaggregations"))
        
        d <-validateDataElementDisaggs(d)
        
        incProgress(0.1, detail = ("Checking data element/ organisation units"))
        
        d <- validateDataElementOrgunits(d)
        
        incProgress(0.1, detail = ("Processing mechanisms"))
        
        d <- adornMechanisms(d)
        
        incProgress(0.1, detail = ("Processing data element groups"))
        
        d <- adornDataElements(d)
        
        incProgress(0.1, detail = ("Performing dimensional transformation"))
        
        d <- adornMetdata(d)
        
        incProgress(0.1, detail = ("Processing site information"))
        
        d <- adornSites(d)
        
        shinyjs::show("downloadFlatPack")
        flog.info(paste0("Finished validation of ",d$info$datapack_name, "SiteTool."), name="sitetool")
        if (d$info$has_error) {
          msg<-paste(d$info$datapack_name, "SiteTool had errors.")
        } else{
          msg<-paste(d$info$datapack_name, "SiteTool passed with no errors.")
        }
        flog.info(msg, name="sitetool")
        
      }
    })
    return(d)
    
  }
  
  
  validation_results <- reactive({ validate() })
  
  
  output$indicator_summary<-renderDataTable({
    
    vr<-validation_results()
    
    if (!inherits(vr,"error") & !is.null(vr$datim$site_data_pretty)){
      vr  %>% 
        purrr::pluck(.,"datim") %>%
        purrr::pluck(.,"site_data_pretty") %>%
        dplyr::select(dataElement,agency,value) %>% 
        dplyr::group_by(dataElement,agency) %>% 
        dplyr::summarise(value = format( sum(value) ,big.mark=',', scientific=FALSE)) %>%
        dplyr::arrange(dataElement,agency) 
      
    } else {
      NULL
    }
  })
  
   output$modality_summary <- renderPlot({ 
     
     vr<-validation_results()
     
     if (!inherits(vr,"error") & !is.null(vr$datim$site_data_pretty)){
       
         modalitySummaryChart(vr)
       
     } else {
       NULL
     }
     
   },height = 400,width = 600)

  output$vr_rules <- renderDataTable({ 
    
    vr<-validation_results()
    
    if (!inherits(vr,"error")  & !is.null(vr$datim$vr_rules_check)){
      
      vr %>%
        purrr::pluck(.,"datim") %>%
        purrr::pluck(.,"vr_rules_check")  
      
      } else {
          NULL
        }
    
  })
  
  output$downloadFlatPack <- downloadHandler(
    
    filename = function() {
      
      prefix <- "site_tool_flatpack"
    
      date<-format(Sys.time(),"%Y%m%d_%H%M%S")
      
      paste0(paste(prefix,date,sep="_"),".xlsx")
    },
    content = function(file) {
      
      download_data <- validation_results() %>% 
        purrr::pluck(.,"datim")
      
      datapack_name <-
        validation_results() %>% 
        purrr::pluck(.,"info") %>%
        purrr::pluck(.,"datapack_name")
      
      flog.info(paste("Flatpack requested for",datapack_name), name="sitetool")
      
      openxlsx::write.xlsx(download_data, file = file)
      
    })
    
  
  output$messages <- renderUI({
    
    vr<-validation_results()
    messages<-NULL
    
    if ( is.null(vr) ) {
      return(NULL)
    }
    
    if ( inherits(vr,"error") ) {
      return( paste0("ERROR! ",vr$message) )
      
    } else {
      
      messages <- vr %>%
        purrr::pluck(., "info") %>%
        purrr::pluck(., "warningMsg")
      
      if (!is.null(messages))  {
        lapply(messages, function(x)
          tags$li(x))
      } else
      {
        tags$li("No Issues with Integrity Checks: Congratulations!")
      }
    }
  })
})
