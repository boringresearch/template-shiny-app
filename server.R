library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)

# function definition ----
somefunc <- function(input1, input2 = "_"){
  return(input2)
}

server <- shinyServer(function(input, output, session) {
  
  # Init0. eg.change some variables ----
  v <- reactiveValues(upload = 0)
  
  observeEvent(input$submit, {
    v$upload <- 1
  })
  
  # Sections
  # I. load data ----
  result <- reactive({
    #result <- read.csv("data/xxx.csv")
    return(result)
  })
  
  # p1. show a button once uploaded a file ----
  output$uiaction <- renderUI({
    column(6, offset = 0, actionButton("uiaction", "Run"))
  })
  
  # p1. load default rna-seq data ----
  df <- eventReactive(input$load_default_data, {
    return(df)
  })
  
  
  # p1. submit gene symbol with ID and genename ----
  datapasted <- eventReactive(input$submit, {
    return(datapasted)
  })
  
  # visitor counts ----
  output$counter <- renderValueBox({
    if (!file.exists("data/counter.Rdata"))
      counter <- 0
    else
      load(file="data/counter.Rdata")
    counter <- counter + 1
    save(counter,file="data/counter.Rdata")
    valueBox(paste0(counter), 
             "Visitors", icon = icon("user"), color = 'green')
  })
  
  session$allowReconnect(TRUE)
  
})

