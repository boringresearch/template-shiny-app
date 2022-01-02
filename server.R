library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
# loading ->  %>% withSpinner(color="blue")
library(shinycssloaders)
library(dplyr) 
library(plotly)
library(DT)
library(magick)
library(networkD3)
library(rintrojs)
library(dplyr)
library(ggplot2)
library(pheatmap)
library("gplots")
library(RColorBrewer)
library(ComplexHeatmap)
library(circlize)
library(grid)
library(VennDiagram)
library(UpSetR)
library(ggplotify)
library(biomaRt)
library(tidyverse)
library(processx)
library(org.Hs.eg.db)
library(formattable)
library(GeneNetworkBuilder)
library(simpIntLists)
library(networkD3)
library(fgsea)

# splitvec for split input with . ----
splitvec <- function(vector, split, select, merge = "_"){
  processed <- sapply(vector, function(x){
    separated <- unlist(strsplit(x, split = split))[select]
    if (length(separated) > 1){
      return(paste(separated, collapse = merge))
    } else
      return(separated)
  })
  processed <- unname(processed)
  return(processed)
}

server <- shinyServer(function(input, output, session) {
  
  # init ----
  v <- reactiveValues(upload = 0)
  observeEvent(input$submit, {
    v$upload <- 1
  })
  observeEvent(input$load_default_data, {
    v$upload <- 'quickload'
  })
  # https://stackoverflow.com/questions/42460509/r-shiny-passing-youtube-links-to-iframe
  output$video <- renderUI({
    #tags$iframe(src = "youtube-url",width = 600, height = 400)
  })
  # I. load data ----
  ######result######
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
  
  
  # counter ----
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

