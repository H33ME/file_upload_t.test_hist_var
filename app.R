## an app that;
## lets user upload a file
## select a variable
## perform a t.test on that variable
## updateSelectInput() to fill in the available variables
## plot histogram for each numeric variable

library(tidyverse)
library(shiny)

ui<- fluidPage(
  sidebarLayout(
  sidebarPanel(
    fileInput("file","Data",buttonLabel = "Upload...",accept = ".csv"),
    selectInput("variable","select variable",choices =colnames(file) ),
    textInput("delim","delimiter(leave for app to guess)",value=""),
    sliderInput("bins","Bins",min=0,max=100,value=30)
  ),
  mainPanel(
    tableOutput("table"),
    verbatimTextOutput("ttest"),
    plotOutput("hist")
  )
)
)

server<- function(input,output){
  dat<- reactive({
    req(input$file)
    
    delim<- if(input$delim=="")NULL else input$delim
    vroom::vroom(input$file$datapath,delim = delim)
    
  })
  observeEvent(dat(),{
    choices<- names(dat())
    updateSelectInput(inputId = "variable",choices = choices)
  })
  output$ttest<- renderPrint({
   
   t.test(dat()[[input$variable]])
    
  })
  output$table<- renderTable({
    head(dat())
  })
  output$hist<- renderPlot({
      ggplot()+
      geom_histogram(aes(dat()[[input$variable]],binwidth =input$bins) )+
      labs(title = paste0("Histogram of", "'",input$variable,"'"),
           x=paste0("'",input$variable,"'"))
  })
}

shinyApp(ui,server)
