library(shiny)
library(ePort)

shinyServer(function(input, output) {
    
  selectedData = reactive({
    c(input$radio1, input$radio2)
  })
  
  output$plot <- renderPlot({
    if (input$run==0) return()
    KeyFile = input$key
    DataFile = input$filenm
    LoFile = input$lo
    
    for (i in DataFile$datapath) report_routine(KeyFile$datapath[1],datafile=i,rewrite=FALSE,skip=NULL,LOfile=LoFile$datapath[1],knitfile=NULL)
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(rnorm(30),rnorm(30),
         col = rbinom(30,3,0.3),
         pch = 20, cex = 3)
    text(0:1,0:1,labels=selectedData())
  })
})
