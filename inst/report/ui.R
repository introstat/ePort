library(shiny)

shinyUI(fluidPage(
  titlePanel("Selecting Files to Generate a Report"),
  fluidRow(
    column(3, wellPanel(
      h4("Answer Key"),
      fileInput('key', 'Choose txt file',
                accept=c('text/csv', 
                         'text/plain', 
                         '.txt'))
    )),
    column(3, wellPanel(
      h4("Data File"),
      fileInput('filenm', 'Choose csv file', multiple=TRUE,
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv'))
    )),
    column(3, wellPanel(
      h4("Learning Outcomes"),
      fileInput('lo', 'Choose txt file',
                accept=c('text/csv', 
                         'text/plain', 
                         '.txt'))
    ))
  ),
  hr(),
  fluidRow(
    column(2, radioButtons("radio1", label = h4("Section Setting"), 
                          choices = list("One Section" = 1, "Sections Crossed" = 2), 
                          selected = 1)),
    column(2, radioButtons("radio2", label = h4("Topic Setting"), 
                          choices = list("One Topic" = 1, "Topics Crossed" = 2), 
                          selected = 1))
  ),
  actionButton("run", label = "Run"),
  hr(),
  plotOutput("plot")
))
