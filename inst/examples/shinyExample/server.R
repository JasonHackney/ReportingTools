library(shiny)
library(datasets)
library(XML)
library(ReportingTools)

renderRepTools = function(expr, env=parent.frame(), quoted=FALSE) {
  func <- exprToFunction(expr, env, quoted)
  
  function(){
    paste(capture.output(func()), collapse="\n")
  }
}

myrep = HTMLReport(reportDirectory = "./",shortName="bigtest", handlers = shinyHandlers)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # By declaring datasetInput as a reactive function we ensure that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers (it 
  #     only executes a single time)
  #  3) When the inputs change and the function is re-executed, the
  #     new result is compared to the previous result; if the two are
  #     identical, then the callers are not notified
  #
  datasetInput <- reactive(function() {
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
 
  # The output$summary depends on the datasetInput reactive function, 
  # so will be re-executed whenever datasetInput is re-executed 
  # (i.e. whenever the input$dataset changes)
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
 
  
  output$view2 <- renderRepTools({
    publish(datasetInput(), myrep, name="view2tabdiv")
  })
})
