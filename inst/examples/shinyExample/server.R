library(shiny)
library(datasets)
library(XML)
library(ReportingTools)

##custom rendering function for server.R
renderRepTools = function(expr, env=parent.frame(), quoted=FALSE) {
  func <- exprToFunction(expr, env, quoted)
  function(){
    paste(capture.output(func()), collapse="\n")
  }
}


##open the report with the shinyHandlers
htmlrep = HTMLReport(reportDirectory = "./",shortName="bigtest", handlers = shinyHandlers)


###define three .modifyDF functions:
##This function does nothing.
noChange <- function(object, ...){ return(object) }
##this function normalizes the columns of the data frame
normalize <- function(object, ...){
  for (j in 1:dim(object)[2]){
    object[,j] <- (object[,j]-mean(object[,j],na.rm=T))/sd(object[,j],na.rm=T)
  }
  return(object)
}
##This function subtracts the median from each column
subMedian <- function(object, ...){
  for (j in 1:dim(object)[2]){
    object[,j] <- object[,j]-median(object[,j],na.rm=T)
  }
return(object)
}


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
  
   modifyInput <- reactive(function() {
    switch(input$modifyFunction,
           "raw data" = noChange,
           "normalize" = normalize,
           "subtract median" = subMedian)
  })
 
  # The output$summary depends on the datasetInput reactive function, 
  # so will be re-executed whenever datasetInput is re-executed 
  # (i.e. whenever the input$dataset changes)
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
 
  ###use RT to display output
  output$view2 <- renderRepTools({
    publish(datasetInput(), htmlrep, .modifyDF = modifyInput())
  })
})

