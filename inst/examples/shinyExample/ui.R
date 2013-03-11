library(shiny)

custHeaderPanel = function(title, windowTitle=title, js=NULL, css = NULL)
{
  #allhtags = c(lapply(js, function(x) tags$script(HTML(paste(readLines(x), collapse="\n")))), 
   #           lapply(css, function(x) tags$style(HTML(paste(readLines(x), collapse="\n"))))
    #          )
    allhtags = c(tags$script(HTML("alert('hi there!');")), 
                 tags$title("A title"))
  #tagList(tag("head", c(tags$script(HTML("alert('its working!');")),
   #                     allhtags, tags$title(windowTitle))),
    tagList(tag("head", tags$script(HTML("alert('hi there!');"), type="text/javascript")),  
  div(class = "span12", 
              style = "padding: 10px 0px;", h1(title)))
}
custHeaderPanel2 = function(title, windowTitle =title, js= NULL, css=NULL)
{
  mytlist = c(#lapply(c("hi", "there"), function(x) tags$script(HTML(paste("alert('", x, "');")))),
              lapply(js, function(x) tags$script(HTML(paste(readLines(x), collapse="\n")))),
              lapply(css, function(x) tags$style(HTML(paste(readLines(x), collapse="\n")))))
   tagList(tag("head",mytlist), div(class = "span12", 
                                                  style = "padding: 10px 0px;", h1(title)))
}
# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  #headerPanel("Reactivity"),
  
  custHeaderPanel2("ReportingTools", 
                  js = c(system.file("extdata/jslib/jquery-1.8.0.min.js",
                                       package="ReportingTools"), 
                         system.file("extdata/jslib/jquery.dataTables-1.9.3.js",
                                       package="ReportingTools"),
                         system.file("extdata/jslib/jquery.dataTables.columnFilter.js",
                                       package="ReportingTools"),
                         system.file("extdata/jslib/jquery.dataTables.plugins.js",
                                       package="ReportingTools"),
                         system.file("extdata/jslib/jquery.dataTables.reprise.js",
                                       package="ReportingTools")),
                  css = c(system.file("extdata/csslib/reset-min.css", 
                                       package="ReportingTools"), 
                                   system.file("extdata/csslib/reprise.table.css", 
                                       package="ReportingTools"))
  ),
 
  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
    textInput("caption", "Caption:", "Data Summary"),
    
    selectInput("dataset", "Choose a dataset:", 
                choices = c("rock", "pressure", "cars")),
    
    numericInput("obs", "Number of observations to view:", 10)
  ),
  
  
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    #h3(verbatimTextOutput("caption")), 
    tableOutput("caption"),
    verbatimTextOutput("summary"), 
    
    #tableOutput("view"),
    htmlOutput("view2")
  )
))
