library(shiny)


custHeaderPanel = function(title, windowTitle =title, js= NULL, css=NULL)
{
  mytlist = c(lapply(js, function(x) tags$script(HTML(paste(readLines(x), collapse="\n")))),
              lapply(css, function(x) tags$style(HTML(paste(readLines(x), collapse="\n")))))
   tagList(tag("head",mytlist), div(class = "span12", 
                                                  style = "padding: 10px 0px;", h1(title)))
}
# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
    custHeaderPanel("ReportingTools", 
                  js = list.files(system.file("extdata/jslib", package="ReportingTools"),
                                 full.names=TRUE),
                  css = list.files(system.file("extdata/csslib", package="ReportingTools"),
                    pattern="bootstrap.css", full.names=TRUE),
                  ),
 
   #### #### #### ####                
 ####here and below are shiny specific commands:
  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
    sidebarPanel(
                 selectInput("dataset", "Choose a dataset:", 
                             choices = c("rock", "pressure", "cars")),
                 selectInput("modifyFunction", "Choose a modification function:", 
                             choices = c("raw data", "normalize", "subtract median"))
                 ),
  
  
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
     mainPanel(
               verbatimTextOutput("summary"), 
               htmlOutput("view2")
               )
                        ))
