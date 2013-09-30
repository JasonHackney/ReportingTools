##tells shiny how to use ReportingTools for ui.R
custHeaderPanel = function(title, windowTitle =title, js= NULL, css=NULL)
{
  mytlist = c(lapply(js, function(x) tags$script(HTML(paste(readLines(x), collapse="\n")))),
              lapply(css, function(x) tags$style(HTML(paste(readLines(x), collapse="\n")))))
   tagList(tag("head",mytlist), div(class = "span12", 
                                                  style = "padding: 10px 0px;", h1(title)))
}

