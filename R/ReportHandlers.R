#init expects node, args
#addElement and removeElement expect node, name, args
#finish expects report, args


.makeSplash <- function(){
    hwLink <- hwrite("hwriter", link = "http://www.ebi.ac.uk/~gpau/hwriter/")
    hwVersion <- sessionInfo()$otherPkgs[["hwriter"]]$Version
    rtLink <- hwrite("ReportingTools", 
        link = "http://research-pub.gene.com/ReportingTools/")
    rtVersion <- sessionInfo()$otherPkgs[["ReportingTools"]]$Version
    splash <- paste("\n<br/><br/><font size=\"-2\">(Page generated on ", 
                date(), " by ", rtLink, " ", rtVersion, " and ", 
                hwLink, " ", hwVersion, ")</font>", sep = "")
    splash
}


fileHandlers = function(filename, basePath, reportDirectory, shortName, ...)
  {
    if(missing(filename))
      filename = makeReportPath(basePath, reportDirectory, shortName)
    
    new("ReportHandlers", location = filename,
        finish = function(rep, args)
        {
          rep$addElement(".RTsplash", .makeSplash())
          saveXML(rep$.reportDOM, file=filename)
        },
        args = list(...))
  }

fileWIndexHandlers = function(filename, basePath, reportDirectory, shortName, ...)
  {
    if(missing(filename))
      filename = makeReportPath(basePath, reportDirectory, shortName)
    new("ReportHandlers", location = filename,
        addElement = function(node, name, args)
        {
          addChildren(node, newXMLNode("a", attrs=list(name=name)), at = 1)
        },
        finish = function(rep, args)
        {
       
          elementids  = xpathSApply(rep$.reportDOM, "//div[@class='ReportingTools container']/@id")
          hnodes =  getNodeSet(rep$.reportDOM, "/html/body/h1|/html/body/h2|/html/body/h3")
          tabcontents = Link(elementids, paste0("#", elementids))
          
          if(length(hnodes))
            addSibling(hnodes[[1]], tabcontents, after=TRUE)
    else
      {
        body = getNodeSet(rep$.reportDOM, "/html/body")[[1]]
        addChildren(body, tabcontents, at=1)
      }
                                        #not sure if the splash should be an actual report element...
          rep$addElement(".RTsplash", .makeSplash())
          saveXML(rep$.reportDOM, file = filename)
        },
        args = list(...)
        )
  }


connectionHandlers = function(con, ...)
  {
    new("ReportHandlers", location = con,
        init = function(node, args)
        {
                                        #ugly and horrible :*(
          chars = as.character( node )
          chars = gsub("</body>.*</html>", "", chars)
          cat(chars, file = con)
  },
        addElement = function(node, name, args)
        {
          cat(as.character(node), file=con)
        },
        finish = function(node, args)
        {
          cat(.makeSplash(), file=con)
          cat("</body>\n</html>", file=con)

        },
        args = list(...))
  }

knitrHandlers = function(location,...)
  {
    if(missing(location))
      {
        location = makeReportPath(...)
      }
    new("ReportHandlers", location = location,
        init = function(node, args)
        {
          grabScriptAndStyle(node)
          
        },
        addElement = function(node, name, args)
        {
          cat(saveXML(node))
        },
        args = list(...)
        )
  }

shinyHandlers = function(appDir = getwd(),...)
  {
    new("ReportHandlers", location = appDir,
        addElement = function(node, name, args)
        {
          cat(saveXML(node))
          cat(paste0("<script>\n",
                     "$('#",
                     name, " .dataTable').each(configureTable);\n",
                     "</script>\n"))
        }, args = list(...)
        )
  }
