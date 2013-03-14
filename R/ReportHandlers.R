#init expects node, args
#addElement and removeElement expect node, name, args
#finish expects report, args


.makeSplash <- function(){
    hwLink <- hwrite("hwriter", link = "http://www.embl.de/~gpau/hwriter/index.html")
    hwVersion <- sessionInfo()$otherPkgs[["hwriter"]]$Version
    rtLink <- hwrite("ReportingTools", 
        link = "http://research-pub.gene.com/ReportingTools/")
    rtVersion <- sessionInfo()$otherPkgs[["ReportingTools"]]$Version
    splash <- paste("\n<br/><br/><font size=\"-2\">(Page generated on ", 
                date(), " by ", rtLink, " ", rtVersion, " and ", 
                hwLink, " ", hwVersion, ")</font>", sep = "")
    splash
}


fileHandlers = function(filename, ...)
  {
    new("ReportHandlers", location = filename,
        finish = function(rep, args)
        {
          rep$addElement(".RTsplash", .makeSplash())
          saveXML(rep$.reportDOM, file=filename)
        },
        args = list(...))
  }

fileWIndexHandlers = function(filename, ...)
  {
    new("ReportHandlers", location = filename,
        addElement = function(node, name, args)
        {
          addChildren(node, newXMLNode("a", attrs=list(name=name)), at = 1)
        },
        finish = function(rep, args)
        {
       
          elementids  = xpathSApply(rep$.reportDOM, "//div[@class='ReportingTools']/@id")
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
    new("ReportHandlers",
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

shinyHandlers = function(...)
  {
    new("ReportHandlers", location = NULL,
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


if(FALSE)
  {
fileHandlers = new("ReportHandlers",
  finish = function(rep, args)
  {
    if(is.null(args$file))
      file = file.path(rep$basePath, rep$reportDirectory, 
          paste(rep$shortName, ".html", sep=""))
    else
      file = args$file
    #not sure if the splash should be an actual report element...
    rep$addElement(".RTsplash", .makeSplash())
    saveXML(rep$.reportDOM, file = file)
  })

fileWIndexHandlers = new("ReportHandlers",
  addElement = function(node, name, args)
  {
    addChildren(node, newXMLNode("a", attrs=list(name=name)), at = 1)
  },
  finish = function(rep, args)
  {
    if(is.null(args$file))
      file = file.path(rep$basePath, rep$reportDirectory, 
          paste(rep$shortName, ".html", sep=""))
    else
      file = args$file
    elementids  = xpathSApply(rep$.reportDOM, "//div[@class='ReportingTools']/@id")
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
    saveXML(rep$.reportDOM, file = file)
  })

.makeSplash <- function(){
    hwLink <- hwrite("hwriter", link = "http://www.embl.de/~gpau/hwriter/index.html")
    hwVersion <- sessionInfo()$otherPkgs[["hwriter"]]$Version
    rtLink <- hwrite("ReportingTools", 
        link = "http://research-pub.gene.com/ReportingTools/")
    rtVersion <- sessionInfo()$otherPkgs[["ReportingTools"]]$Version
    splash <- paste("\n<br/><br/><font size=\"-2\">(Page generated on ", 
                date(), " by ", rtLink, " ", rtVersion, " and ", 
                hwLink, " ", hwVersion, ")</font>", sep = "")
    splash
}



connectionHandlers = new("ReportHandlers",
  init = function(node, args)
  {
    #ugly and horrible :*(
    chars = as.character( node )
    chars = gsub("</body>.*</html>", "", chars)
    cat(chars, file = args$connection)
  },
  addElement = function(node, name, args)
  {
    cat(as.character(node), file=args$connectio)
  },
  finish = function(node, args)
  {
    cat("</body>\n</html>", file=args$connection)

  })

 
knitrHandlers = new("ReportHandlers",
  init = function(node, args)
  {
    grabScriptAndStyle(node)

  },
  addElement = function(node, name, args)
  {
    cat(saveXML(node))
  }
  )

 shinyHandlers = new("ReportHandlers",
   addElement = function(node, name, args)
   {
     cat(saveXML(node))
     cat(paste0("<script>\n",
               "$('#",
               name, " .dataTable').each(configureTable);\n",
               "</script>\n"))
   }
   )
}
