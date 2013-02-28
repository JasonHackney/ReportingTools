#init expects node, args
#addElement and removeElement expect node, name, args
#finish expects report, args



toFileHandlers = new("ReportHandlers",
  finish = function(rep, args)
  {
    if(is.null(args$file))
      file = file.path(rep$basePath, rep$reportDirectory, 
          paste(rep$shortName, ".html", sep=""))
    else
      file = args$file
    #not sure if the splash should be an actual report element...
    #also, don't want to add more than one if we are working off an existing report
    #I'll put this check in here for now
    if(!(".RTsplash" %in% names(rep$.report)))
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
    elementids  = getNodeSet(rep$.reportDOM, "//div[@class='ReportingTools']/@id")
    hnodes =  getNodeSet(rep$.reportDOM, "/html/body/h1|/html/body/h2|/html/body/h3")
    tabcontents = newXMLNode("div", attrs = list(class="TableOfContents"),
      kids = unlist(lapply(elementids, function(id)
        {
          list(
            newXMLNode("a", id, attrs = list(href = paste0("#", id))),
            newXMLNode("br"))
        }))
      )
      
    if(length(hnodes))
      addSibling(hnodes[[1]], tabcontents, after=TRUE)
    else
      {
        body = getNodeSet(rep$.reportDOM, "/html/body")[[1]]
        addChildren(body, tabcontents, at=1)
      }
    #not sure if the splash should be an actual report element...
    #also, don't want to add more than one if we are working off an existing report
    #I'll put this check in here for now
    if(!(".RTsplash" %in% names(rep$.report)))
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

toConnectionHandlers = new("ReportHandlers",
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
