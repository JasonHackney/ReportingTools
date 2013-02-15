toFileHandlers = new("ReportHandlers",
  finish = function(rep, args)
  {
    if(is.null(args$file))
      file = file.path(rep$basePath, rep$reportDirectory, 
          paste(rep$shortName, ".html", sep=""))
    else
      file = args$file
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
  addElement = function(node, args)
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
  addElement = function(node, args)
  {
    cat(saveXML(node))
  }
  )
