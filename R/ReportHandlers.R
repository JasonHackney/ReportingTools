toFileHandlers = new("ReportHandlers",
  finish = function(rep, args)
  {
    if(is.null(args$file))
      file = file.path(rep$reportDirectory, paste(rep$shortName, ".html", sep=""))
    else
      file = args$file
    saveXML(rep$.reportDOM, file = file)
  })

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
