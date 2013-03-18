
#problem with specifying new path/dir/etc: The report we read in will have the old values encoded in its links/image tags/etc. Do we want to scan the DOM and try to change these?

##For now I just won't allow it within this file. One could of course write custom handlers that manipulate the dom and change these attributes after the HTML has been created/read in.
readReport = function(reportFile, handlers = fileHandlers(reportFile), .toHTML = NULL, .toDF = NULL, .modifyDF = NULL, title)
  {
    dom = htmlParse(reportFile)

    #basePath + reportDirectory are unidentifiable together, so we set basePath to nothing and put the entire directory into reportDirectory
    basePath = ""

    reportDirectory = dirname(reportFile)
    
    shortName = gsub("(.*)\\..*", "\\1",basename(reportFile))

    rep = HTMLReport(shortName = shortName, reportDirectory = reportDirectory, 
        basePath = basePath, handlers = handlers, 
        .toHTML = .toHTML, .toDF = .toDF, .modifyDF  = .modifyDF)
    rep$.reportDOM = dom
    #find the report elements within the html!!
    repElNodes = getNodeSet(dom, "//div[@class='ReportingTools']")
    names(repElNodes) = sapply(repElNodes, function(x) xmlAttrs(x)[["id"]])
    class(repElNodes) = "list"
    rep$.report = repElNodes

      
    if(missing(title))
      {
        tnode = getNodeSet(dom, "/html/head/title")
        if(length(tnode))
          title = xmlValue(tnode[[1]])
        else
          title = rep$title
      }
    rep$title = title
    rep
  }
