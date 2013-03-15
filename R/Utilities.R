makeReportPath = function(basePath, reportDirectory, shortName)
  {
    filename = paste0(shortName, ".html")
    if(missing(basePath) || is.null(basePath))
      return( paste0(reportDirectory,"/", filename ))

    if(substr(reportDirectory, 1, 1) == "/")
      stop("Non-NULL baseDirectory in combination with absolute reportDirectory is not supported.")

    paste(basePath, reportDirectory,filename, sep="/")
  }


