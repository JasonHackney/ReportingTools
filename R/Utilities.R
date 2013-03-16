makeReportPath = function(basePath, reportDirectory, shortName)
{
      
    filename = paste0(shortName, ".html")
    if(substr(reportDirectory,1,1) == "~"){
        reportDirectory <- path.expand(reportDirectory)
    }

    if(missing(basePath) || is.null(basePath))
        return( file.path(reportDirectory, filename ))

    if(substr(reportDirectory, 1, 1) == "/")
        stop("Non-NULL baseDirectory in combination with absolute reportDirectory is not supported.")
    if(sum(grepl("[A-Za-z]:", reportDirectory)) > 0 & .Platform$OS.type == "windows")
        stop("Non-NULL baseDirectory in combination with absolute reportDirectory is not supported.")

    paste(basePath, reportDirectory,filename, sep="/")
}


