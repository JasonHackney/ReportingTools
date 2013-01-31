#lifted the openPage function from hwriter package by Gregoire Pau

makePageStart = function ( title = "Title!", link.javascript = NULL, 
    link.css = NULL, css = NULL, head = NULL, charset = "utf-8", 
    lang = "en", head.attributes = NULL, body.attributes = NULL) 
{
    doctype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
    meta = hmakeTag("meta", NULL, `http-equiv` = "Content-Type", 
        content = paste("text/html; charset=", charset, sep = ""), 
        newline = FALSE)
    if (!is.null(link.javascript)) 
        link.javascript = paste(hmakeTag("script", language = "JavaScript", 
            src = link.javascript), collapse = "\n")
    if (!is.null(link.css)) 
        link.css = paste(hmakeTag("link", rel = "stylesheet", 
            type = "text/css", href = link.css), collapse = "\n")
    if (!is.null(css)) 
        css = paste(hmakeTag("style", css), collapse = "\n")
    head = paste(meta, hmakeTag("title", title), head, link.javascript, 
        link.css, css, sep = "\n")
    head = do.call(hmakeTag, c(list("head", head, newline = TRUE), 
        head.attributes))
    bodyStart = do.call(hmakeTag, c(list("body", NULL), body.attributes))
    bodyStart = substr(bodyStart, 1, regexpr("</body>", bodyStart) - 
        1)
    paste(doctype, "<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='", 
        lang, "' lang='", lang, "'>", head, bodyStart, sep = "")
}


makePageEnd = function()
  {
    "\n</body>\n</html>"
  }

#This will be called from the initialize method of HTMLReportRef to create the initial internal DOM representation
startHTMLReport =  function(shortName, title = NULL, reportDirectory = ".",
    baseUrl = "localhost", basePath = ".", page = NULL,
    link.css = NULL, link.javascript = NULL, overwrite.js=TRUE,
                       ...)
{
    shortName <- sub(".html$", "", shortName)
    if(is.null(title))
        title <- shortName

    if(substr(reportDirectory,1,1) == "~"){
        reportDirectory <- path.expand(reportDirectory)
    }
    
    if(basePath == "" | is.na(basePath))
        basePath <- "."

    ## Not sure what to do here when basePath is set, but reportDirectory needs
    ## to be rewritten... Probably should just throw an execption    
    if(substr(reportDirectory, 1, 1) == "/" && 
        (basePath=="" || is.na(basePath) || basePath == ".")){
            basePath <- "/"
    }
    
    if(substr(basePath,1,1) == "~"){
        basePath <- path.expand(basePath)
    }
    
    pageDir <- file.path(basePath, reportDirectory)
    
    if(is.null(link.javascript)){
        js.libloc <- Sys.getenv("REPORTINGTOOLSJSLIB")
        if(js.libloc == ""){
            javascript.files <- c(system.file("extdata/jslib/jquery-1.8.0.min.js",
                package="ReportingTools"), 
                system.file("extdata/jslib/jquery.dataTables-1.9.3.js",
                    package="ReportingTools"),
                system.file("extdata/jslib/jquery.dataTables.columnFilter.js",
                    package="ReportingTools"),
                system.file("extdata/jslib/jquery.dataTables.plugins.js",
                    package="ReportingTools"),
                system.file("extdata/jslib/jquery.dataTables.reprise.js",
                    package="ReportingTools"))

            jsDir <- file.path(pageDir,"jslib")
            if(!file.exists(jsDir))
                dir.create(jsDir, recursive=TRUE)
            
            file.copy(javascript.files, jsDir, overwrite=overwrite.js)
            javascript.files <- sub(".*extdata/","",javascript.files)
          } else {
            javascript.files <- paste(js.libloc, c("jquery-1.8.0.min.js",
                "jquery.dataTables-1.9.3.js","jquery.dataTables.columnFilter.js",
                "jquery.dataTables.plugins.js","jquery.dataTables.reprise.js"), 
                sep="/")
        }
        link.javascript <- javascript.files
    }
    if(is.null(link.css)){
        css.libloc <- Sys.getenv("REPORTINGTOOLSCSSLIB")
        if(css.libloc == ""){
            css.files <- c(system.file("extdata/csslib/reset-min.css", 
                    package="ReportingTools"), 
                system.file("extdata/csslib/reprise.table.css", 
                    package="ReportingTools"))
            css.files = system.file("extdata/csslib/reprise.table.css", package = "ReportingTools")
            cssDir <- (file.path(pageDir,"csslib"))
            .safe.dir.create(cssDir, recursive=TRUE)
            
            file.copy(css.files,cssDir, overwrite=overwrite.js)
            css.files <- sub(".*extdata/","",css.files)

            css.png.package.dir <- c(system.file("extdata/csslib/images/", package="ReportingTools"))
            css.pngs <- list.files(path=css.png.package.dir)
            css.pngs <-  paste(css.png.package.dir,css.pngs, sep="")

            cssPngsDir <- (file.path(pageDir,"csslib/images"))
            .safe.dir.create(cssPngsDir, recursive=TRUE)            
            file.copy(css.pngs,cssPngsDir)
            
          } else {
            css.files <- paste(css.libloc, 
                c("reset-min.css", "reprise.table.css"), sep="/")
        }
        link.css <- css.files
    }
    

    pagestart = makePageStart(title = title, link.css = link.css, link.javascript = link.javascript)
    #DOM (HTML) representation of the (currently empty) report
    node = xmlRoot(htmlParse(paste(pagestart, makePageEnd(), collapse = "\n")))
    htitle = hwrite(title, heading=2, br=TRUE)
    body = node[["body"]]
    child = xmlRoot(htmlParse(htitle))[["body"]][[1]]
    addChildren(body, child )

    node
  }
 
