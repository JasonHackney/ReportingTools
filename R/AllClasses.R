setClass("BaseReport",
    representation = representation(
        shortName           = "character",
        title               = "character",
        reportDirectory    = "character"
    ),
    prototype = list(
        shortName           = "coolProject",
        title               = "My cool project",
        reportDirectory    = "."
    )
)

setClass("HTMLReport", contains = "BaseReport",
    representation = representation(
        baseUrl = "character",
        basePath = "character",
        page    = "connection"
    )
)

HTMLReport <- function(shortName, title = NULL, reportDirectory = ".",
    baseUrl = "localhost", basePath = ".", page = NULL,
    link.css = NULL, link.javascript = NULL, overwrite.js=TRUE)
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
    
    if(is.null(page)){
        pageDir <- file.path(basePath, reportDirectory)
        page <- openPage(paste(shortName, "html", sep="."), 
            dirname = pageDir, title = title, link.css = link.css,
            link.javascript = link.javascript)
    }
    hwrite(title, page = page, heading=2, br=TRUE)
    htmlRep <- new("HTMLReport", shortName = shortName,
        title = title, reportDirectory = reportDirectory, 
        baseUrl = baseUrl, basePath = basePath, page = page)
}

setClass("CSVFile", contains = "BaseReport")

CSVFile <- function(shortName, title = "", reportDirectory = ".")
{
    csvRep <- new("CSVFile", shortName = shortName, 
        reportDirectory = reportDirectory, title = title)
    return(csvRep)
}

setClass("DataPackage", contains = "BaseReport",
    representation = representation(
        version         = "character",
        dependencies    = "character",
        author          = "character",
        maintainer      = "character",
        license         = "character",
        description     = "character",
        package.Rd      = "character"
    ),
    prototype = list(
        version         = "0.0.1",
        dependencies    = "Biobase",
        author          = "nobody <nobody@nothing.net>",
        maintainer      = "nobody",
        license         = "No license specified",
        description     = "No description necessary",
        package.Rd      = ""
    )
)

DataPackage <- function(shortName, title = "", reportDirectory = ".",
    version = "0.0.1", dependencies = c("Biobase"), license = "",
    description = "", author = "nobody", 
    maintainer = "nobody <nobody@nothing.net>")
{
    dependencies <- unique(dependencies)
    dataPkg <- new("DataPackage", shortName = shortName, 
        reportDirectory = reportDirectory, title = title,
        version = version, dependencies = dependencies, license = license,
        description = description, author = author, maintainer = maintainer)
    ## Need to do some basic setup of the data package:
    ## Make directories, create the DESCRIPTION and NAMESPACE,
    ## add some documentation into the package.Rd file
    .safe.dir.create(reportDirectory)
    pkg.dir <- path(dataPkg)
    .safe.dir.create(pkg.dir)
    
    man.dir <- file.path(pkg.dir, "man")
    .safe.dir.create(man.dir)

    vignettes.dir <- file.path(pkg.dir, "vignettes")
    .safe.dir.create(vignettes.dir)
    
    data.dir <- file.path(pkg.dir, "data")
    .safe.dir.create(data.dir)
    
    ## I should move this to a finish method, just to make sure that
    ## any changes to the package are put into the description.
    ## Or add this to all accessor methods. Oy!
    description.str <- paste("Package: ", shortName, "\nVersion: ", version, 
        "\nTitle: ", title, "\nAuthor: ", author, "\nMaintainer: ", maintainer,
        "\nDepends: ", dependencies, "\nLicense: ", license, "\nType: Package",
        "\nLazyLoad: yes\nDescription: ", description, sep="")
    description.fn <- file.path(pkg.dir, "DESCRIPTION")
    
    namespace.str <- paste("#Export all variables that do not start with a period.", 
        "exportPattern(\"^[^\\\\.]+\")", sep = "\n")
    out <- file(file.path(pkg.dir, "NAMESPACE"), "wt") 
    writeLines(namespace.str, out)
    close(out)
    ## Also need to make a namespace file here.
    
    cat(description.str, file=description.fn)
    
    return(dataPkg)
}
