setMethod("name",
    signature = signature(
        object = "BaseReport"
    ),
    definition = function(object){
        return(object@shortName)
    }
)

setMethod("name<-",
    signature = signature(
        object = "BaseReport",
        value = "character"
    ),
    definition = function(object, value){
        object@shortName <- value
        return(object)
    }
)

setMethod("name<-",
    signature = signature(
        object = "HTMLReport",
        value = "character"
    ),
    definition = function(object, value){
        value <- sub('.html$', '', value)
        object@shortName <- value
        return(object)
    }
)

setMethod("name<-",
    signature = signature(
        object = "CSVFile",
        value = "character"
    ),
    definition = function(object, value){
        value <- sub('.csv$', '', value)
        object@shortName <- value
        return(object)
    }
)

setMethod("title",
    signature = signature(
        main = "BaseReport"
    ),
    definition = function(main, ...){
        return(main@title)
    }
)

setMethod("title<-",
    signature = signature(
        object = "BaseReport",
        value = "character"
    ),
    definition = function(object, value){
        object@title <- value
        return(object)
    }
)

setMethod("reportDirectory",
    signature = signature(
        object = "BaseReport"
    ),
    definition = function(object){
        return(object@reportDirectory)
    }
)

setMethod("reportDirectory<-",
    signature = signature(
        object = "BaseReport",
        value = "character"
    ),
    definition = function(object, value){
        object@reportDirectory <- value
        return(object)
    }
)

setMethod("filename",
    signature = signature(
        object = "CSVFile"
    ),
    definition = function(object){
        filename <- paste(name(object), 'csv', sep='.')
        return(filename)
    }
)

setMethod("filename",
    signature = signature(
        object = "HTMLReport"
    ),
    definition = function(object){
        filename <- paste(name(object), 'html', sep='.')
        return(filename)
    }
)

setMethod("url",
    signature = signature(
        description = "HTMLReport"
    ),
    definition = function(description){
        fn <- filename(description)
        url <- ''
        ## Do I need to do something here with the reportDirectory?
        ## This has some really bad qualities when the baseUrl
        ## conditionally needs the reportDirectory... I'll look into this
        ## later
        if(description@baseUrl != ''){
            url <- paste(url, description@baseUrl, sep="")
            if(reportDirectory(description) != ''){
                url <- paste(url, reportDirectory(description), sep="/")
            }
            url <- paste(url, fn, sep='/')
        } else {
            url <- fn
        }
        return(url)
    }
)

setMethod("path",
    signature = signature(
        object = "BaseReport"
    ),
    definition = function(object){
        path <- file.path(reportDirectory(object), 
            filename(object))
        return(path)
    }
)

setMethod("path",
    signature = signature(
        object = "HTMLReport"
    ),
    definition = function(object){
        path <- file.path(object@basePath, reportDirectory(object), 
            filename(object))
        return(path)
    }
)

setMethod("path",
    signature = signature(
        object = "DataPackage"
    ),
    definition = function(object){
        path <- file.path(reportDirectory(object), name(object))
        return(path)
    }
)

setMethod("page",
    signature = signature(
        x = "HTMLReport"
    ),
    definition = function(x, ...){
        return(x@page)
    }
)

setMethod("page<-",
    signature = signature(
        object = "HTMLReport",
        value = "character"
    ),
    definition = function(object, value){
        object@page <- value
        return(object)
    }
)

setMethod("basePath",
    signature = signature(
        object = "HTMLReport"
    ),
    definition = function(object){
        return(object@basePath)
    }
)

setMethod("baseUrl",
    signature = signature(
        object = "HTMLReport"
    ),
    definition = function(object){
        return(object@baseUrl)
    }
)

setMethod("dependencies",
    signature = signature(
        object = "DataPackage"
    ),
    definition = function(object){
        return(object@dependencies)
    }
)

setMethod("dependencies<-",
    signature = signature(
        object = "DataPackage",
        value = "character"
    ),
    definition = function(object, value){
        object@dependencies <- sort(unique(value))
        return(object)
    }
)

