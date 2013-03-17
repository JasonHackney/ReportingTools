setGeneric("publish",
    function(object, publicationType, ... )
        standardGeneric("publish")
)

setGeneric("finish",
    function(publicationType, ...)
        standardGeneric("finish")
)

setGeneric("name",
    function(object)
        standardGeneric("name")
)

setGeneric("name<-",
    function(object, value)
        standardGeneric("name<-")
)

setGeneric("title")

setGeneric("title<-",
    function(object, value)
        standardGeneric("title<-")
)

setGeneric("reportDirectory",
    function(object)
        standardGeneric("reportDirectory")
)

setGeneric("reportDirectory<-",
    function(object, value)
        standardGeneric("reportDirectory<-")
)

setGeneric("filename",
    function(object)
        standardGeneric("filename")
)

setGeneric("url")

setGeneric("path",
    function(object)
        standardGeneric("path")
)

setGeneric("page")

setGeneric("page<-",
    function(object, value)
        standardGeneric("page<-")
)

setGeneric("baseUrl",
    function(object)
        standardGeneric("baseUrl")
)

setGeneric("basePath",
    function(object)
        standardGeneric("basePath")
)

setGeneric('dependencies',
    function(object)
        standardGeneric("dependencies")
)

setGeneric('dependencies<-',
    function(object, value)
        standardGeneric("dependencies<-")
)

setGeneric("worksheetCount",
    function(object)
        standardGeneric("worksheetCount")
)


setGeneric("worksheetCount<-",
    function(object, value)
        standardGeneric("worksheetCount<-")
)

setGeneric("objectToHTML", function(object, report, .modifyDF, .toDF,
                                    ...) standardGeneric("objectToHTML"))

setGeneric("toReportDF", function(object, ...) standardGeneric("toReportDF"))

setGeneric("modifyReportDF", function(df, htmlRep, object, ...) standardGeneric("modifyReportDF"))

setGeneric("Link", function(obj, target = NA, report=NULL) standardGeneric("Link"))
