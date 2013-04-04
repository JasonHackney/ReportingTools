setMethod("show", 
    signature = signature(
        object = "BaseReport"
    ),
    definition = function(object){
        cat("An object of type \"", class(object), "\"\n", sep = "")
        cat("shortName:", name(object) ,"\n")
        cat("title:", title(object), "\n")
        cat("reportDirectory:", reportDirectory(object), "\n")
    }
)

setMethod("show", 
    signature = signature(
        object = "HTMLReport"
    ),
    definition = function(object){
        cat("An object of type \"", class(object), "\"\n", sep = "")
        cat("shortName:", name(object) ,"\n")
        cat("title:", title(object), "\n")
        cat("reportFilePath:", path(object), "\n")
        cat("reportUrl:", url(object), "\n")
        if(!validConnection(object)) 
            cat("Connection closed. Publication not possible.\n")
    }
)

setMethod("show",
    signature = signature(object = "HTMLReportRef"),
    definition = function(object)
    {
        cat("An object of type \"", class(object), "\"\n", sep = "")
        cat("shortName:", object$shortName ,"\n")
        cat("title:", object$title, "\n")
        cat("reportFilePath:", path(object), "\n")
    }
)

