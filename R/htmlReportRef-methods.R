setMethod(   '[[<-', c(x="HTMLReportRef"),  function(x, i, ...,value)
    {
      x$addElement(name = i, value = value, ...)
      x
    })

setMethod("[[", c(x="HTMLReportRef"),  function(x, i, exact = TRUE) 
    x$.report[[i, exact = exact]])
 
setMethod("publish", 
    signature = signature(object = "ANY", publicationType = "HTMLReportRef"), 
    definition = function(object, publicationType, .modifyDF = NULL, 
        .toDF = NULL, ...) 
        publicationType$addElement(value = object, ..., 
            .modifyDF = .modifyDF, .toDF = .toDF ))

setMethod("finish",
    signature = signature(publicationType = "HTMLReportRef"),
    definition = function(publicationType, ...){
        publicationType$finish(...)
    }
)

setMethod("path", "HTMLReportRef", function(object)
          {
            sapply(object$handlers, path)
          })
