setMethod(   '[[<-', c(x="HTMLReportRef"),  function(x, i, ...,value)
    {
      x$addElement(name = i, value = value, ...)
      x
    })

setMethod("[[", c(x="HTMLReportRef"),  function(x, i, exact = TRUE) 
    x$.report[[i, exact = exact]])
 
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
