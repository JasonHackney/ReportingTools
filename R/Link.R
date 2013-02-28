setMethod("Link", signature = signature(obj = "list"),
          function(obj, target)
          {
            links = unlist(mapply(function(object, targ)
              {
                c(Link(object, targ), newXMLNode("br"))
                }, object = obj, targ = target))
            linkdiv = newXMLNode("div", attrs = list(class="LinkSet"),
              kids = links)
            linkdiv
          }
          )

setMethod("Link", signature = signature(obj = "character"),
          function(obj, target)
          {
            #if we have more than one, back to the list case!
            if(length(obj) > 1)
              return(Link(as.list(obj), target))

            if(!is.null(names(obj)) & is.na(target))
              target = names(obj)

            if(is.na(target))
              stop("No target specified for link")

            newXMLNode("a", obj, attrs = list(href=target))
          })

setMethod("Link", signature = signature(obj = "HTMLReportRef"),
          function(obj, target)
          {
            if(is.na(target))
              target = file.path(obj$basePath, obj$reportDirectory, paste0(obj$shortName, ".html" ))
            Link(ojb$title, target)

          })
         
