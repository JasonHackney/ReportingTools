
setMethod("Link", signature = signature(obj = "list"),
          function(obj, target, report)
          {
            links = unlist(mapply(function(object, targ)
              {
                c(Link(object, targ, report), newXMLNode("br"))
                }, object = obj, targ = target))
            linkdiv = newXMLNode("div", attrs = list(class="LinkSet"),
              kids = links)
            linkdiv
          }
          )

setMethod("Link", signature = signature(obj = "character"),
          function(obj, target, report)
          {
            #if we have more than one, back to the list case!
            if(length(obj) > 1)
              return(Link(as.list(obj), target, report))

            if(!is.null(names(obj)) & is.na(target))
              target = names(obj)

            if(is.na(target))
              stop("No target specified for link")
            if(!is.null(report))
              target = file.path(getRelativePath(dirname(target), dirname(path(report)[1])), basename(target))
            newXMLNode("a", obj, attrs = list(href=target))
          })

setMethod("Link", signature = signature(obj = "HTMLReportRef"),
          function(obj, target, report)
          {
            if(is.na(target))
              target = path(obj)[1]
              
            Link(obj$title, target, report)

          })
         
