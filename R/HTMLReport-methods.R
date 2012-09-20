setMethod("publish",
    signature = signature(
        object          = "HTMLReport", 
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, ...){
        if(! validConnection(publicationType))
            stop("Cannot write to closed connection.")
        
        title <- title(object)
        obj.url <- url(object)
        ## Really should check that the URL actually makes sense here
        hwrite(title, link=obj.url, page = page(publicationType), br=TRUE)
    }
)

setMethod("publish",
    signature = signature(
        object          = "list",
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, groupTitle=NA, ...){
        if(! validConnection(publicationType))
            stop("Cannot write to closed connection.")
        
        if(!is.na(groupTitle))
            hwrite(groupTitle, page = page(publicationType),
                heading=3)
                
        ## Should check that each of the objects is publishable
        for(obj in object){
            publish(obj, publicationType, ...)
        }
    }
)

setMethod("publish",
    signature = signature(
        object          = "ANY",
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, ...){
        obj.df <- as(object, 'data.frame')
        publish(obj.df, publicationType, ...)
    }
)


