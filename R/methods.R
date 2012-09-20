setMethod("publish",
    signature = signature(
        object = "ANY",
        publicationType = "DataPackage"
    ),
    definition = function(object, publicationType, objectName, ...){
        .save.rda.fn(object, publicationType, objectName, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "character",
        publicationType = "DataPackage"
    ),
    definition = function(object, publicationType, ...){
        .save.rda.by.name(object, publicationType, ...)
    }
)

