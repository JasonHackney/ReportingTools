.save.rda.fn <- function(object, dataPackage, objectName = 'data', 
    rd.string = NULL, compress = TRUE, ...)
{
    .safe.dir.create(path(dataPackage))
    data.dir <- file.path(path(dataPackage), 'data')
    
    .safe.dir.create(data.dir)
    
    objectName <- sub('.rda$', '', objectName)
    filename <- sub('$', '.rda', objectName)
    data.fn <- file.path(data.dir, filename)
    
    objectClass <- class(object)
    objectPackage <- attr(objectClass, 'package')
    ## Since lattice doesn't export the class definition for trellis objects
    ## we have to put that dependency in ourselves.
    if(objectClass == 'trellis')
        dependencies(dataPackage) <- c(dependencies(dataPackage), 'lattice')
    if(!is.null(objectPackage))
        dependencies(dataPackage) <- c(dependencies(dataPackage), objectPackage)
    
    ## Change the name of `object' to what is set in objectName
    ## save that object to a file with the same name
    assign(objectName, object)
    save(list=c(objectName), file = data.fn, compress = compress, ...) 
    
    ## Print out some help for the object
    man.dir <- file.path(path(dataPackage), 'man')
    .safe.dir.create(man.dir)
    man.fn <- sub('.rda$', '.Rd', filename)
    man.fn <- file.path(man.dir, man.fn)
    if(!is.null(rd.string)){
        cat(rd.string, file=man.fn)
    } else if (objectClass == 'trellis') {
        file.create(man.fn)
    } else {
        promptData(name=objectName, filename = man.fn)
    }
}

.save.rda.by.name <- function(objectNames, dataPackage, 
    rd.string = NULL, compress = TRUE, envir = parent.frame(), ...)
{
    .safe.dir.create(path(dataPackage))
    data.dir <- file.path(path(dataPackage), 'data')
    
    .safe.dir.create(data.dir)
    
    objectNames <- sub('.rda$', '', objectNames)
    
    ok <- unlist(lapply(objectNames, exists, envir = envir))
    if(sum(ok) != length(ok)){
        if(sum(ok) != 0){
            warning(paste("Not all objects could be saved. These objects were not found:",
                paste(objectNames[!ok], sep=", "), sep=' '))            
        } else {
            stop("No saveable objects were found.")
        }
    }
    objectNames <- objectNames[ok]
    for(objName in objectNames){
        ## Get a copy of the object, and give it a new name in the
        ## current environment
        object <- get(objName, envir = envir)
        assign(objName, object)
        
        filename <- sub('$', '.rda', objName)
        data.fn <- file.path(data.dir, filename)
    
        objectClass <- class(object)
        objectPackage <- attr(objectClass, 'package')
        ## Since lattice doesn't export the class definition for trellis objects
        ## we have to put that dependency in ourselves.
        if(objectClass == 'trellis')
            dependencies(dataPackage) <- c(dependencies(dataPackage), 'lattice')
        if(!is.null(objectPackage))
            dependencies(dataPackage) <- c(dependencies(dataPackage), objectPackage)
    
        ## Change the name of `object' to what is set in objectName
        ## save that object to a file with the same name
        save(list=c(objName), file = data.fn, compress = compress, ...) 
    
        ## Print out some help for the object
        man.dir <- file.path(path(dataPackage), 'man')
        .safe.dir.create(man.dir)
        man.fn <- sub('.rda$', '.Rd', filename)
        man.fn <- file.path(man.dir, man.fn)
        if(!is.null(rd.string)){
            cat(rd.string, file=man.fn)
        } else if (objectClass == 'trellis'){
            # Do nothing here?
            file.create(man.fn)
            # prompt(object, name=objName, filename = man.fn)
        } else {
            promptData(object, name=objName, filename = man.fn)
        }
    }
}

.safe.dir.create <- function(path, recursive=FALSE)
{
    if(!file.exists(path)){
        dirTest <- function(x) isTRUE(file.info(x)$isdir)
        if (!dirTest(path) && !dir.create(path, recursive = recursive)) 
            stop(gettextf("cannot create directory '%s'", path), 
                domain = NA)
    }
}

validConnection <- function(htmlRep)
{
    valid <- tryCatch(isOpen(page(htmlRep)), error=function(e) return(FALSE))
    return(valid)
}
