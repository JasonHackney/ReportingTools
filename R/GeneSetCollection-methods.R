setMethod("publish",
    signature = signature(
        object = "GeneSetCollection",
        publicationType = "ANY"
    ),
    definition = function(object, publicationType, setStats = NULL, 
        setPValues = NULL, ...){
        df <- .GeneSetCollection.to.data.frame(object, setStats, setPValues)
        publish(df, publicationType, ...)
    }
)


.GeneSetCollection.to.data.frame <- function(object, setStats, setPValues)
{
   setNames <- names(object)
   descriptions <- sapply(object, description)
   ret <- data.frame(Name=setNames, Description=descriptions, 
       stringsAsFactors = FALSE)
   if(!is.null(setStats)){
       ret <- cbind(ret, setStats)
       colnames(ret)[ncol(ret)] <- 'Set Statistic'
   }
   if(!is.null(setPValues)){
       ret <- cbind(ret, setPValues)
       colnames(ret)[ncol(ret)] <- 'Set P-Value'
   }
   return(ret)
}

setMethod("publish",
    signature = signature(
        object = "GeneSetCollection",
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType, ..., name)
          publicationType$addElement(name, value=object, ...)
          )


setMethod("publish",
    signature = signature(
        object = "GeneSetCollection",
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, annotation.db=NULL, 
        setStats=NULL, setPValues=NULL, geneStats=NULL, ...){
        
        df <- .GeneSetCollection.to.html(object,publicationType, annotation.db, 
            setStats=setStats, setPValues=setPValues, geneStats = geneStats)
        publish(df, publicationType, ...)
    }
)


.GeneSetCollection.to.html <- function(object, htmlRep, annotation.db=NULL, 
    setStats=NULL, setPValues=NULL, geneStats = NULL)
{
    pages.dirname <- paste0('GeneSetCollectionPages', name(htmlRep))  
    page.directory <- file.path(basePath(htmlRep), 
        reportDirectory(htmlRep), pages.dirname)
    .safe.dir.create(page.directory)  
    gs.reportDirectory <- paste(reportDirectory(htmlRep), pages.dirname, sep="/")
    makeGeneListPagesGSC(object, reportDir=gs.reportDirectory, 
        annotation.db, geneStats = geneStats, baseUrl=baseUrl(htmlRep), 
        basePath=basePath(htmlRep))
   
    setLink <- paste('<a href="',pages.dirname,"/", names(object), 
        ".html",'">', names(object), '</a>', sep="")
    descriptions <- sapply(object, description)

    ret <- data.frame("GeneSet" = setLink, "Description" = descriptions,
        stringsAsFactors = FALSE)
    colnames(ret) <- c("Gene Set", "Description")

    if(!is.null(setStats)){
        ret <- cbind(ret, setStats)
        colnames(ret)[ncol(ret)] <- "Gene Set Statistic"
    }
    if(!is.null(setPValues)){
        ret <- cbind(ret, setPValues)
        colnames(ret)[ncol(ret)] <- "Gene Set P-value"
    }
    return(ret)
}
