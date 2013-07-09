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

.GeneSet.to.data.frame <- function(object, htmlRep, annotation.db = NULL, 
    geneStats = NULL)
{
    if(!is.null(annotation.db)){
        tryCatch(getAnnMap("SYMBOL", annotation.db), error=function(e)
            {stop(paste0("Unable to find your annotation.db: ",annotation.db))}
        )
    } else if(annotation(geneIdType(object)) != ""){
        annotation.db <- annotation(geneIdType(object))
        tryCatch(getAnnMap("SYMBOL", annotation.db), error=function(e)
            {stop(paste0("Unable to find your annotation.db: ",annotation.db))}
        )
    }
    
    geneIds <- geneIds(object)
    if(!is.null(annotation.db)){
        ret <- getNamesAndSymbols(geneIds, annotation.db)
        if(! "geneids" %in% names(ret) ){
            ret <- data.frame(
                EntrezId = geneIds,
                Symbol = ret$symbol,
                "Gene Name" = ret$name,
                stringsAsFactors = FALSE, check.names = FALSE
            )
        } else {
            ret <- data.frame(
                ID = geneIds,
                EntrezId = ret$entrez,
                Symbol = ret$symbol,
                "Gene Name" = ret$name,
                stringsAsFactors = FALSE, check.names = FALSE
            )
        }
    } else {
        ret <- data.frame(
            ID = geneIds,
            stringsAsFactors = FALSE
        )
    }
    if(!is.null(geneStats)){
        ret$Statistic <- geneStats[geneIds]
    }
    ret
}

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
        annotation.db, geneStats = geneStats, 
        basePath=basePath(htmlRep))
   
    setLink <- paste('<a href="',pages.dirname,"/", gsub(":", "", names(object)), 
        ".html",'">', names(object), '</a>', sep="")
    descriptions <- sapply(object, description)
    ret <- data.frame("GeneSet" = setLink, "Description" = descriptions,
        stringsAsFactors = FALSE)
    colnames(ret) <- c("Gene Set", "Description")
    
    if (descriptions[1]==""){
        ret <- data.frame("GeneSet" = setLink, stringsAsFactors = FALSE)
        colnames(ret) <- c("Gene Set")
    }

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
