setMethod("publish",
    signature = signature(
        object = "MArrayLM",
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, eSet = NULL, factor = NULL, 
        n = 1000, pvalueCutoff = 0.01, lfc = 0, coef = NULL, 
        adjust.method = 'BH', make.plots = !is.null(eSet), ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .marrayLM.to.html(object, publicationType, eSet, factor,
            n = n, pvalueCutoff = pvalueCutoff, lfc = lfc, coef = coef, 
            adjust.method = adjust.method, make.plots = make.plots, ...)
        publish(df, publicationType, ...)
    }
)

#need to be careful here that n doesn't get partial matched into name
#setMethod("publish", signature = signature(
#                       object = "MArrayLM",
#                       publicationType="HTMLReportRef"),
#          definition = function(object, publicationType,n = 1000, ..., name) publicationType$addElement(value = object,  n = n,...,  name = name)
#          )


.marrayLM.to.data.frame <- function(object, eSet = NULL, n = 1000, 
    pvalueCutoff = 0.01, lfc = 0, adjust.method='BH', coef = NULL,
    make.plots = FALSE, factor = NULL, ...)
{
    dat <- topTable(object, number = n, p.value = pvalueCutoff, lfc = lfc,
        coef = coef, adjust.method = adjust.method)
    if(is.null(coef))
        coef <- 1:ncol(object)
    
    ## If only one coefficient is selected, then topTable is called
    ## otherwise topTableF is called, returning different values
    selection <- rownames(dat)
    if(length(selection) == 0)
        stop("No probes meet selection criteria. Try changing the log-fold change or p-value cutoff.")

    if(length(coef) == 1){
        padj <- apply(object$p.value, 2, p.adjust, method = adjust.method)
        padj <- padj[selection, coef]
    } else {
        padj <- dat$adj.P.Val
    }

    object <- object[selection, coef]        

    
    ## If there's an eSet, try to get the featureData from the appropriate
    ## annotation package. If there's no annotation package, get it from the
    ## featureData of the eSet itself. If neither are available, try to get it
    ## from the 'genes' slot in the MArrayLM object itself
    fdata <- NULL
    if(!missing(eSet) & !is.null(eSet)){
        eSet <- eSet[selection, ]
        
        ann.map.available <- tryCatch(getAnnMap("ENTREZID", annotation(eSet)), 
            error=function(e){ return(FALSE) })

        if(inherits(ann.map.available, "AnnDbBimap")){
            fdata <- annotate.genes(featureNames(eSet), annotation(eSet),
                keytype = "PROBEID", columns = list(ProbeId = "PROBEID",
                    EntrezId = "ENTREZID", Symbol = "SYMBOL", 
                    GeneName = "GENENAME"))
        } else {
            if(ncol(fData(eSet)) > 0){
                fdata <- fData(eSet)
            }
        }
    }
    
    if( is.null(fdata) ){
        if(!is.null(object$genes)){
            fdata <- object$genes
        } else {
            fdata <- data.frame(
                ProbeId = rownames(eSet),
                stringsAsFactors = FALSE
            )
        }
    }
    
    if(make.plots){
        ret <- data.frame(
            fdata,
            Image = rep(NA, nrow(fdata)),
            object$coef,
            padj,
            stringsAsFactors = FALSE
        )
        fc.cols <- (ncol(fdata)+2):(ncol(fdata)+1+ncol(object$coef))
        colnames(ret)[fc.cols] <- paste(colnames(object), 'logFC')
        if(length(coef) == 1){
            pv.cols <- (ncol(fdata)+ncol(object$coef)+2):ncol(ret)
            colnames(ret)[pv.cols] <- paste(colnames(object), 'Adjusted p-Value')
        } else {
            pv.col <- ncol(ret)
            colnames(ret)[pv.col] <- "Adjusted p-Value"
        }
        
    } else {
        ret <- data.frame(
            fdata,
            object$coef,
            padj,
            stringsAsFactors = FALSE
        )
        fc.cols <- (ncol(fdata)+1):(ncol(fdata)+ncol(object$coef))
        colnames(ret)[fc.cols] <- paste(colnames(object), 'logFC')
        if(length(coef) == 1){
            pv.cols <- (ncol(fdata)+ncol(object$coef)+1):ncol(ret)
            colnames(ret)[pv.cols] <- paste(colnames(object), 'Adjusted p-Value')
        } else {
            pv.col <- ncol(ret)
            colnames(ret)[pv.col] <- "Adjusted p-Value"
        }
        
    }
    
    return(ret)
}

.marrayLM.to.html <- function(object, htmlRep, eSet = NULL, factor = NULL, 
    n = 1000, pvalueCutoff = 0.01, lfc = 0, coef = NULL, adjust.method='BH', 
    make.plots = !is.null(eSet), ...)
{
    
    dat <- topTable(object, number = n, p.value = pvalueCutoff, lfc = lfc,
        adjust.method = adjust.method, coef = coef, ...)
    if(is.null(coef))
        coef <- 1:ncol(object)
    
    if(length(coef) == 1){
        selection <- as.numeric(rownames(dat))
        if(length(selection) == 0)
            stop("No probes meet selection criteria. Try changing the log-fold change or p-value cutoff.")
        padj <- apply(object$p.value, 2, p.adjust, method = adjust.method)
        padj <- padj[selection, coef]
        object <- object[selection, coef]
    } else {
        selection <- rownames(dat)
        if(length(selection) == 0)
            stop("No probes meet selection criteria. Try changing the log-fold change or p-value cutoff.")
        padj <- dat$adj.P.Val
        object <- object[selection, coef]        
    }
    
    ## If there's an eSet, try to get the featureData from the appropriate
    ## annotation package. If there's no annotation package, get it from the
    ## featureData of the eSet itself. If neither are available, try to get it
    ## from the 'genes' slot in the MArrayLM object itself
    fdata <- NULL
    if(! is.null(eSet) ){
        eSet <- eSet[selection, ]
    
        ann.map.available <- tryCatch(getAnnMap("ENTREZID", annotation(eSet)), 
            error=function(e){ return(FALSE) })
        
        if(inherits(ann.map.available, "AnnDbBimap")){
            fdata <- data.frame(
                ProbeId = featureNames(eSet),
                EntrezId = unlist(mget(featureNames(eSet), 
                    getAnnMap("ENTREZID", annotation(eSet)))),
                Symbol = unlist(mget(featureNames(eSet), 
                    getAnnMap("SYMBOL", annotation(eSet)))),
                GeneName = unlist(mget(featureNames(eSet), 
                    getAnnMap("GENENAME", annotation(eSet)))),
                stringsAsFactors = FALSE
            )
        } else {
            if(ncol(fData(eSet)) > 0){
                fdata <- fData(eSet)
            }
        }
    }
    if( is.null(fdata) ){
        fdata <- object$genes
    }
    
    if("EntrezId" %in% colnames(fdata)){
        fdata$EntrezId <- hwrite(fdata$EntrezId, 
            link=paste("http://www.ncbi.nlm.nih.gov/gene/",
                fdata$EntrezId, sep=''), table=FALSE)
    }
    
    
    if(!make.plots){
        ret <- data.frame(
            fdata,
            object$coef,
            padj,
            stringsAsFactors = FALSE
        )
        fc.cols <- (ncol(fdata)+1):(ncol(fdata)+ncol(object$coef))
        colnames(ret)[fc.cols] <- paste(colnames(object), 'logFC')

        pv.cols <- (ncol(fdata)+1+length(fc.cols)):ncol(ret)
        colnames(ret)[pv.cols] <- paste(colnames(object), 'Adjusted p-Value')
        
    } else {
        if(is.null(eSet) || is.null(factor)){
            stop("Can't make plots if either eSet or factor are not set.")
        }
        
        ret <- data.frame(
            fdata,
            Image = rep("", nrow(fdata)),
            object$coef,
            padj,
            stringsAsFactors = FALSE
        )
        
        ret$Image <- rep("", nrow(fdata))
        
        figures.dirname <- paste('figures', name(htmlRep), sep='')  
        figure.directory <- file.path(basePath(htmlRep), 
            reportDirectory(htmlRep), figures.dirname)
        .safe.dir.create(figure.directory)
        
        .make.gene.plots(ret, eSet, factor, figure.directory)
        
        mini.image <- file.path(figures.dirname, 
            paste("mini", rownames(object), "png", sep="."))
        pdf.image <- file.path(figures.dirname, 
            paste("boxplot", rownames(object), "pdf", sep="."))
        ret$Image <- hwriteImage(mini.image, link=pdf.image, table=FALSE)
        
        fc.cols <- (ncol(fdata)+2):(ncol(fdata)+1+ncol(object$coef))
        colnames(ret)[fc.cols] <- paste(colnames(object), 'logFC')
        
        if(length(coef) == 1){
            pv.cols <- (ncol(fdata)+2+length(fc.cols)):ncol(ret)
            colnames(ret)[pv.cols] <- paste(colnames(object), 'Adjusted p-Value')
        } else {
            pv.col <- ncol(ret)
            colnames(ret)[pv.col] <- "Adjusted p-Value"
        }
        
    }
    
    
    return(ret)
}

