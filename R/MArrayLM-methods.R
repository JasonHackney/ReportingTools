setMethod("publish",
    signature = signature(
        object = "MArrayLM",
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, eSet, factor, n = 1000, 
        pvalueCutoff = 0.01, lfc = 0, coef = NULL, adjust.method = 'BH', 
        make.plots = TRUE, ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .marrayLM.to.html(object, publicationType, eSet, factor,
            n = n, pvalueCutoff = pvalueCutoff, lfc = lfc, coef = coef, 
            adjust.method = adjust.method, make.plots = make.plots, ...)
        publish(df, publicationType, ...)
    }
)

#need to be careful here that n doesn't get partial matched into name
setMethod("publish",
    signature = signature(
        object = "MArrayLM",
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType,
      eSet, factor, n = 1000, 
        pvalueCutoff = 0.01, lfc = 0, coef = NULL, adjust.method = 'BH', 
        make.plots = TRUE, ..., name)
          publicationType$addElement(name = name, value=object, eSet = eSet, factor = factor,
            n = n, pvalueCutoff = pvalueCutoff, lfc = lfc, coef = coef, 
            adjust.method = adjust.method, make.plots = make.plots, ...)
)



setMethod("publish",
    signature = signature(
        object = "MArrayLM",
        publicationType = "ANY"
    ),
    definition = function(object, publicationType, eSet, n = 1000, 
        pvalueCutoff = 0.01, lfc = 0, coef = NULL, adjust.method = 'BH', ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .marrayLM.to.data.frame(object, eSet, n = n, 
            pvalueCutoff = pvalueCutoff, lfc = lfc, coef = coef, 
            adjust.method = adjust.method, ...)
        publish(df, publicationType, ...)
    }
)




.marrayLM.to.data.frame <- function(object, eSet, n = 1000, pvalueCutoff = 0.01,
    lfc = 0, adjust.method='BH', coef = NULL, ...)
{
    dat <- topTable(object, number = n, p.value = pvalueCutoff, lfc = lfc,
        coef = coef, adjust.method = adjust.method, ...)
    if(is.null(coef))
        coef <- 1:ncol(object)
    
    selection <- as.numeric(rownames(dat))
    if(length(selection) == 0)
        stop("No probes meet selection criteria. Try changing the log-fold change or p-value cutoff.")
    padj <- apply(object$p.value, 2, p.adjust, method = adjust.method)
    padj <- padj[selection, coef]
    object <- object[selection, coef]
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
        } else {
            fdata <- data.frame(ProbeId = featureNames(eSet), stringsAsFactors = FALSE)
        }
    }
    
    ret <- data.frame(
        fdata,
        object$coef,
        padj,
        stringsAsFactors = FALSE
    )
    
    fc.cols <- (ncol(fdata)+1):(ncol(fdata)+ncol(object$coef))
    colnames(ret)[fc.cols] <- paste(colnames(object), 'logFC')
    
    pv.cols <- (ncol(fdata)+ncol(object$coef)+1):ncol(ret)
    colnames(ret)[pv.cols] <- paste(colnames(object), 'p-Value')
    
    return(ret)
}

.marrayLM.to.html <- function(object, htmlRep, eSet, factor, n = 1000,
    pvalueCutoff = 0.01, lfc = 0, coef = NULL, adjust.method='BH', 
    make.plots = TRUE, ...)
{
    
    dat <- topTable(object, number = n, p.value = pvalueCutoff, lfc = lfc,
        adjust.method = adjust.method, coef = coef, ...)
    if(is.null(coef))
        coef <- 1:ncol(object)
    
    selection <- as.numeric(rownames(dat))
    if(length(selection) == 0)
        stop("No probes meet selection criteria. 
Try changing the log-fold change or p-value cutoff.")
    padj <- apply(object$p.value, 2, p.adjust, method = adjust.method)
    padj <- padj[selection, coef]
    object <- object[selection, coef]
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
        } else {
            fdata <- data.frame(featureNames(eSet), stringsAsFactors = FALSE)
        }
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
        colnames(ret)[pv.cols] <- paste(colnames(object), 'p-Value')
        
    } else {
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
        
        pv.cols <- (ncol(fdata)+2+length(fc.cols)):ncol(ret)
        colnames(ret)[pv.cols] <- paste(colnames(object), 'p-Value')
        
    }
    
    
    return(ret)
}

