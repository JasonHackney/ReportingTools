setMethod("publish",
    signature = signature(
        object = "DGEExact",
        publicationType = "HTMLReport"
    ),
    definition = function(object, publicationType, countTable, conditions,
        annotation.db = 'org.Hs.eg', n = 1000,
        pvalueCutoff = 0.01, lfc = 0, adjust.method = 'BH', 
        sort.method = 'p.value', ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .edgeR.to.html(object, publicationType, as.matrix(countTable), 
            conditions, annotation.db = annotation.db, n = n, pvalueCutoff = pvalueCutoff, lfc = lfc, 
            adjust.method = adjust.method, sort.method = sort.method, ...)
        publish(df, publicationType, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "DGEExact",
        publicationType = "ANY"
    ),
    definition = function(object, publicationType, annotation.db = 'org.Hs.eg', 
        n = 1000, pvalueCutoff = 0.01, lfc = 0, adjust.method = 'BH', 
        sort.method = 'p.value', ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .edgeR.to.data.frame(object, annotation.db = annotation.db,
            n = n, pvalueCutoff = pvalueCutoff, lfc = lfc, 
            adjust.method = adjust.method, sort.method = sort.method, ...)
        publish(df, publicationType, ...)
    }
)

.edgeR.to.data.frame <- function(object, annotation.db = 'org.Hs.eg', 
    pvalueCutoff = 0.01, n = 1000, lfc = 0, adjust.method='BH', 
    sort.method = 'p.value', ...)
{
    dat <- topTags(object, n = n, adjust.method = adjust.method, sort.by = sort.method)

    ## Check valid Entrez ids are passed in
    check.eg.ids(rownames(dat), annotation.db)
    
    selection <- as.numeric(rownames(dat$table))
    
    ##The following gives you all pvalues
    padj <- p.adjust(object$table$PValue, method = adjust.method)
    padj <- padj[match(selection, rownames(object$table))]
    dat <- data.frame(dat$table, padj)

    dat.adj <- dat[dat$padj < pvalueCutoff,]
    dat.lfc <- dat.adj[abs(dat.adj$logFC) > abs(lfc), ]

    if(length(rownames(dat.lfc)) == 0)
        stop("No genes meet the selection criteria. Try changing the log-fold change or p-value cutoff.")

    ann.map.available <- tryCatch(getAnnMap("SYMBOL", annotation.db), 
        error=function(e){ return(FALSE) })

    if (inherits(ann.map.available, "AnnDbBimap")){
      fdata <- data.frame(
        EntrezId = unlist(rownames(dat.lfc)),
        Symbol = unlist(mget(rownames(dat.lfc), 
            getAnnMap("SYMBOL", annotation.db), ifnotfound = NA)),
        GeneName = unlist(mget(rownames(dat.lfc), 
            getAnnMap("GENENAME", annotation.db), ifnotfound = NA)),
        stringsAsFactors = FALSE
      )
    } else {
      IDs <- rownames(dat.lfc)
      fdata <- data.frame(IDs, stringsAsFactors = FALSE)
    }
            
    ret <- data.frame(
        fdata,
        Image = rep("", nrow(fdata)),
        dat.lfc$logFC,
        dat.lfc$padj,
        stringsAsFactors = FALSE
    )

    colnames(ret)[which(colnames(ret) == 'dat.lfc.logFC')] <- 'logFC'
    colnames(ret)[which(colnames(ret) == 'dat.lfc.padj')] <- 'p-Value'
    
    return(ret)
}


.edgeR.to.html <- function(object, htmlRep, countTable, conditions, 
    annotation.db = 'org.Hs.eg', pvalueCutoff = 0.01, n = 1000, lfc = 0, 
    adjust.method = 'BH', sort.method = 'p.value', ...)
{
    dat <- topTags(object, n = n, adjust.method = adjust.method, sort.by = sort.method)

    ## Check valid Entrez ids are passed in
    check.eg.ids(rownames(dat), annotation.db)
    
    selection <- as.numeric(rownames(dat$table))
    ##The following gives you all pvalues
    padj <- p.adjust(object$table$PValue, method = adjust.method)
    padj <- padj[match(selection, rownames(object$table))]
    dat <- data.frame(dat$table, padj)

    dat.adj <- dat[dat$padj < pvalueCutoff,]
    dat.lfc <- dat.adj[abs(dat.adj$logFC) > abs(lfc), ]

    if(length(rownames(dat.lfc)) == 0)
        stop("No genes meet the selection criteria. Try changing the log-fold change or p-value cutoff.")
  
    ann.map.available <- tryCatch(getAnnMap("SYMBOL", annotation.db), 
        error=function(e){ return(FALSE)})
    
    if (inherits(ann.map.available, "AnnDbBimap")){
      fdata <- data.frame(
        EntrezId = unlist(rownames(dat.lfc)),
        Symbol = unlist(mget(rownames(dat.lfc), 
            getAnnMap("SYMBOL", annotation.db), ifnotfound = NA)),
        GeneName = unlist(mget(rownames(dat.lfc), 
            getAnnMap("GENENAME", annotation.db), ifnotfound = NA)),
        stringsAsFactors = FALSE
      )

    fdata$EntrezId <- hwrite(fdata$EntrezId, 
        link=paste("http://www.ncbi.nlm.nih.gov/gene/", fdata$EntrezId, sep=''), table=FALSE)
      
    } else {
      IDs <- rownames(dat.lfc)
      fdata <- data.frame(IDs, stringsAsFactors = FALSE)
    }    
    
    
    ret <- data.frame(
        fdata,
        Image = rep("", nrow(fdata)),
        dat.lfc$logFC,
        dat.lfc$padj,
        stringsAsFactors = FALSE
    )

    figures.dirname <- paste('figures', name(htmlRep), sep='')  
    figure.directory <- file.path(basePath(htmlRep), 
        reportDirectory(htmlRep), figures.dirname)
    .safe.dir.create(figure.directory)

    ## Add pseudocount and log2 transform counts
    countTable <- log2(countTable + 1)
        
    for(gene in rownames(dat.lfc)){
        miniplot <- miniplot(conditions ~ countTable[gene, ], groups=conditions)
        ylab <- paste(fdata[gene, 'Symbol'], "Expression Value (log2)")
        bigplot <- stripplot(countTable[gene, ] ~ conditions,
            panel=panel.boxandstrip, groups=conditions, ylab=ylab)
        
        minipng.filename <- paste("mini", gene ,"png", sep='.')
        minipng.file <- file.path(figure.directory, minipng.filename)
        
        png(minipng.file, height=40, width=200)
        print(miniplot)
        dev.off()
        
        pdf.filename <- paste("boxplot", gene, "pdf", sep=".")
        pdf.file <- file.path(figure.directory, pdf.filename)
        
        pdf(pdf.file, height=4.5, width=4.5)
        print(bigplot)
        dev.off()
    }
    
    mini.image <- file.path(figures.dirname, 
        paste("mini", rownames(dat.lfc), "png", sep="."))
    pdf.image <- file.path(figures.dirname, 
        paste("boxplot", rownames(dat.lfc), "pdf", sep="."))
    ret$Image <- hwriteImage(mini.image, link=pdf.image, table=FALSE)


    colnames(ret)[which(colnames(ret) == 'dat.lfc.logFC')] <- 'logFC'
    colnames(ret)[which(colnames(ret) == 'dat.lfc.padj')] <- 'p-Value'
    
    return(ret)
}

