setMethod("toReportDF",
    signature = signature(object= "ANY"),
    definition = function(object, htmlReport, ...)
        as(object, "data.frame")
)



setMethod("toReportDF",
    signature = signature(object = "GOHyperGResult"),
    definition = function(object, htmlReport, pvalueCutoff = 0.01, 
        categorySize = 10, ...)
    {
        summary.tab<-summary(object, pvalue=pvalueCutoff, 
            categorySize = categorySize)
        summary.tab$GOID <- summary.tab[,1]
        summary.tab
    }
)

setMethod("toReportDF",
    signature = signature(object = "PFAMHyperGResult"),
    definition = function(object, htmlReport, selectedIDs, annotation.db,
            pvalueCutoff = 0.01,categorySize=10, name, path, ...)
    {
        df <- .PFAMhyperG.to.htmlDF2(object, htmlReport, selectedIDs, annotation.db,
            pvalueCutoff = pvalueCutoff, categorySize = categorySize)
        df
    }
)

setMethod("toReportDF",
    signature = signature(object = "MArrayLM"),
    definition = function(object, htmlReport, eSet = NULL, n = 1000, 
        pvalueCutoff = 0.01, lfc = 0, adjust.method = "BH", coef = NULL, 
        make.plots = FALSE, factor = NULL, .modifyDF = list(), ...)
    {
        .marrayLM.to.data.frame(object, eSet = eSet, n = n, 
            pvalueCutoff = pvalueCutoff, lfc = lfc, 
            adjust.method = adjust.method, coef = coef, 
            make.plots = make.plots, ...)
    }
)

setMethod("toReportDF", 
    signature = signature(object = "DGEExact"),
    definition = function(object, htmlReport, ...)
    {
        .DGEExact.to.data.frame(object, ...)
    }
)

setMethod("toReportDF",
    signature = signature(object = "DGELRT"),
    definition = function(object, htmlReport, ...)
        .DGELRT.to.data.frame(object, ...)
)

setMethod("toReportDF",
    signature = signature(object = "GeneSetCollection"),
    definition = function(object, htmlReport, ...)
        .GeneSetCollection.to.html2(object, htmlReport, ...)
)

setMethod("toReportDF",
    signature = signature(object = "GeneSet"),
    definition = function(object, htmlReport, ...)
        .GeneSet.to.data.frame(object, htmlReport, ...)
)

setMethod("toReportDF",
    signature = signature(object="data.frame"),
    definition = function(object, rep, ...) object
)
    
setMethod("toReportDF",
    signature = signature(object = "DESeqResults"),
    definition = function(object, htmlReport, DataSet = NULL, 
        annotation.db = NULL, pvalueCutoff = 0.01, lfc = 0, n = 500,
        sort.by = "pvalue", make.plots = FALSE, ..., name = NULL)
    {
        resTab <- as.data.frame(object[
            which(object$pad < pvalueCutoff & 
            abs(object$log2FoldChange) > abs(lfc)), ])
        
        if(nrow(resTab) < 1){
            stop("No features meet selection criteria.\nTry changing the log-fold change or p-value cutoff.")
        }
        
        if(!is.null(sort.by) & sort.by %in% colnames(resTab))
            resTab <- resTab[order(resTab[, sort.by]), ]
        
        if(n < nrow(resTab))
            resTab <- resTab[1:n, ]
        
        ann.map.available <- tryCatch(getAnnMap("SYMBOL", annotation.db), 
            error=function(e){ return(FALSE) })

        if (inherits(ann.map.available, "AnnDbBimap")){
            ## Check valid Entrez ids are passed in
            check.eg.ids(rownames(resTab), annotation.db)

            fdata <- ReportingTools:::annotate.genes(rownames(resTab), annotation.db,
                keytype = "ENTREZID", columns = list(EntrezId = "ENTREZID", 
                    Symbol = "SYMBOL", GeneName = "GENENAME"))
        } else {
            fdata <- data.frame(ID = rownames(resTab), stringsAsFactors = FALSE)
        }

        if(make.plots){
            ret <- data.frame(
                fdata,
                Image = rep("", nrow(fdata)),
                logFC = resTab$log2FoldChange,
                "p-Value" = resTab$pvalue,
                "Adjusted p-Value" = resTab$padj,
                stringsAsFactors = FALSE,
                check.names = FALSE,
                row.names = rownames(resTab)
            )
        } else {
            ret <- data.frame(
                fdata,
                logFC = resTab$log2FoldChange,
                "p-Value" = resTab$pvalue,
                "Adjusted p-Value" = resTab$padj,
                stringsAsFactors = FALSE,
                check.names = FALSE,
                row.names = rownames(resTab)
            )
        }
        ret
    }
)

setMethod("toReportDF",
    signature = signature(object = "DESeqDataSet"),
    definition = function(object, htmlReport, 
        contrast = NULL, resultName = NULL, annotation.db = NULL,
        pvalueCutoff = 0.01, lfc = 0, n = 500, sort.by = "pvalue", 
        make.plots = FALSE, ...)
    {
        if (!"results" %in% mcols(mcols(object))$type) {
            stop("No results found in DESeqDataSet, please run DESeq first.")
        }
        if(is.null(contrast) && !is.null(resultName)){
            resTab <- results(object, name = resultName)
        } else if(!is.null(contrast) && is.null(resultName)) {
            resTab <- results(object, contrast = contrast)
        } else {
            resTab <- results(object)
        }
        resTab <- as.data.frame(resTab[which(resTab$padj < pvalueCutoff & 
            abs(resTab$log2FoldChange) > abs(lfc)), ])
            
        if(!is.null(sort.by) & sort.by %in% colnames(resTab))
            resTab <- resTab[order(resTab[, sort.by]), ]
        
        if(n < nrow(resTab))
            resTab <- resTab[1:n, ]
        
        ann.map.available <- tryCatch(getAnnMap("SYMBOL", annotation.db), 
            error=function(e){ return(FALSE) })

        if (inherits(ann.map.available, "AnnDbBimap")){
            ## Check valid Entrez ids are passed in
            check.eg.ids(rownames(resTab), annotation.db)

            fdata <- annotate.genes(rownames(resTab), annotation.db,
                keytype = "ENTREZID", columns = list(EntrezId = "ENTREZID", 
                    Symbol = "SYMBOL", GeneName = "GENENAME"))
        } else {
            fdata <- data.frame(ID = rownames(resTab), stringsAsFactors = FALSE)
        }

        if(make.plots){
            ret <- data.frame(
                fdata,
                Image = rep("", nrow(fdata)),
                logFC = resTab$log2FoldChange,
                "p-Value" = resTab$pvalue,
                "Adjusted p-Value" = resTab$padj,
                stringsAsFactors = FALSE,
                check.names = FALSE,
                row.names = rownames(resTab)
            )
        } else {
            ret <- data.frame(
                fdata,
                logFC = resTab$log2FoldChange,
                "p-Value" = resTab$pvalue,
                "Adjusted p-Value" = resTab$padj,
                stringsAsFactors = FALSE,
                check.names = FALSE,
                row.names = rownames(resTab)
            )
        }
        ret
    }
)
