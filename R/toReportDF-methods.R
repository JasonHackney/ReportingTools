setMethod("toReportDF",
    signature = signature(object= "ANY"),
    definition = function(object, report, ...)
        as.data.frame(object, "data.frame")
)



setMethod("toReportDF",
    signature = signature(object = "GOHyperGResult"),
    definition = function(object, report, pvalueCutoff = 0.01, 
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
    definition = function(object, report, selectedIDs, annotation.db,
            pvalueCutoff = 0.01,categorySize=10, name, path, ...)
    {
        df <- .PFAMhyperG.to.htmlDF2(object, report, selectedIDs, annotation.db,
            pvalueCutoff = pvalueCutoff, categorySize = categorySize)
        df
    }
)

setMethod("toReportDF",
    signature = signature(object = "MArrayLM"),
    definition = function(object, rep, eSet = NULL, n = 1000, 
        pvalueCutoff = 0.01, lfc = 0, adjust.method = "BH", coef = NULL, 
        make.plots = FALSE, factor = NULL, ...)
    {
        .marrayLM.to.data.frame(object, eSet = eSet, n = n, 
            pvalueCutoff = pvalueCutoff, lfc = lfc, 
            adjust.method = adjust.method, coef = coef, 
            make.plots = make.plots, ...)
    }
)

setMethod("toReportDF", 
    signature = signature(object = "DGEExact"),
    definition = function(object, rep, ...)
    {
        .DGEExact.to.data.frame(object, ...)
    }
)

setMethod("toReportDF",
    signature = signature(object = "DGELRT"),
    definition = function(object, rep, ...)
        .DGELRT.to.data.frame(object, ...)
)

setMethod("toReportDF",
    signature = signature(object = "GeneSetCollection"),
    definition = function(object, htmlRep, ...)
        .GeneSetCollection.to.html2(object, htmlRep, ...)
)

setMethod("toReportDF",
    signature = signature(object = "GeneSet"),
    definition = function(object, htmlRep, ...)
        .GeneSet.to.data.frame(object, htmlRep, ...)
)

setMethod("toReportDF",
    signature = signature(object="data.frame"),
    definition = function(object, rep, ...) object
)

setMethod("toReportDF",
    signature = signature(object = "DESeqDataSet"),
    definition = function(object, report, 
        resultName = length(resultsNames(object)), 
        annotation.db = NULL, pvalueCutoff = 0.01, lfc = 0, n = 100,
        sort.by = "pvalue", make.plots = FALSE, ...)
    {
        if (!"results" %in% mcols(mcols(object))$type) {
            stop("No results found in DESeqDataSet, please run DESeq first.")
        }
        if(is(resultName, "numeric")){
            if(resultName > length(resultsNames(object))){
                stop(paste("No result at index", resultName, "there are only",
                    length(resultsNames(object)), "coefficients available."))
            }
            resultName <- resultsNames(object)[resultName]
        }
        
        resTab <- results(object, resultName)
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

            fdata <- data.frame(
                EntrezId = unlist(rownames(resTab)),
                Symbol = unlist(mget(rownames(resTab),
                    getAnnMap("SYMBOL", annotation.db), ifnotfound = NA)),
                GeneName = unlist(mget(rownames(resTab),
                    getAnnMap("GENENAME", annotation.db), ifnotfound = NA)),
                stringsAsFactors = FALSE
            )
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
