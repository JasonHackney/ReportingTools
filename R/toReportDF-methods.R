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
    signature = signature(object="data.frame"),
    definition = function(object, rep, ...) object
)
