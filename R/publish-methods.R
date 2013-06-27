setMethod("publish",
    signature = signature(
        object = "DESeqDataSet",
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType, factor = NULL, 
        n = 1000, pvalueCutoff = 0.01, lfc = 0, coef = NULL, make.plots = TRUE, 
        ..., name)
    {
        publicationType$addElement(name = name, value=object, factor = factor,
            n = n, pvalueCutoff = pvalueCutoff, lfc = lfc, coef = coef, 
            make.plots = make.plots, ...)
    }
)

