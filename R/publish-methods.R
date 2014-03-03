setMethod("publish", 
    signature = signature(
        object = "ANY", 
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType, .modifyDF = NULL, 
        .toDF = NULL, ...)
    {
        publicationType$addElement(value = object, ..., 
            .modifyDF = .modifyDF, .toDF = .toDF)
    }
)

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

setMethod("publish",
    signature = signature(
        object = "DESeqResults",
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType, DataSet = NULL, 
        factor = NULL,  n = 1000, pvalueCutoff = 0.01, lfc = 0, 
        make.plots = TRUE, ..., name)
    {
        publicationType$addElement(name = name, value=object, factor = factor,
            DataSet = DataSet, n = n, pvalueCutoff = pvalueCutoff, lfc = lfc, 
            make.plots = make.plots, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "DGEExact",
        publicationType = "ANY"
    ),
    definition = function(object, publicationType, annotation.db = 'org.Hs.eg', 
        n = 1000, pvalueCutoff = 0.01, lfc = 0, adjust.method = 'BH', 
        sort.method = 'p.value', ...)
    {
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .DGEExact.to.data.frame(object, annotation.db = annotation.db,
            n = n, pvalueCutoff = pvalueCutoff, lfc = lfc, 
            adjust.method = adjust.method, sort.method = sort.method, ...)
        publish(df, publicationType, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "DGEExact",
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType, ..., name)
    {
        publicationType$addElement(name = name,value= object, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "DGELRT",
        publicationType = "ANY"
    ),
    def = function(object, publicationType, annotation.db = 'org.Hs.eg', 
        n = 1000, pvalueCutoff = 0.01, lfc = 0, adjust.method = 'BH', 
        sort.method = 'p.value', ...)
    {
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .DGELRT.to.data.frame(object, annotation.db = annotation.db,
            n = n, pvalueCutoff = pvalueCutoff, lfc = lfc,
            adjust.method = adjust.method, sort.method = sort.method, ...)
        publish(df, publicationType, ...)
    }
)

setMethod("publish", 
    signature = signature(
        object = "DGELRT", 
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType, ..., name){
        publicationType$addElement(name = name, object,  ...)
    }
)

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


setMethod("publish",
    signature = signature(
        object = "GeneSetCollection",
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType,annotation.db=NULL, 
        setStats=NULL, setPValues=NULL, geneStats=NULL, ..., name)
    {
        publicationType$addElement(name, value=object, 
            annotation.db=annotation.db, setStats=setStats, 
            setPValues=setPValues, geneStats=geneStats, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "GOHyperGResult",
        publicationType = "ANY"
    ),
    definition = function(object, publicationType, 
        pvalueCutoff = 0.01, categorySize = 10, ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .GOhyperG.to.data.frame(object, pvalueCutoff = pvalueCutoff,
            categorySize = categorySize)
        publish(df, publicationType, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "GOHyperGResult",
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType,..., name)
    {
        publicationType$addElement(name, value = object,...)
    }
)



setMethod("publish",
    signature = signature(
        object = "PFAMHyperGResult",
        publicationType = "ANY"
    ),
    definition = function(object, publicationType, 
        pvalueCutoff = 0.01,categorySize=10, ...){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .PFAMhyperG.to.data.frame(object, pvalueCutoff = pvalueCutoff, 
            categorySize = categorySize)
        publish(df, publicationType, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "PFAMHyperGResult",
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType, name,  ...)
    {
        publicationType$addElement(name, value = object, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "MArrayLM",
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType, eSet, factor, n = 1000, 
        pvalueCutoff = 0.01, lfc = 0, coef = NULL, adjust.method = 'BH', 
        make.plots = TRUE,..., name)
    {
        publicationType$addElement(name = name, value=object, eSet = eSet, 
            factor = factor, n = n, pvalueCutoff = pvalueCutoff, lfc = lfc, 
            coef = coef, adjust.method = adjust.method, 
            make.plots = make.plots, ...)
    }
)

setMethod("publish",
    signature = signature(
        object = "MArrayLM",
        publicationType = "ANY"
    ),
    definition = function(object, publicationType, eSet, n = 1000, 
        pvalueCutoff = 0.01, lfc = 0, coef = NULL, adjust.method = 'BH', ...)
    {
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        df <- .marrayLM.to.data.frame(object, eSet, n = n, 
            pvalueCutoff = pvalueCutoff, lfc = lfc, coef = coef, 
            adjust.method = adjust.method, ...)
        publish(df, publicationType, ...)
    }
)

setMethod("publish",
    signature = signature(
        object          = "ANY",
        publicationType = "list"
    ),
    definition = function(object, publicationType, ...)
    {
        for(pubType in publicationType){
            publish(object, pubType, ...)
        }
    }
)

setMethod("publish",
    signature = signature(
        object = "data.frame", 
        publicationType = "HTMLReportRef"
    ),
    definition = function(object, publicationType, ..., name){
        publicationType$addElement(name, object, ...)
    }
)
