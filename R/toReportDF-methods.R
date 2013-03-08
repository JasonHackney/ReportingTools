setMethod("toReportDF",
          signature = signature(
            object= "ANY"),
          definition = function(object, report, ...)
          as.data.frame(object, "data.frame")
          )



setMethod("toReportDF",
          signature = signature(
            object = "GOHyperGResult"),
          definition = function(object, report, pvalueCutoff = 0.01, categorySize = 10, ...)
          {
            summary.tab<-summary(object, pvalue=pvalueCutoff, categorySize = categorySize)
            summary.tab$GOID<-summary.tab[,1]
            summary.tab
            #df<-data.frame(summary.tab$GOID,summary.tab$Term,signif(summary.tab$Pvalue, 3), signif(summary.tab$OddsRatio, 3), summary.tab$Count,summary.tab$Size)
            #colnames(df)<-c("Gene Ontology", "GO Term", "P-value", "Odds Ratio", "Count", "Ontology Size")
            #return(df)                              
#            .GOhyperG.to.data.frame(object, pvalueCutoff = pvalueCutoff, categorySize = categorySize)
          }
       )

setMethod("toReportDF",
   signature = signature(
        object = "PFAMHyperGResult"
      ),
    definition = function(object, report, selectedIDs,annotation.db,
      pvalueCutoff = 0.01,categorySize=10, name, path, ...){
      df <- .PFAMhyperG.to.htmlDF2(object, report,selectedIDs,annotation.db,pvalueCutoff = pvalueCutoff,categorySize )
    })

setMethod("toReportDF", signature = signature(
                            object = "DGEExact"),
          definition = function(object, rep, ...)
          {
            .DGEExact.to.html2(object, rep, ...)
          }
          )


setMethod("toReportDF",
    signature = signature(
        object = "MArrayLM"
      ),
    definition = function(object, publicationType, eSet, factor, n = 1000, 
        pvalueCutoff = 0.01, lfc = 0, adjust.method = 'BH', coef = NULL, 
        make.plots = TRUE, ..., .addColumns, .toDF){
        ## First, make a data.frame for publication,
        ## then call publish on that data.frame
        .marrayLM.to.data.frame(object, eSet, n = n, 
            pvalueCutoff = pvalueCutoff, lfc = lfc, adjust.method = adjust.method, 
            coef = coef, make.plots = make.plots, ...)
      }
          )


setMethod("toReportDF",
    signature = signature(
        object = "GeneSetCollection"
    ),
          definition = function(object, htmlRep, ...)
          .GeneSetCollection.to.html2(object, htmlRep, ...)
          )


setMethod("toReportDF",
          signature = signature(object = "DGELRT"),
          definition = function(object, rep, ...)
           .DGELRT.to.html2(object, rep, ...)
          )

setMethod("toReportDF",
          signature = signature(object="data.frame"),
          definition = function(object, rep, ...) object
          )
