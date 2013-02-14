setMethod("toReportDF",
          signature = signature(
            object = "GOHyperGResult"),
          definition = function(object, pvalueCutoff = 0.01, categorySize = 10, ...)
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


