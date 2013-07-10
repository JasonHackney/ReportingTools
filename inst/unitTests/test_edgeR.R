library(edgeR)
library(org.Mm.eg.db)

data(mockRnaSeqData)
conditions <- c(rep("case",3), rep("control", 3))
fd <- data.frame(
    EntrezId = rownames(mockRnaSeqData), 
    Symbol = unlist(mget(rownames(mockRnaSeqData), org.Mm.egSYMBOL,
        ifnotfound=NA)), 
    GeneName = unlist(mget(rownames(mockRnaSeqData), org.Mm.egGENENAME,
        ifnotfound=NA)),
    stringsAsFactors = FALSE
)
d <- DGEList(counts = mockRnaSeqData, group = conditions, genes = fd)
d <- calcNormFactors(d)
d <- estimateCommonDisp(d)
## Get an edgeR object
edgeR.de <- exactTest(d)
min.pval <- min(edgeR.de$table$PValue)/10

test_1dataframe <- function(){
    df <- ReportingTools:::.DGEExact.to.data.frame(edgeR.de, 
        annotation.db='org.Mm.eg', pvalueCutoff=1, lfc=0, n=100)
    checkTrue(nrow(df) == 100, 
        "100 rows are returned in coercing fit to data.frame")
    
    df2 <- ReportingTools:::.DGEExact.to.data.frame(edgeR.de,
        annotation.db = NULL, pvalueCutoff = 1, lfc = 0, n = 100)
    checkTrue(nrow(df2) == 100, 
        "100 rows are returned in coercing fit to data.frame")
    checkTrue(ncol(df2) == 5, 
        "5 columns are returned in coercing fit to data.frame")
    checkTrue(all.equal(df, df2), "The same data.frame is returned for the calls with and without the annotation db")
    checkException(ReportingTools:::.DGEExact.to.data.frame(edgeR.de, 
            annotation.db='org.Mm.eg.db', pvalueCutoff=min.pval, n=100), 
        "Returning a zero-length data.frame raises an exception")
    
    edgeR.de2 <- edgeR.de
    edgeR.de2$genes <- NULL
    df3 <- ReportingTools:::.DGEExact.to.data.frame(edgeR.de2,
        annotation.db = NULL, pvalueCutoff = 1, lfc = 0, n = 100)
    checkTrue(all(rownames(df3) %in% rownames(d)), 
            "rownames for the data.frame match the expression data when there is no annotation data.")
    
    edgeR.de3 <- edgeR.de
    edgeR.de3$genes <- edgeR.de3$genes[, 1, drop = FALSE]
    df4 <- ReportingTools:::.DGEExact.to.data.frame(edgeR.de3,
        annotation.db = NULL, pvalueCutoff = 1, lfc = 0, n = 100)
    checkTrue(all(rownames(df4) %in% rownames(d)), 
            "rownames for the data.frame match the expression data when there's one column of annotation data")
    
    edgeR.de3$genes <- fd
    rownames(edgeR.de3$genes) <- NULL
    df5 <- ReportingTools:::.DGEExact.to.data.frame(edgeR.de3,
        annotation.db = NULL, pvalueCutoff = 1, lfc = 0, n = 100)
    checkTrue(all(rownames(df5) %in% rownames(d)), 
            "rownames for the data.frame match the expression data when there's no rownames on the annotation data")
    
}


test_3toReportDF <- function(){
    htmlRep <- HTMLReport("testhtmlPage4", 
        reportDirectory = 'testHTMLDirectory', title = "Test Report 4")
    
    rep.df <- toReportDF(edgeR.de, htmlRep, countTable = d$counts, 
        conditions = conditions, annotation.db='org.Mm.eg', 
        pvalueCutoff=1, lfc=0, n=100, make.plots = TRUE)
    
    checkTrue(nrow(rep.df) == 100, 
        "100 rows are returned in coercing fit to data.frame")
    
    checkTrue(ncol(rep.df) == 6, 
        "6 columns are returned in coercing fit to data.frame")
 
}


test_3modifyReportDF <- function(){
    htmlRep <- HTMLReport("testhtmlPage4", 
        reportDirectory = 'testHTMLDirectory', title = "Test Report 4")
    
    rep.df <- toReportDF(edgeR.de, htmlRep, countTable = d$counts, 
        conditions = conditions, annotation.db='org.Mm.eg', 
        pvalueCutoff=1, lfc=0, n=100, make.plots = TRUE)
    
    checkTrue(nrow(rep.df) == 100)

    rep.df <- modifyReportDF(rep.df, htmlRep, object = edgeR.de,
        countTable = NULL, conditions = NULL, make.plots = FALSE)
    
    rep.df <- modifyReportDF(rep.df, htmlRep, object = edgeR.de,
        countTable = mockRnaSeqData, conditions = conditions, make.plots = TRUE)
 
}


test_4publishHTMLReportRef <- function(){
    htmlRep <- HTMLReport("testhtmlPage4", 
        reportDirectory = 'testHTMLDirectory', title = "Test Report 4")
    publish(edgeR.de, htmlRep, countTable = d$counts, conditions = conditions,
        annotation.db = NULL, pvalueCutoff = 1, lfc = 0, n = 100, 
        make.plots = FALSE, name = "DEtable")
    finish(htmlRep)
}

test_5modifyhHTMLReportRef <- function(){
    fives <- function(df, ...){df$Fives <- rep(5, nrow(df)); df}
    htmlRep <- HTMLReport("testhtmlPage5", 
        reportDirectory = 'testHTMLDirectory', title = "Test Report 5")
    publish(edgeR.de, htmlRep, countTable = d$counts, conditions = conditions,
        annotation.db = NULL, pvalueCutoff = 1, lfc = 0, n = 100, 
        make.plots = FALSE, name = "DEtable", .modifyDF = fives)
    finish(htmlRep)
}