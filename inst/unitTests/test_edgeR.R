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
    checkException(ReportingTools:::.edgeR.to.data.frame(edgeR.de, 
            annotation.db='org.Mm.eg.db', pvalueCutoff=min.pval, n=100), 
        "Returning a zero-length data.frame raises an exception")
}

test_2html <- function(){
    htmlRep <- HTMLReport("testhtmlPage3", reportDirectory = 'testHTMLDirectory',
        title = "Test Report 3")

    html.df <- ReportingTools:::.DGEExact.to.html(edgeR.de, htmlRep, 
        d$counts, conditions, annotation.db='org.Mm.eg', pvalueCutoff=1, 
        lfc=0, n=100)
    checkTrue(nrow(html.df) == 100, 
        "100 rows are returned in coercing fit to data.frame")
    checkException(ReportingTools:::.edgeR.to.html(edgeR.de, htmlRep, 
            d$counts, conditions, annotation.db='org.Mm.eg', 
            pvalueCutoff=min.pval, n=100),
        "Returning a zero-length data.frame raises an exception")
}

