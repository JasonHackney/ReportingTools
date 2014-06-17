library(DESeq2)
library(pasilla)

data(pasillaGenes)
countData <- counts(pasillaGenes)
colData <- pData(pasillaGenes)[,c("condition","type")]
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = colData,
                              design = ~ condition + type)
colData(dds)$condition <- factor(colData(dds)$condition,
                                 levels=c("untreated","treated"))
dds <- DESeq(dds)
ddsRes <- results(dds)

test_1dataframe <- function(){
    df <- toReportDF(ddsRes, n = 100, make.plots = FALSE)
    checkTrue(nrow(df) == 100, 
        "100 rows are returned in coercing the DataSet to data.frame")
    checkTrue(ncol(df) == 4,
        "3 columns are returned for the default data.frame")
}

test_1nogenes <- function(){
    minp <- min(ddsRes$padj, na.rm=TRUE) / 2
    df <- try(toReportDF(ddsRes, n = 100, pvalueCutoff = minp, 
        make.plots = FALSE), silent=TRUE)
    checkTrue(class(df) == "try-error")
}

test_2objectToHTML <- function(){
    rv <- objectToHTML(ddsRes, rep=NULL, factor = ddsRes$condition, 
        n = 100, make.plots=FALSE)
    df <- rv[[2]]
    checkTrue(nrow(df) == 100, 
        "100 rows are returned in coercing the DataSet to data.frame")
    checkTrue(ncol(df) == 4,
        "3 columns are returned for the default data.frame")
}

test_4publish <- function(){
    htmlRep <- HTMLReport("testDESeqDataSet1",
        reportDirectory = "testHTMLDirectory", title = "Test DESeqDataSet Report 1")
    publish(ddsRes, htmlRep, factor = dds$condition,
        n = 100, make.plots = TRUE, DataSet = dds)
    finish(htmlRep)    
}

test_5modifyDF <- function(){
    fives <- function(df, ...){df$Fives <- rep(5, nrow(df)); df}
    htmlRep <- HTMLReport("testDESeqDataSet2",
        reportDirectory = "testHTMLDirectory", title = "Test DESeqDataSet Report 2")
    publish(ddsRes, htmlRep, factor = ddsRes$condition, n = 100, 
        make.plots = FALSE, .modifyDF = fives)
    finish(htmlRep)    
}


