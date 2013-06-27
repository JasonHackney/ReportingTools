library(DESeq2)
library(pasilla)

data(pasillaGenes)
countData <- counts(pasillaGenes)
colData <- pData(pasillaGenes)[,c("condition","type")]
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = colData,
                              design = ~ condition)
colData(dds)$condition <- factor(colData(dds)$condition,
                                 levels=c("untreated","treated"))
dds.res <- DESeq(dds)

test_1dataframe <- function(){
    df <- toReportDF(dds.res, resultName = 2, n = 100, make.plots = FALSE)
    checkTrue(nrow(df) == 100, 
        "100 rows are returned in coercing the DataSet to data.frame")
    checkTrue(ncol(df) == 4,
        "3 columns are returned for the default data.frame")
}

test_2objectToHTML <- function(){
    rv <- objectToHTML(dds.res, rep=NULL, factor = dds.res$condition, coef = 2, 
        n = 100, make.plots=FALSE)
    df <- rv[[2]]
    checkTrue(nrow(df) == 100, 
        "100 rows are returned in coercing the DataSet to data.frame")
    checkTrue(ncol(df) == 4,
        "3 columns are returned for the default data.frame")
}

test_3publish <- function(){
    htmlRep <- HTMLReport("testDESeqDataSet1",
        reportDirectory = "testHTMLDirectory", title = "Test DESeqDataSet Report 1")
    publish(dds.res, htmlRep, factor = dds.res$condition, coef = 2, 
        n = 100, make.plots = TRUE)
    finish(htmlRep)    
}

test_4modifyDF <- function(){
    fives <- function(df, ...){df$Fives <- rep(5, nrow(df)); df}
    htmlRep <- HTMLReport("testDESeqDataSet2",
        reportDirectory = "testHTMLDirectory", title = "Test DESeqDataSet Report 2")
    publish(dds.res, htmlRep, factor = dds.res$condition, coef = 2, n = 100,
        make.plots = FALSE, .modifyDF = fives)
    finish(htmlRep)    
}


