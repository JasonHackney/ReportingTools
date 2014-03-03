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
dds.res <- DESeq(dds)

test_1dataframe <- function(){
    df <- toReportDF(dds.res, coef = 2, n = 100, make.plots = FALSE)
    checkTrue(nrow(df) == 100, 
        "100 rows are returned in coercing the DataSet to data.frame")
    checkTrue(ncol(df) == 4,
        "3 columns are returned for the default data.frame")
}

test_2coefs <- function(){
    df <- toReportDF(dds.res, resultName = "conditiontreated", n = 100,
        make.plots = FALSE)
    df2 <- toReportDF(dds.res, resultName = "typesingle.read", n = 100,
        make.plots = FALSE)
    checkTrue(any(df$logFC != df2$logFC),
        "Results are different between selected coefficients")
    df3 <- toReportDF(dds.res, 
        contrast = c("condition", "treated", "untreated"), n = 100, 
        make.plots = FALSE)
}

test_3objectToHTML <- function(){
    rv <- objectToHTML(dds.res, rep=NULL, factor = dds.res$condition, 
        contrast = c("condition", "treated", "untreated"), n = 100,
        make.plots=FALSE)
    df <- rv[[2]]
    checkTrue(nrow(df) == 100, 
        "100 rows are returned in coercing the DataSet to data.frame")
    checkTrue(ncol(df) == 4,
        "3 columns are returned for the default data.frame")
}

test_4publish <- function(){
    htmlRep <- HTMLReport("testDESeqDataSet1",
        reportDirectory = "testHTMLDirectory", 
        title = "Test DESeqDataSet Report 1")
    publish(dds.res, htmlRep, factor = dds.res$condition, 
        contrast = c("condition", "treated", "untreated"), n = 100, 
        make.plots = TRUE)
    finish(htmlRep)    
}

test_5modifyDF <- function(){
    fives <- function(df, ...){df$Fives <- rep(5, nrow(df)); df}
    htmlRep <- HTMLReport("testDESeqDataSet2",
        reportDirectory = "testHTMLDirectory", 
        title = "Test DESeqDataSet Report 2")
    publish(dds.res, htmlRep, factor = dds.res$condition, 
        contrast = c("condition", "treated", "untreated"), n = 100,
        make.plots = FALSE, .modifyDF = fives)
    finish(htmlRep)    
}


