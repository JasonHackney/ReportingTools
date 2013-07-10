library(ALL)
library(limma)
library(hgu95av2.db)

data(ALL)
ALL <- ALL[, !is.na(ALL$sex)]
model <- model.matrix(~mol.biol+sex, ALL)
fit <- eBayes(lmFit(ALL, model))
max.lfc <- max(fit$coef[,2])+1


test_1dataframe <- function(){
    df <- ReportingTools:::.marrayLM.to.data.frame(fit, ALL, coef=2, n=100)
    checkTrue(nrow(df) == 100, 
        "100 rows are returned in coercing fit to data.frame")
    checkException(ReportingTools:::.marrayLM.to.data.frame(fit, ALL, coef=2, 
        lfc=max.lfc), "Returning a zero-length data.frame raises an exception")
}

test_3fdata <- function(){
    htmlRep <- HTMLReport("testMArrayLMhtmlPage",
        reportDirectory = 'testHTMLDirectory', title = "Test MArrayLM Report")

    ALL2 <- ALL
    annotation(ALL2) <- ""
    df2 <- ReportingTools:::.marrayLM.to.data.frame(fit, ALL2, coef=2, n=100)
    checkTrue(all.equal(colnames(df2), 
        c("ProbeId", "mol.biolBCR/ABL logFC", "mol.biolBCR/ABL Adjusted p-Value")))
    checkTrue(all(rownames(df2) %in% rownames(ALL2)), 
        "The rownames of the data.frame are found in the eSet")

    fd <- data.frame(
        ProbeId = featureNames(ALL),
        EntrezId = unlist(mget(featureNames(ALL), hgu95av2ENTREZID, 
            ifnotfound=NA)),
        stringsAsFactors = FALSE
    )
    
    ALL3 <- ALL
    annotation(ALL3) <- ""
    fData(ALL3) <- fd
    df3 <- ReportingTools:::.marrayLM.to.data.frame(fit, ALL3, coef=2, n=100)
    checkTrue(all.equal(colnames(df3), 
        c("ProbeId", "EntrezId", "mol.biolBCR/ABL logFC", "mol.biolBCR/ABL Adjusted p-Value")))
    
    fit2 <- fit
    fit2$genes <- data.frame(MProbeId = rownames(fit), row.names = rownames(fit))
    df4 <- ReportingTools:::.marrayLM.to.data.frame(fit2, ALL2, coef=2, n=100)
    checkTrue(all.equal(colnames(df4), 
        c("MProbeId", "mol.biolBCR/ABL logFC", "mol.biolBCR/ABL Adjusted p-Value")))
    checkTrue(all(rownames(df4) %in% rownames(ALL2)), 
        "The rownames of the data.frame are found in the eSet")
    
}

test_4publishNoFigures <- function(){
    htmlRep <- HTMLReport("testMArrayLMhtmlPage2", 
        reportDirectory = "testHTMLDirectory", title = "Test MArrayLM Report 2")
    df <- publish(fit, htmlRep, eSet = ALL, coef = 2, n = 100, make.plots = FALSE)
    checkTrue(all.equal(dim(df), c(100,6)), "The dimensions of the data.frame are correct")
    finish(htmlRep)
}

test_5publish <- function(){
    htmlRep <- HTMLReport("testMArrayLMhtmlPage3", 
        reportDirectory = "testHTMLDirectory", title = "Test MArrayLM Report 3")
    publish(fit, htmlRep, eSet = ALL, factor = ALL$mol.biol, coef = 2, n = 100)
    finish(htmlRep)
}

test_6dataframe_2coefs <- function(){
    df <- ReportingTools:::.marrayLM.to.data.frame(fit, ALL, coef = 2:3, 
        n = 100)
    checkTrue(all.equal(colnames(df), c("ProbeId", "EntrezId", "Symbol", "GeneName",
        "mol.biolBCR/ABL logFC", "mol.biolE2A/PBX1 logFC", "Adjusted p-Value")))
}

test_7toReportDF <- function(){
    htmlRep <- HTMLReport("testMArrayLMhtmlPage4",
        reportDirectory = "testHTMLDirectory", title = "Test MArrayLM Report 4")

    df <- toReportDF(fit, htmlRep, eSet = ALL, factor = ALL$mol.biol, coef = 2,
        n = 100, make.plots = FALSE)
    checkTrue(nrow(df) == 100)
    checkTrue(ncol(df) == 6)
}

test_8HTMLReport <- function(){
    htmlRep <- HTMLReport("testMArrayLMhtmlPage4",
        reportDirectory = "testHTMLDirectory", title = "Test MArrayLM Report 4")
    publish(fit, htmlRep, eSet = ALL, factor = ALL$mol.biol, coef = 2, n = 100,
        make.plots = TRUE)
    finish(htmlRep)    
}

test_9ModifyReport <- function(){
    fives <- function(df, ...){df$Fives <- rep(5, nrow(df)); df}
    htmlRep <- HTMLReport("testMArrayLMhtmlPage5",
        reportDirectory = "testHTMLDirectory", title = "Test MArrayLM Report 4")
    publish(fit, htmlRep, eSet = ALL, factor = ALL$mol.biol, coef = 2, n = 100,
        make.plots = FALSE, .modifyDF = fives)
    finish(htmlRep)    
}