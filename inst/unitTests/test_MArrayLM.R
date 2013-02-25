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

test_2html <- function(){
    htmlRep <- HTMLReport("testMArrayLMhtmlPage", 
        reportDirectory = 'testHTMLDirectory', title = "Test MArrayLM Report")

    html.df <- ReportingTools:::.marrayLM.to.html(fit, htmlRep, ALL, 
        factor=ALL$mol.biol, coef=2, n=100)
    checkTrue(nrow(html.df) == 100, 
        "100 rows are returned in coercing fit to data.frame")
    checkException(ReportingTools:::.marrayLM.to.html(fit, htmlRep, ALL, 
            factor=ALL$mol.biol, coef=2, lfc=max.lfc),
        "Returning a zero-length data.frame raises an exception")    
}

test_3fdata <- function(){
    htmlRep <- HTMLReport("testMArrayLMhtmlPage",
        reportDirectory = 'testHTMLDirectory', title = "Test MArrayLM Report")

    fd <- data.frame(
        ProbeId = featureNames(ALL),
        EntrezId = unlist(mget(featureNames(ALL), hgu95av2ENTREZID, 
            ifnotfound=NA)),
        stringsAsFactors = FALSE
    )
    
    ALL2 <- ALL
    annotation(ALL2) <- ""
    df <- ReportingTools:::.marrayLM.to.data.frame(fit, ALL2, coef=2, n=100)
    checkTrue(all.equal(colnames(df), 
        c("ID", "mol.biolBCR/ABL logFC", "mol.biolBCR/ABL Adjusted p-Value")))
    
    ALL3 <- ALL
    annotation(ALL3) <- ""
    fData(ALL3) <- fd
    df <- ReportingTools:::.marrayLM.to.data.frame(fit, ALL3, coef=2, n=100)
    checkTrue(all.equal(colnames(df), 
        c("ProbeId", "EntrezId", "mol.biolBCR/ABL logFC", "mol.biolBCR/ABL Adjusted p-Value")))
    
}

test_4publishNoFigures <- function(){
    htmlRep <- HTMLReport("testMArrayLMhtmlPage2", 
        reportDirectory = "testHTMLDirectory", title = "Test MArrayLM Report 2")
    publish(fit, htmlRep, coef = 2, n = 100)
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