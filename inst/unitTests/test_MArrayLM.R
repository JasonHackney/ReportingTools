library(ALL)
library(limma)
library(hgu95av2.db)

data(ALL)
model <- model.matrix(~mol.biol, ALL)
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
    htmlRep <- HTMLReport("testhtmlPage2", reportDirectory = 'testHTMLDirectory',
        title = "Test Report 2")

    html.df <- ReportingTools:::.marrayLM.to.html(fit, htmlRep, ALL, 
        factor=ALL$mol.biol, coef=2, n=100)
    checkTrue(nrow(html.df) == 100, 
        "100 rows are returned in coercing fit to data.frame")
    checkException(ReportingTools:::.marrayLM.to.html(fit, htmlRep, ALL, 
            factor=ALL$mol.biol, coef=2, lfc=max.lfc),
        "Returning a zero-length data.frame raises an exception")    
}

test_3fdata <- function(){
    htmlRep <- HTMLReport("testhtmlPage2", reportDirectory = 'testHTMLDirectory',
        title = "Test Report 2")

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
        c("ProbeId", "mol.biolBCR/ABL logFC", "mol.biolBCR/ABL p-Value")))
    
    ALL3 <- ALL
    annotation(ALL3) <- ""
    fData(ALL3) <- fd
    df <- ReportingTools:::.marrayLM.to.data.frame(fit, ALL3, coef=2, n=100)
    checkTrue(all.equal(colnames(df), 
        c("ProbeId", "EntrezId", "mol.biolBCR/ABL logFC", "mol.biolBCR/ABL p-Value")))
    
}