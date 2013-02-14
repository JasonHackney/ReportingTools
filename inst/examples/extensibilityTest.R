
library(ReportingTools)
library(ALL)
library(hgu95av2.db)
library(genefilter)
library(GOstats)
library(limma)
library(GSEAlm)
library(GSEABase)

#I was tired of waiting a noticable amount of time each time I wanted to test this stuff. Original code in the if(FALSE) at the bottom of this file
load("gores.rda")
myrep = htmlReportRef(reportDirectory = "./",shortName="bigtest")
stuff = publish(goResults, myrep, selectedIDs=selectedIDs, annotation.db="org.Hs.eg")

#trace(ReportingTools:::objectToHTML, browser, signature=c(object="ANY"))

fiveCol = function(df, ...) cbind(df, fives=5)
stuff2=publish(goResults, myrep, selectedIDs=selectedIDs, annotation.db="org.Hs.eg", .addColumns = list(addReportColumns,fiveCol))

if(FALSE)
  {
    data(ALL)

ALL <- ALL[, ALL$mol.biol %in% c('NEG','BCR/ABL') &
    !is.na(ALL$sex)]
ALL$mol.biol <- factor(ALL$mol.biol, 
    levels = c('NEG', 'BCR/ABL'))
ALL <- featureFilter(ALL)

model <- model.matrix(~mol.biol+sex, ALL)
fit <- eBayes(lmFit(ALL, model))


publish(fit, myrep, eSet=ALL, factor=ALL$mol.biol, coef=2, n=100)

tt <- topTable(fit, coef = 2, n = 100)
selectedIDs <- unlist(mget(tt$ID, hgu95av2ENTREZID))
universeIDs <- unlist(mget(featureNames(ALL), hgu95av2ENTREZID))
goParams <- new("GOHyperGParams", 
    geneIds = selectedIDs, 
    universeGeneIds = universeIDs, 
    annotation = annotation(ALL), 
    ontology = "BP", 
    pvalueCutoff = 0.01,
    conditional = TRUE, 
    testDirection = "over")
goResults <- hyperGTest(goParams)
  }
