options(error=recover)
library(ReportingTools)
library(ALL)
library(hgu95av2.db)
library(genefilter)
library(GOstats)
library(limma)
data(ALL)

ALL <- ALL[, ALL$mol.biol %in% c('NEG','BCR/ABL') &
    !is.na(ALL$sex)]
ALL$mol.biol <- factor(ALL$mol.biol, 
    levels = c('NEG', 'BCR/ABL'))
ALL <- featureFilter(ALL)

model <- model.matrix(~mol.biol+sex, ALL)
fit <- eBayes(lmFit(ALL, model))
myrep = HTMLReport(reportDirectory = "./",shortName="bigtest")
publish(fit, myrep, eSet=ALL, factor=ALL$mol.biol, coef=2, n=100, name = "bayesfit")

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

publish(goResults, myrep, selectedIDs=selectedIDs, annotation.db="org.Hs.eg", name="gohyperg")

library(Category)
pfamParams <- new("PFAMHyperGParams", 
    geneIds = selectedIDs, 
    universeGeneIds = universeIDs, 
    annotation = annotation(ALL),  
    pvalueCutoff = 0.01,
    testDirection = "over")
PFAMResults <- hyperGTest(pfamParams)

publish(PFAMResults, myrep, selectedIDs=selectedIDs, annotation.db="org.Hs.eg",categorySize=3, name = "hypergpfam")


library(GSEAlm)
library(GSEABase)
mapped_genes <- mappedkeys(org.Hs.egSYMBOL)
eidsAndSymbols <- as.list(org.Hs.egSYMBOL[mapped_genes])
geneEids<-names(eidsAndSymbols)
set.seed(123)
set1<-GeneSet(geneIds=sample(geneEids,100, replace=FALSE), setName="set1", 
	shortDescription="This is set1")
set2<-GeneSet(geneIds=sample(geneEids,10, replace=FALSE), setName="set2",  
	shortDescription="This is set2")
set3<-GeneSet(geneIds=sample(geneEids,37, replace=FALSE), setName="set3",  
	shortDescription="This is set3")
set4<-GeneSet(geneIds=sample(geneEids,300, replace=FALSE), setName="set4", 
	shortDescription="This is set4")
geneSets<-GeneSetCollection(c(set1,set2,set3,set4))

publish(geneSets, myrep, annotation.db="org.Hs.eg", name="geneset")

mat <- matrix(data=0, ncol=length(universeIDs),nrow=length(geneSets))
for(i in 1:length(geneSets)){
	geneIdEntrez<-unlist(geneIds(geneSets[[i]]))
	mat[i,match(geneIdEntrez, universeIDs)] <- 1
}
colnames(mat) <- universeIDs
rownames(mat) <- sapply(geneSets, function(x) x@setName)

lm<-lmPerGene(ALL, ~mol.biol+sex, na.rm=TRUE)
GSNorm<-GSNormalize(lm$tstat[2,], mat)
#one-sided p-values
pVals<-gsealmPerm(ALL,~mol.biol+sex, mat, nperm=100)  
bestPval<-apply(pVals,1, min)

publish(geneSets, myrep, annotation.db="org.Hs.eg", setStats=GSNorm, setPValues=2*bestPval, name="gsea")


myrep$finish()
