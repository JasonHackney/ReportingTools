library(GSEABase)
library(org.Hs.eg.db)
library(hgu95av2.db)

geneIds <- mappedkeys(org.Hs.egSYMBOL)

geneSet1 <- GeneSet(geneIds = sample(geneIds, 100), setName = "Set1", 
    shortDescription = "This is set 1", 
    geneIdType = EntrezIdentifier())
geneSet2 <- GeneSet(geneIds = sample(geneIds, 100), setName = "Set2", 
    shortDescription = "This is set 2", 
    geneIdType = EntrezIdentifier("org.Hs.eg.db"))

probes <- mappedkeys(hgu95av2ENTREZID)
geneSet3 <- GeneSet(geneIds = sample(probes, 100), setName = "Set3",
    shortDescription = "This is set 3",
    geneIdType = AnnotationIdentifier("hgu95av2"))

geneStats <- rnorm(100)
names(geneStats) <- geneIds(geneSet2)

geneSetCollection <- GeneSetCollection(list(geneSet1, geneSet2, geneSet3))

test_1GeneSetToDF <- function()
{
    df1 <- toReportDF(geneSet1)
    checkTrue(nrow(df1) == 100, "There are 100 rows in the data.frame")
    checkTrue(ncol(df1) == 1, "There is one column in the data.frame")
    
    df2 <- toReportDF(geneSet2)
    checkTrue(all.equal(colnames(df2), c("EntrezId", "Symbol", "Gene Name")),
        "GeneSets with annotation slots get annotated features")
    
    df3 <- toReportDF(geneSet1, annotation.db = "org.Hs.eg.db")
    checkTrue(all.equal(colnames(df3), c("EntrezId", "Symbol", "Gene Name")),
        "Adding annotation.db adds feature annotations")
    
    df4 <- toReportDF(geneSet2, geneStats = geneStats)
    checkTrue(ncol(df4) == 4, "Adding geneStats appends a new column to the data.frame")
    
    df5 <- toReportDF(geneSet3)
    checkTrue(all.equal(colnames(df5), c("ID", "EntrezId", "Symbol", "Gene Name")), "GeneSets with of AnnotationIdentifiers get annotated features.")
    
    df6 <- toReportDF(geneSet1, annotation.db = "org.Hs.eg")
    checkTrue(all.equal(colnames(df3), c("EntrezId", "Symbol", "Gene Name")),
        "annotation.db works for both org.Hs.eg.db and org.Hs.eg")    
}

test_2ModifyDF <- function()
{
    df1 <- toReportDF(geneSet3)
    df2 <- modifyReportDF(df1, object = geneSet3)
    checkTrue(sum(grepl("www.ncbi.nlm.nih.gov", df1$EntrezId)) == 0,
        "No URLs in the starting data.frame")
    checkTrue(sum(grepl("www.ncbi.nlm.nih.gov", df2$EntrezId)) == 100,
        "All Entrez IDs get a URL")
}
