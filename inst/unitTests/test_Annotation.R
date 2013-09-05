library(ALL)
library(limma)
library(hgu95av2.db)
library(org.Hs.eg.db)

test_AnnotationDims <- function()
{
    keys <- sample(keys(hgu95av2.db), 100)
    ann.vals <- ReportingTools:::annotate.genes(keys, hgu95av2.db,
        keytype = "PROBEID")
    checkTrue(nrow(ann.vals) == 100, "100 rows are returned when selecting 100 keys")
    checkTrue(all(colnames(ann.vals) == c("EntrezId", "Symbol", "GeneName")),
        "The expected column names are returned for annotations")
}

test_AnnotationData <- function()
{
    keys <- c("HBD", "THISDOESNTEXIST", "B2M", "B2M")
    ann.vals <- ReportingTools:::annotate.genes(keys, org.Hs.eg.db,
        keytype = "SYMBOL")
    checkTrue(nrow(ann.vals) == 4, "4 rows are returned for 4 keys, even with duplicates")
    checkTrue(is.na(ann.vals["THISDOESNTEXIST", "EntrezId"]), 
        "NA is returned for keys that aren't in the database")
    checkTrue(is.na(ann.vals["HBD", "EntrezId"]), 
        "NA is returned for keys that have 1:many relationships")
    checkTrue(sum(ann.vals[,"Symbol"] == "B2M") == 2,
        "2 rows are returned for a duplicated key")
}