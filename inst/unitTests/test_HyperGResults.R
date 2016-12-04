library(GOstats)
library(hgu95av2.db)
library(org.Sc.sgd.db)

test_1HumanGOResults <- function()
{
    selectedIDs <- c("687","25","5445","7277","2273","2013","1490",
        "9788","168544","57556","216","92","2549","2022","1893")
    universeIDs <- unique(keys(hgu95av2.db, "ENTREZID"))
    goParams <- new("GOHyperGParams", 
                    geneIds = selectedIDs, 
                    universeGeneIds = universeIDs, 
                    annotation = "org.Hs.eg.db", 
                    ontology = "BP", 
                    pvalueCutoff = 0.01,
                    conditional = TRUE, 
                    testDirection = "over")
    goResults <- hyperGTest(goParams)
    df <- toReportDF(goResults)
    checkTrue(class(df) == "data.frame")
    htmlRep <- HTMLReport("testHumanGOResultshtmlPage",
        reportDirectory = 'testHTMLDirectory', title = "Test GOResults Report")
    publish(goResults, htmlRep)
    finish(htmlRep)
}

test_2YeastGOResults <- function()
{
    selectedIDs <- c("YAL046C", "YAL044W-A", "YKL040C", "YLL027W", "YOR226C", 
        "YPL135W", "YPR067W")
    universeIDs <- unique(keys(org.Sc.sgd.db, keytype = "ENSEMBL"))
    goParams <- new("GOHyperGParams", 
                    geneIds = selectedIDs, 
                    universeGeneIds = universeIDs, 
                    annotation = "org.Sc.sgd.db", 
                    ontology = "BP", 
                    pvalueCutoff = 0.01,
                    conditional = TRUE, 
                    testDirection = "over")
    goResults <- hyperGTest(goParams)
    df <- toReportDF(goResults)
    checkTrue(class(df) == "data.frame")
    htmlRep <- HTMLReport("testYeastGOResultshtmlPage",
        reportDirectory = 'testHTMLDirectory', title = "Test GOResults Report")
    publish(goResults, htmlRep, keytype = "ENSEMBL", 
        columns = list(EntrezId = "ENTREZID", EnsEMBL = "ENSEMBL", 
            Common = "COMMON", GeneName = "GENENAME"))
    finish(htmlRep)
}

test_3PFAMResults <- function()
{
    selectedIDs <- c("25", "2059", "4688", "7525")
    universeIDs <- unique(keys(hgu95av2.db, "ENTREZID"))
    pfamParams <- new("PFAMHyperGParams", 
                      geneIds = selectedIDs, 
                      universeGeneIds = universeIDs, 
                      annotation = "hgu95av2.db",  
                      pvalueCutoff = 0.01,
                      testDirection = "over")
    PFAMResults <- hyperGTest(pfamParams)
    htmlRep <- HTMLReport("testPFAMResultshtmlPage",
        reportDirectory = 'testHTMLDirectory', title = "Test PFAMResults Report")
    publish(PFAMResults, htmlRep)
    finish(htmlRep)
}
