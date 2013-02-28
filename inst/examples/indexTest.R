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
myrep = htmlReportRef(reportDirectory = "./",shortName="indextest", handlers = fileWIndexHandlers)
stuff = publish(goResults, myrep, selectedIDs=selectedIDs, annotation.db="org.Hs.eg", name = "GO Results")

#trace(ReportingTools:::objectToHTML, browser, signature=c(object="ANY"))

fiveCol = function(df, ...) cbind(df, fives=5)
stuff2=publish(goResults, myrep, selectedIDs=selectedIDs, annotation.db="org.Hs.eg", .addColumns = list(addReportColumns,fiveCol), name = "GO Results with 5s")

finish(myrep )
