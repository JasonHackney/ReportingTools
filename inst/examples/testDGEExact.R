library(ReportingTools)
library(edgeR)
library(org.Mm.eg.db)

data(mockRnaSeqData)
conditions <- c(rep("case",3), rep("control", 3))
d <- DGEList(counts = mockRnaSeqData, group = conditions)
d <- calcNormFactors(d)
d <- estimateCommonDisp(d)
## Get an edgeR object
edgeR.de <- exactTest(d)
min.pval <- min(edgeR.de$table$PValue)/10

myrep = HTMLReport("DGEExactTest",reportDirectory = "tmp")
#publish(edgeR.de, myrep,d$counts, conditions, annotation.db='org.Mm.eg', pvalueCutoff=1, lfc=0, n=100, name ="mydge")
myrep[["mydge", countTable = d$counts, conditions = conditions, annotation.db='org.Mm.eg', pvalueCutoff=1, lfc=0, n=100]] = edgeR.de
myrep$finish()
