\name{makeDESeqDF}
\alias{makeDESeqDF}
\title{Converts DESeq results into a data frame to be published}
\usage{
    makeDESeqDF(object, countTable, pvalueCutoff, conditions,
annotation.db, expName, reportDir, ...)

}
\arguments{
  \item{object}{Output from nbinomTest; see DESeq manual for details}

  \item{countTable}{A table of the raw counts, where rows indicate genes and columns are samples}

  \item{pvalueCutoff}{A threshold value for what is considered a significantly different gene.  Only genes with p-values less than this will be included.}

  \item{conditions}{A vector indicating the groupings of the samples, for image purposes.}

  \item{annotation.db}{A character string, indicating the species of the samples}

  \item{expName}{The name of the experiment}

  \item{reportDir}{Directory to which the report (and figures) will be written}
  
    \item{...}{Unused arguments passed in from the publish method}
}
\value{
  ret, A data frame with the following values: Entrez Id, Symbol, Gene Name, Image, Log2 Fold Change, P-value and Adjusted p-value.

}
\description{
	Manipulates DESeq output to a data frame of significant observations.  Draws corresponding box and whisker plots.
}
\details{
	This function converts DESeq output into a data frame and draws the corresponding images
}
\author{
  Jessica Larson
}
\examples{
	#library(DESeq)
	#library(lattice)
	#data(mockRnaSeqData)
	#conditions <- c(rep("case",3), rep("control", 3))
	#cds<-newCountDataSet(mockRnaSeqData, conditions)
	#cds<-estimateSizeFactors(cds)
	#cds<-estimateDispersions(cds)
	#res<-nbinomTest(cds,"control", "case" )

	#desReport <- HTMLReport(shortName = 'RNAseq_analysis_with_DESeq',
    #title = 'RNA-seq analysis of differential expression using DESeq',
    #reportDirectory = "./reports")
	#publish(res,desReport,name="df",countTable=mockRnaSeqData,
    #pvalueCutoff=0.05,
    ##conditions=conditions,annotation.db="org.Mm.eg.db",
    #expName="deseq",reportDir="./reports", .modifyDF=makeDESeqDF)

#finish(desReport)
}
