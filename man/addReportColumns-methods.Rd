\name{modifyReportDF-methods}
\docType{methods}
\alias{modifyReportDF}
\alias{modifyReportDF-methods}
\alias{modifyReportDF,ANY,ANY,ANY-method}
\alias{modifyReportDF,ANY,ANY,DGEExact-method}
\alias{modifyReportDF,ANY,ANY,DGELRT-method}
\alias{modifyReportDF,ANY,ANY,DESeqDataSet-method}
\alias{modifyReportDF,ANY,ANY,MArrayLM-method}
\alias{modifyReportDF,ANY,ANY,GeneSet-method}
\alias{modifyReportDF,ANY,ANY,GOHyperGResult-method}
\title{ Decorate the Basic data.frame Representation of an Object}
\description{
 This function is called after toReportDF (or the overriding.toDF if it
 is specified) in order to transform the basic data.frame into the form
 that will appear in the report. This typically includes transforming
 columns into links, adding image columns, etc. Each class publishable
 by ReportingTools should have either a custom objectToHTML method
 (which supercedes toReportDF and modifyReportDF) or a custom
 toReportDF and modifyReportDF methods which construct a dataframe to
 be placed in the report as an HTML table.
}
\section{Methods}{
\describe{
\item{\code{signature(df = "ANY", htmlRep = "ANY", object = "ANY")}}{
The default method is simply a placeholder, and returns the data.frame unchanged.
}
}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
