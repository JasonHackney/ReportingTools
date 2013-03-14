\name{addReportColumns-methods}
\docType{methods}
\alias{addReportColumns}
\alias{addReportColumns-methods}
\alias{addReportColumns,ANY,ANY,ANY-method}
\alias{addReportColumns,ANY,ANY,DGEExact-method}
\alias{addReportColumns,ANY,ANY,DGELRT-method}
\alias{addReportColumns,ANY,ANY,MArrayLM-method}
\alias{addReportColumns,ANY,ANY,GOHyperGResult-method}
\title{ Decorate the Basic data.frame Representation of an Object}
\description{
 This function is called after toReportDF (or the overriding.toDF if it
 is specified) in order to transform the basic data.frame into the form
 that will appear in the report. This typically includes transforming
 columns into links, adding image columns, etc. Each class publishable
 by ReportingTools should have either a custom objectToHTML method
 (which supercedes toReportDF and addReportColumns) or a custom
 toReportDF and addReportColumns methods which construct a dataframe to
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
