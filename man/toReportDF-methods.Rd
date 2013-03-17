\name{toReportDF-methods}
\docType{methods}
\alias{toReportDF}
\alias{toReportDF-methods}
\alias{toReportDF,ANY-method}
\alias{toReportDF,data.frame-method}
\alias{toReportDF,DGEExact-method}
\alias{toReportDF,DGELRT-method}
\alias{toReportDF,GeneSetCollection-method}
\alias{toReportDF,GOHyperGResult-method}
\alias{toReportDF,MArrayLM-method}
\alias{toReportDF,PFAMHyperGResult-method}
\title{ Class Specific Default Behavior for Transforming Objects to data.frames}
\description{
 The \code{toReportDF} generic and its methods are used to transform
 objects into a basic tabular (data.frame) form as part of the two step
 process of preparing to insert them into a report. This function (and
 functions passed to the \code{.toDF} argument used to override it) will
 only be called during the default ("ANY") \code{objectToHTML}
 method. Non-default \code{objectToHTML} methods are expected to perform
 the entire process of creating HTML code from an R object. 
}
\note{
The default \code{toReportDF} method is equivalent to calling \code{as.data.frame}.
}
\seealso{
  \code{\link{modifyReportDF}}
}
\keyword{methods}
\keyword{ data.frame}
\keyword{report}
