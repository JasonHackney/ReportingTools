\name{finish-methods}
\docType{methods}
\alias{finish}
\alias{finish-methods}
\alias{finish,DataPackage-method}
\alias{finish,HTMLReport-method}
\title{ Finalizing reports after publishing results}
\description{
This is a method for finalizing a report after results have been published. The exact nature of finalizing depends on the report type, as detailed below.
}
\section{Methods}{
\describe{

\item{\code{signature(publicationType = "DataPackage")}}{
Calling \code{finish} on a DataPackage object rewrites the DESCRIPTION file, making sure that all of the dependencies for the objects in the DataPackage are listed.
}

\item{\code{signature(publicationType = "HTMLReport")}}{
Calling \code{finish} on an HTMLReport calls the \code{hwrite} function \code{closePage}, which closes the body and html tags on the page and closes the connection to the file.
}
}}
\keyword{methods}
\examples{
my.df <- data.frame()
## html.report <- HTMLReport(shortName = "my_html_file",
##     reportDirectory = "reportDirectory")
# publish(my.df, html.report)
## finish(html.report)
}
