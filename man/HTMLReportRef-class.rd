\name{HTMLReportRef-class}
\Rdversion{1.1}
\docType{class}
\alias{HTMLReportRef-class}
\alias{HTMLReport}
\alias{[[<-,HTMLReportRef,ANY,ANY,ANY-method}
\alias{[[,HTMLReportRef,ANY-method}
\alias{publish,ANY,HTMLReportRef-method}
\alias{path,HTMLReportRef-method}
\title{Class \code{"HTMLReportRef"}}
\description{
A referenceClass-based representation of an HTML report generated using R objects. This class is based around a persistent, DOM-based representation of the HTML page being created, which is created during initialization and updated as the report is manipulated. \code{HTMLReport} is the constructor function for the class.
}
\section{Methods}{
  \describe{
    \item{[[<-}{\code{signature(x = "HTMLReportRef", i = "ANY", j = "ANY", value = "ANY")}: Place an element representing \code{value} into the report. This will replace any existing element by the same name in the report. \code{x$addElement} is used to perform the actual report manipulation.}
    \item{[[}{\code{signature(x = "HTMLReportRef", i = "ANY")}: Access an element within the report. The element will be returned in its DOM/HTML representation. }
    \item{publish}{\code{signature(object = "ANY", publicationType = "HTMLReportRef")}: Place an element representing \code{object} into the report. \code{publicationType$addElement} is used to perform the actual report manipulation.}
    \item{path}{\code{signature(object = "HTMLReportRef")}: The paths returned by each of the \code{ReportHandlers} for the report.}
}
}
\author{
	Gabriel Becker <gmbecker@ucdavis.edu>
}
\details{
Publication behavior of \code{HTMLReportRef} reports is controlled via an event-observer paradigm. Developers specify behavior in the form of one or more \code{ReportHandlers} objects. These objects contain instructions (in the form of R functions) to be carried out whenever certain actions are taken. These actions are: initial report creation ("init"), elements being added to the report ("addElement"), elements being removed from the report ("removeElement"), and the report being finalized ("finish"). See \code{\linkS4class{ReportHandlers}} for more detail.
}

\seealso{
\code{\linkS4class{ReportHandlers}}
}
\examples{
showClass("HTMLReportRef")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{shortName}:}{Short version of report name.}
    \item{\code{title}:}{Title of the report.}
    \item{\code{reportDirectory}:}{Directory (relative to \code{basePath}) in which to publish the report.}
    \item{\code{basePath}:}{Base path to publish to.}
    \item{\code{baseUrl}:}{Base URL report will be published to (used to generate link URLs, etc)}
    \item{\code{handlers}:}{A list of \code{ReportHandlers} objects specifying actions to be taken in reponse specific events during the report creation/manipulation process. See Details.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{prepare(obj, ...)}:}{ Prepare an R object to be added to the report by converting it to an HTML representation. Currently this is done by calling the \code{objectToHTML} function on the object, though this is likely to change in the future.}
    \item{\code{initialize(...)}:}{ Initialize the report, including generating the base HTML structure. The \code{init} function for each \code{ReportHandlers} object in \code{handlers} is called at the end of this process. }
    \item{\code{addElement(name, value, ...)}:}{ Add an element to the report. \code{prepare} is called to generate an HTML representation for \code{value}. That representation is then added to the internal DOM representation of the report. The \code{addElement} function for each \code{ReportHandlers} object in \code{handlers} is called at the end of this process. }
    \item{\code{finish()}:}{ Finalize a report. This causes the \code{finish} function on each element of \code{handlers} to be called. This is something of a misnomer in that the report can be further manipulated and "finished" again after this function has been called. }
  }

}
