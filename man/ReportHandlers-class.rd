\name{ReportHandlers-class}
\Rdversion{1.1}
\docType{class}
\alias{ReportHandlers-class}

\title{Class \code{"ReportHandlers"}}
\description{
A set of event-handler functions to be called when certain actions are performed on a \code{HTMLReportRef} object.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ReportHandlers", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{init}:}{A function to be called when the report is initialized/created }
    \item{\code{addElement}:}{A function to be called when an element is added/published to the report }
    \item{\code{removeElement}:}{A function to be called when an element is removed from the report }
    \item{\code{finish}:}{A function to be called when the report is "finished" }
    \item{\code{args}:}{A named list containing zero or more of the elements "init", "addElement", "removeElement", and "finish", which contain additional arguments to be passed to the respective functions when called. }
  }
}
\author{
Gabriel Becker <gmbecker@ucdavis.edu>
}
\seealso{
	\code{\linkS4class{HTMLReportRef}}
}
\examples{
showClass("ReportHandlers")
}
\keyword{classes}
