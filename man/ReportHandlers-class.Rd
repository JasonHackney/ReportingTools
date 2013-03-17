\name{ReportHandlers-class}
\Rdversion{1.1}
\docType{class}
\alias{ReportHandlers-class}
\alias{fileHandlers}
\alias{fileWIndexHandlers}
\alias{knitrHandlers}
\alias{shinyHandlers}
\alias{connectionHandlers}
\alias{path, ReportHandlers-method}
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
<<<<<<< HEAD
    \item{\code{location}:}{For \code{fileHandlers}, the path to the file that will be written}
    \item{\code{args}:}{A named list containing zero or more of the elements "init", "addElement", "removeElement", and "finish", which contain additional arguments to be passed to the respective functions when called. }
=======
    \item{\code{args}:}{A named list containing zero or more of the
      elements "init", "addElement", "removeElement", and "finish",
      which contain additional arguments to be passed to the respective
      functions when called. }
    \item{\code{locatoin}:}{An R object indicating the location these
      handlers will publish the report to. The exact meaning of this
      argument depends on the behavior of the handlers.}
>>>>>>> preparing to rebase for jason's changes
  }
}
\section{Methods}{
  \describe{
    \item{path}{\code{signature(object = "ReportHandlers")}: For file handlers, the file system location where the file will be written. For all others, NULL.}
  }
}

\details{
  ReportingTools ships with a number of predefined ReportHandlers
  constructorslo functions which generate ReportHandlers objects which implement commonly desired behaviors:
  \describe{
    \item{fileHandlers}{Accepts destination file
      location in the form of a character value, which is stored in the
      object's location slot. Report is built up internally and written to a
      file via the \code{finish} observer. }
    \item{fileWIndexHandlers}{As with fileHandlers except that a table of contents is appended to the be beginning of the report with links to each element within the page. Link text is the element names}
    \item{connectionHandlers}{Accepts a connection to which the report
      should be streamed (which is stored in the location slot). Report is streamed out to the provided
      connection as elements are added to the report. code{finish} completes the page and closes the connection.}
    \item{knitrHandlers}{Accepts an option filename argument which will
      be stored as the location slot, but is not used
      internally. Elements are streamed in HTML form to standard output,
      which is captured by knitr and inserted into the page, allowing ReportingTools to be used as a formatting mechanism to publish objects within a knitr document and have them displayed correctly. Note in this case ReportingTools has no control over where the final HTML file is written}
    \item{shinyHandlers}{These handlers allow ReportingTools to be used
      as a formatting mechanism within a shiny Web application. Note in
      this case there is no meaninggful HTML file being created that
      represents the report. location is set to \code{NULL} as no
      meaningful location exists in the shiny case.}
  }
  
}
<<<<<<< HEAD

=======
\section{Methods}{
  \describe{
    \item{path}{Returns the location associated with these ReportHandlers}
  }
>>>>>>> preparing to rebase for jason's changes
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
