\name{BaseReport-class}
\Rdversion{1.1}
\docType{class}
\alias{reportDirectory}
\alias{reportDirectory<-}
\alias{path}
\alias{title}
\alias{title<-}
\alias{name}
\alias{BaseReport-class}
\alias{name,BaseReport-method}
\alias{name<-,BaseReport,character-method}
\alias{reportDirectory,BaseReport-method}
\alias{reportDirectory<-,BaseReport,character-method}
\alias{show,BaseReport-method}
\alias{title,BaseReport-method}
\alias{title<-,BaseReport,character-method}
\alias{path,BaseReport-method}

\title{Class \code{"BaseReport"}}
\description{
A ReportType defines a way of representing an R object in a different, ordered format. Several ReportTypes have been detailed in this package, including classes for HTML reports and R data packages.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BaseReport", ...)}. This is an abstract class that is inherited by other ReportTypes, and should not be used directly.
}
\section{Slots}{
  \describe{
    \item{\code{shortName}:}{ A character string generally used as the filename for reports }
    \item{\code{title}:}{ A character string that gives a longer description of what the report shows. }
    \item{\code{reportDirectory}:}{ A file path that details where the report will be saved. }
  }
}
\section{Methods}{
  \describe{
    \item{name}{\code{signature(object = "BaseReport")}: Get the shortName assigned to the report. }
    \item{reportDirectory}{\code{signature(object = "BaseReport")}: Get the directory where the report will be generated. }
    \item{reportDirectory<-}{\code{signature(object = "BaseReport", value = "character")}: Set the directory where the report will be generated. }
    \item{show}{\code{signature(object = "BaseReport")}: ... }
    \item{title}{\code{signature(main = "BaseReport")}: Return the title of the report }
    \item{title<-}{\code{signature(object = "BaseReport", value = "character")}: Set the title of the report. }
    \item{path}{\code{signature(object = "BaseReport")}: Get the filesystem location of the report. }
	 }
}
\seealso{
\code{\linkS4class{HTMLReport}}
\code{\linkS4class{DataPackage}}
\code{\linkS4class{CSVFile}}
}
\examples{
showClass("BaseReport")
}
\keyword{classes}
