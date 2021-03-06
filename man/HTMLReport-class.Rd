\name{HTMLReport-class}
\Rdversion{1.1}
\docType{class}
\alias{HTMLReport-class}
\alias{basePath}
\alias{baseUrl}
\alias{page}
\alias{page<-}
\alias{basePath,HTMLReport-method}
\alias{baseUrl,HTMLReport-method}
\alias{name<-,HTMLReport,character-method}
\alias{page,HTMLReport-method}
\alias{page<-,HTMLReport,character-method}
\alias{show,HTMLReport-method}
\alias{path,HTMLReport-method}
\alias{url,HTMLReport-method}

\title{Class \code{"HTMLReport"}}
\description{
This class has been superceded by \code{HTMLReportRef} and is
deprecated. The \code{HTMLReport} constructor function now creates an
\code{HTMLReportRef} object.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("HTMLReport", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{baseUrl}:}{ What is the base URL to which the HTMLReport's filename is appended. }
    \item{\code{basePath}:}{ What is the base file system URI to which the HTMLReport's filename is appended. }
    \item{\code{page}:}{ The connection to the html file. See \code{openPage} in the \code{hwriter} package. }
    \item{\code{shortName}:}{ A character string used as the base of the filename. '.html' is appended to the shortName to make the full file name. }
    \item{\code{title}:}{ The title of the HTMLReport. This is used as the page title in the header, and is also printed on the page. }
    \item{\code{reportDirectory}:}{ A file path that details where the report will be saved. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BaseReport}"}, directly.
}
\section{Methods}{
  \describe{
    \item{basePath}{ Return the basePath for the HTMLReport }
    \item{baseUrl}{ Return the baseUrl for the HTMLReport }
    \item{filename}{ Return the file name of the HTMLReport. This is generated by adding '.html' to the shortName of the report. }
    \item{finish}{ This is the final step in publishing results to an HTMLReport. It prints the closing tags for the html body, and then closes the connection to the html file. Further calls to publish will fail because the connection is closed. }
    \item{name<-}{ Set the short name for the html file. }
    \item{page}{ Returns the connection to the html file. }
    \item{page<-}{ Set the connection to the html file. }
    \item{validConnection}{ Is the connection to the html file still open? If finish has been called, this will return FALSE. }
	 }
}
\keyword{classes}
