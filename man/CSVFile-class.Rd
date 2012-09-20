\name{CSVFile-class}
\Rdversion{1.1}
\docType{class}
\alias{CSVFile-class}
\alias{name<-,CSVFile,character-method}

\title{Class \code{"CSVFile"}}
\description{
A CSVFile is a pointer to a comma separated value file on the file system. Publishing to a CSVFile overwrites the current contents of the given CSVFile.
}
\section{Objects from the Class}{
Objects from this class...
}
\section{Slots}{
  \describe{
    \item{\code{shortName}:}{ A character string used as the base of the filename. '.csv' is appended to the shortName to make the full file name. }
    \item{\code{title}:}{ The title of the report. This doesn't appear anywhere in the output, but can be used to track the report in an R session. }
    \item{\code{reportDirectory}:}{ A file path that details where the csv file will be saved. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BaseReport}"}, directly.
}
\section{Methods}{
  \describe{
    \item{filename}{ The name of the csv file, including extension }
    \item{name<-}{ Set the name of the CSVFile object }
  }
}
\examples{
showClass("CSVFile")
}
\keyword{classes}
