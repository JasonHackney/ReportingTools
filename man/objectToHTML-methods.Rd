\name{objectToHTML-methods}
\docType{methods}
\alias{objectToHTML}
\alias{objectToHTML-methods}
\alias{objectToHTML,ANY-method}
\alias{objectToHTML,character-method}
\alias{objectToHTML,data.frame-method}
\alias{objectToHTML,ggbio-method}
\alias{objectToHTML,ggplot-method}
\alias{objectToHTML,recordedplot-method}
\alias{objectToHTML,trellis-method}
\title{ ~~ Methods for Function \code{objectToHTML} in Package \pkg{ReportingTools} ~~}
\description{
 ~~ Methods for function \code{objectToHTML} in package \pkg{ReportingTools} ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ANY")}}{
  The default method for objectToHTML calls toReportDF (or the
  overriding .toDF) then addReportColumns (or the overriding
  .addColumns) to create an HTML table from the object.

  
}

\item{\code{signature(object = "character")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "data.frame")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "ggbio")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "ggplot")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "recordedplot")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "trellis")}}{
%%  ~~describe this method here~~
}
}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
