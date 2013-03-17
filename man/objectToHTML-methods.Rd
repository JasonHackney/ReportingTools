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
\alias{objectToHTML,XMLInternalNode-method}

\title{ Transform R objects into HTML form to be inserted in HTML reports}
\description{
 Unless overridden by a .toHTML argument, this function is called to
 transform objects being published into HTML form before they are
 inserted into a report.
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ANY")}}{
  The default method for objectToHTML calls toReportDF (or the
  overriding .toDF) then addReportColumns (or the overriding
  .addColumns) to create a data.frame to be published. The data.frame
  method of objectToHTML is then called.

  
}

\item{\code{signature(object = "character")}}{
  If the character vector contains HTML code (determined by attempted
  parsing), it is parsed but otherwise unchanged. If not, it is added as
  content to a new <p> HTML node.
}

\item{\code{signature(object = "data.frame")}}{
  The data.frame is transformed into a filterable, sortable HTML table
}

\item{\code{signature(object = "ggbio")}}{
  The graphic is written to the report's figure directory and an <image>
  tag to display the image is returned
}

\item{\code{signature(object = "ggplot")}}{
  The graphic is written to the report's figure directory and an <image>
  tag to display the image is returned
}

\item{\code{signature(object = "recordedplot")}}{
  The graphic is written to the report's figure directory and an <image>
  tag to display the image is returned
}

\item{\code{signature(object = "trellis")}}{
  The graphic is written to the report's figure directory and an <image>
  tag to display the image is returned
}
}}
\keyword{ Report }
\keyword{ HTML }
\keyword{ publish }
