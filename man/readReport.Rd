\name{readReport}
\alias{readReport}

\title{
Read in an Existing Report
}
\description{
This function is used to read an existing Report (previously created
using ReportingTools) into R so that it can modified or computed on.
}
\usage{
readReport(reportFile, handlers = fileHandlers(reportFile), 
  .toHTML = NULL, .toDF = NULL, .modifyDF = NULL, title)
}
\arguments{
  \item{reportFile}{
    A character value pointing to the HTML file containing an existing Report.
}
  \item{handlers}{
    The ReportHandlers object(s) to attach to the resulting HTMLReportRef object.
}
  \item{.toHTML}{
    The list of .toHTML overrides to attach to the HTMLReportRef object
  }
  \item{.toDF}{
    The list of .toDF overrides to attach to the HTMLReportRef object
}
  \item{.modifyDF}{
    The list of .modifyDF overrides to attach to the HTMLReportRef object

}
  \item{title}{
    Title to assign to the new HTMLReportRef object. If not specified
  this value is inferred from the file.
}
}
\value{
An HTMLReportRef object representing the Report.
}
\author{
  Gabriel Becker
}
\note{
  Attempting to write the modified report to a new location without
  updating any report elements involving figures will result in broken
  image links because image locations are assumed to be relative to the
  location of the HTML file.
}

\seealso{
\code{\linkS4class{HTMLReportRef}}
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
