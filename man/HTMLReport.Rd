\name{makeOldHTMLReport}
\alias{makeOldHTMLReport}
\title{
Creating and initializing an HTMLReport object
}
\description{
This is the function one shoud use to instantiate an HTMLReport. It basically
creates the report in the appropriate place, creating directories if needed.
Special care should be taken if non-default javacsript or css files are used, as
detailed below.
}
\usage{
makeOldHTMLReport(shortName, title = NULL, reportDirectory = ".", baseUrl = "localhost", basePath = NULL, page = NULL, link.css = NULL, link.javascript = NULL, overwrite.js=TRUE)
}
\arguments{
  \item{shortName}{
A short name for the report. It is used as the base of the HTML file.
}
  \item{title}{
The title for the page. It will be used in the title element in the page head, and will also be written at the top of the page.
}
  \item{reportDirectory}{
A character string giving the directory where the report will be written.
}
  \item{baseUrl}{
A character string giving the location of the page in terms of HTML locations.
}
  \item{basePath}{
A character string giving the location of the page in terms of filesystem locations.
}
  \item{page}{
The file handle for the html page, returned from the \code{hwriter} function \code{openPage}.
}
  \item{link.css}{
A character vector of file URLs detailing where to get style sheets. If unset, the default styling for ReportingTools is used.
}
  \item{link.javascript}{
A character vector of file URLs detailing where to get javascript. If unset, the default javascript libraries for ReportingTools are used.
}
  \item{overwrite.js}{
  logical whether css and javascript files should be overwritten when copied into the report directory.
  }
}
\details{
Care should be used if \code{link.javascript} or \code{link.css} are set. If the
javascript libraries for ReportingTools aren't linked, then it is likely that
the resulting pages will not be as interactive as the default. The default css
files are found in the extdata/csslib directory of the ReportingTools package,
while the default javascript libraries are found in the extdata/jslib directory
of the ReportingTools package. The default behavior is for the css and js files
to be copied from the package directory into the \code{reportDirectory} of the
\code{HTMLReport}. If these are copied to an alternate location, then the user
can set the environment variables REPORTINGTOOLSCSSLIB and REPORTINGTOOLSJSLIB
to point to the locations of the css and javascript files, respectively.
}
\value{
An HTMLReport object with an open file handle to an HTML page.
}
\seealso{
\code{\link{HTMLReport-class}}, \code{\link{publish}}
}
\examples{
my.df <- data.frame(EGID = c("103", "104", "105", "106", "107"),
                    RPKM = c(4, 5, 3, 100, 75),
                    DE = c("Yes", "Yes", "No", "No", "No"))
html.report <- makeOldHTMLReport(shortName = "my_html_file",
    reportDirectory = "reportDirectory")
publish(my.df, html.report)
}
