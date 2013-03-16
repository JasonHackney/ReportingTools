\name{validConnection}
\alias{validConnection}

\title{
Determine connection validity
}
\description{
Determine if a connection to an HTML page can be written to or not.
}
\usage{
validConnection(htmlRep)
}

\arguments{
    \item{htmlRep}{
    An object of class \code{HTMLReport}.
    }
}
\value{
Returns TRUE if the page can be written to. If the file handle has been closed, then FALSE.
}
\seealso{
\code{link{HTMLReport-class}}
}
\examples{

my.df <- data.frame(EGID = c("103", "104", "105", "106", "107"),
                    RPKM = c(4, 5, 3, 100, 75),
                    DE = c("Yes", "Yes", "No", "No", "No"))
html.report <- makeOldHTMLReport(shortName = "my_html_file",
     reportDirectory = "reportDirectory")
publish(my.df, html.report)
validConnection(html.report) # Returns TRUE

finish(html.report)
validConnection(html.report) # Returns FALSE
}
