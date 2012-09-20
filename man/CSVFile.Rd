\name{CSVFile}
\alias{CSVFile}
\title{
Function for creating and initializing a CSVFile for publishing results
}
\description{
A CSVFile is a pointer to a comma separated value file on the file system. Publishing to a CSVFile overwrites the current contents of the given CSVFile.
}
\usage{
CSVFile(shortName, title = "", reportDirectory = ".")
}
\arguments{
  \item{shortName}{
A character string giving a short title for the report. Used as the base of the filename.
}
  \item{reportDirectory}{
A character string giving the location of the report.
}
  \item{title}{
A character string giving a longer description of the report.
}
}
\value{
An object of class \code{CSVFile}, which will represents a file to be written to. There are no open filehandles, and calling publish multiple times will result in the file being overwritten.
}
\seealso{
\code{\link{CSVFile-class}}, \code{\link{HTMLReport-class}}
}
\examples{
my.df <- data.frame(EGID = c("103", "104", "105", "106", "107"),
                    RPKM = c(4, 5, 3, 100, 75),
                    DE = c("Yes", "Yes", "No", "No", "No"))
csv.file <- CSVFile(shortName = "my_csv_file",
    reportDirectory = "reportDirectory")
publish(my.df, csv.file)
}
