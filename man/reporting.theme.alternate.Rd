\name{reporting.theme.alternate}
\alias{reporting.theme.alternate}

\title{
Alternate color theme for use in ReportingTools
}
\description{
Getting some alternate attractive colors for use in lattice graphics
}
\usage{
reporting.theme.alternate()
}
\value{
A list with slots as defined by the lattice function \code{standard.theme}
}
\seealso{
\code{\link{standard.theme}}
}
\examples{
library(lattice)
theme <- reporting.theme.alternate()
lattice.options(default.theme = theme)
}
