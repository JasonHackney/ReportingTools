\name{reporting.theme}
\alias{reporting.theme}

\title{
Color theme for use in ReportingTools
}
\description{
Getting some attractive colors for use in lattice graphics
}
\usage{
reporting.theme()
}
\value{
A list with slots as defined by the lattice function \code{standard.theme}
}
\seealso{
\code{\link{standard.theme}}
}
\examples{
library(lattice)
theme <- reporting.theme()
lattice.options(default.theme = theme)
}
