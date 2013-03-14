\name{Link}
\alias{Link}
\alias{Link-methods}
\alias{Link,character-method} 
\alias{Link,HTMLReportRef-method}
\alias{Link,list-method}

\title{
Create a table of Links
}
\description{
This is a convenience function for creating tables of links (eg for an
index page).
}
\usage{
Link(obj, target = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
    An object to be turned into one or more links. Typically a character
    vector of link text or a list of \code{HTMLReportRef} objects
  }
  \item{target}{
    The link targets for each created link. 
}
}
\details{
  If \code{target} is NA, Link attempts to infer the link target(s) based on \code{obj} 
}
\value{
  An \code{XMLInternalNode} corresponding to HTML for either a single
  link or a div containing multiple links separated each on separate lines
}
\author{
Gabriel Becker
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
mylinks = Link(c("Link1", "Link2"), c("www.google.com", "www.cnn.com"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ HTML }
\keyword{ Link }
