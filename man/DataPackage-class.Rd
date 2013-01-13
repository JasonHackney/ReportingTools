\name{DataPackage-class}
\Rdversion{1.1}
\docType{class}
\alias{DataPackage-class}
\alias{dependencies}
\alias{dependencies<-}
\alias{dependencies,DataPackage-method}
\alias{dependencies<-,DataPackage,character-method}
\alias{path,DataPackage-method}

\title{Class \code{"DataPackage"}}
\description{
This is a pointer to an R data package. The result should be an installable R data package, with documentation for the data objects published therein.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DataPackage", ...)}.

}
\section{Slots}{
  \describe{
    \item{\code{version}:}{ The current version of the generated data package }
    \item{\code{dependencies}:}{ A character vector of packages that the data package depends on. }
    \item{\code{author}:}{ The author(s) of the data package. }
    \item{\code{maintainer}:}{ The maintainer(s) of the data package. Defaults to the same person as the author. }
    \item{\code{license}:}{ What license to use when creating the data package. }
    \item{\code{description}:}{ The description of the data package. }
    \item{\code{package.Rd}:}{ Rd string for package-level help for the generated data package. }
    \item{\code{shortName}:}{ Name of the data package. }
    \item{\code{title}:}{ The title of the package }
    \item{\code{reportDirectory}:}{ The directory in which the data package is generated. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BaseReport}"}, directly.
}
\section{Methods}{
  \describe{
    \item{dependencies}{ Retrieve the list of dependencies for the data package. }
    \item{dependencies<-}{ Set the list of dependencies for the data package. }
    \item{finish}{ This is the final step in publishing objects to the data pacakge. It recreates the DESCRIPTION file for the data package. The package name, title, version, author, maintainer, dependencies, license and description are set from the appropriate slots in the DataPackage object. }
  }
}
\examples{
my.df <- data.frame(EGID = c("103", "104", "105", "106", "107"),
                    RPKM = c(4, 5, 3, 100, 75),
                    DE = c("Yes", "Yes", "No", "No", "No"))
data.package <- DataPackage('MyPackage', title = "My awesome package",
    author = "J.J. Nobody", maintainer = "J.J. Nobody <nobody@nowhere.net>")
publish('my.df', data.package)

dependencies(data.package) # Returns "Biobase"
dependencies(data.package) <- c('Biobase','GSEABase')

finish(data.package)
}
\keyword{classes}
