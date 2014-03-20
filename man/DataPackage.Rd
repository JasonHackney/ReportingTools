\name{DataPackage}
\alias{DataPackage}
\title{
Function for creating and initializing an object of class DataPackage.
}
\description{
This is a function for creating a DataPackage. Calls to publish an object to the package serialize the object to disk, adding a file to the data directory, and adding a stubbed .Rd documentation file for the dataset.
}
\usage{
DataPackage(shortName, title = "", reportDirectory = ".", version =
"0.0.1", dependencies = c("Biobase"), license = "", description = "",
author = "nobody", maintainer = "nobody <nobody@nothing.net>")
}
\arguments{
  \item{shortName}{
A character string giving the title of the DataPackage. This is used as both the title in the DESCRIPTION file and as the directory name for the package.
}
  \item{reportDirectory}{
Where the DataPackage directory will be created.
}
  \item{title}{
A character string giving the title of the package.
}
  \item{version}{
A character string giving the version of the DataPackage.
}
  \item{dependencies}{
A character vector listing what packages the DataPackage depends on.
}
  \item{license}{
A character string detailing the license the DataPackage published under. 
}
  \item{description}{
A character string giving the description of the DataPackage.
}
  \item{author}{
A character string giving the author of the package.
}
  \item{maintainer}{
A character string giving the maintainer of the package.
}
}
\value{
An object of class \code{DataPackage}. As a side effect, the directory structure of the data package will also be created in the location given by reportDirectory. On publication, objects are saved to the data directory of the package, and whatever dependencies the objects imply are added to the list of package dependencies.
}
\seealso{
\code{\link{DataPackage-class}}
}
\examples{
my.df <- data.frame(EGID = c("103", "104", "105", "106", "107"),
                    RPKM = c(4, 5, 3, 100, 75),
                    DE = c("Yes", "Yes", "No", "No", "No"))
data.package <- DataPackage('MyPackage', title = "My awesome package",
    author = "J.J. Nobody", maintainer = "J.J. Nobody <nobody@nowhere.net>")
publish('my.df', data.package)
}
