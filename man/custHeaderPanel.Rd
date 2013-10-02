\name{custHeaderPanel}
\alias{custHeaderPanel}
\title{Define a header panel with custom JS/CSS for a Shiny app}
\usage{
    custHeaderPanel(title, windowTitle = title, js = NULL, css = NULL) 
}
\arguments{
  \item{title}{Title of the shiny app}

  \item{windowTitle}{Title of the window for the shiny app}

  \item{js}{Custom javascript to be loaded in the app}

  \item{css}{Custom CSS to be loaded in the app}
}

\value{
  The return value is not directly meaningful to the end user and is
  intended to be passed into shiny layout functions such as \code{pageWithSidebar}.
}
\description{
  Defines a header panel which loades custom Javascript and CSS files.
}
\details{
  See the shiny vignette for example usage.
}
\seealso{
  \code{\link[shiny]{pageWithSidebar}}
}
\author{
  Gabriel Becker and Jessica L. Larson 
}