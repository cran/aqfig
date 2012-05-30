\name{kristen.colors}
\alias{kristen.colors}

\title{Sets up a color palette which is particularly appropriate for
  use with the scatterplot.density function}

\description{This function sets up a color palette with \code{n} colors,
  with gradually changing hues of grey, purple, blue, green, yellow,
  orange, red, and brown (in this order).  This palette is a
  particularly good choice for \code{\link{scatterplot.density}}
  function, for which it is the default choice.
}


\usage{kristen.colors(n=64)
}


\arguments{
  \item{n}{Number of colors to include in palette.}
}


\details{This function calls \code{\link{colorRampPalette}} and returns
  \code{n} colors from this palette.
}


\value{Returns \code{n} colors (in hexadecimal).}

\note{This function is called by default by the function
  \code{\link{scatterplot.density}}, which is included in this
  package.}

\author{Original code was authored by Kristen Foley and included in the
  precursor to \code{\link{scatterplot.density}}.  Packaged into a new
  function by Jenise Swall.}


\seealso{\code{\link{colorRampPalette}}}



\keyword{ hplot }
\keyword{ color }
