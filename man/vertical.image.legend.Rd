\name{vertical.image.legend}
\alias{vertical.image.legend}

\title{Put color bar legend in the right plot margin.}


\description{ Put color bar legend in the right-hand side margin of an
existing plot.
}

\usage{
vertical.image.legend(zlim, col)
}



\arguments{
  \item{zlim}{Gives the range of z values to which the colors specified
    in \code{col} are assigned.}
  \item{col}{ Gives the range of colors to use.  To keep multiple plots
    consistent in terms of the colors assigned to various values, keep
    \code{zlim} and \code{col} the same for each of the plots and the legend.}
}


\details{This function makes two important assumptions.  The first is
  that the user has already finished making the main portion of the
  plot; i.e., user should \strong{add the legend last}.  The user should
  not try to add secondary information onto the plot after using this
  legend command.  This is because this funciton alters the
  \code{\link{par}} settings to draw the legend, and trying to reset
  them sometimes causes errors that are mysterious (at least to the
  writer of this function).  Secondly, this function works best when
  there is only one plot on the device, in which case the margin space
  is straightforward (no confusion between \code{oma}/\code{omi} and
  \code{mar}/\code{mai}, etc.}

\value{Puts vertical color bar legend to the right of the plot.}


\author{Jenise Swall}

\note{ As noted in the help for \code{\link[fields]{image.plot}} in the
  fields package, putting a legend on a plot is harder than it might
  seem.  The user may have to experiment with this function a bit to get
  it to work well for a specific application.  The user may also want to
  try the previously mentioned \code{\link[fields]{image.plot}} function
  in the fields package (to just add the legend, use
  \dQuote{legend.only=TRUE}).}


\seealso{\code{\link[fields]{image.plot}}}

\examples{
## Plot ozone at each location using colors from rainbow.colors
## and differently-sized points.  Add a legend using function
## vertical.image.legend (included in this package).
data(ozone1)
col.rng <- rev(rainbow(n=12, start=0, end=4/6))
plot3d.points(x=ozone1$longitude, y=ozone1$latitude, z=ozone1$daily.max,
              xlab="longitude", ylab="latitude", col=col.rng,
              cex.min=0.5, cex.max=1.5)
## To verify, label the points with their concentrations.
text(ozone1$longitude, ozone1$latitude+0.15, ozone1$daily.max, cex=0.7)
## If maps package is available, put on state lines.
if (require("maps"))  map("state", add=TRUE, col="lightgray")
## Put on legend.
vertical.image.legend(col=col.rng, zlim=range(ozone1$daily.max))
}


\keyword{ color }
\keyword{ aplot }
