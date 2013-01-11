\name{plot3d.points}
\alias{plot3d.points}

\title{Make 3-D scatterplot (using colored or differently-sized points)}

\description{Given a list of points' coordinates and the values
  observed at those points, return a scatterplot with points located
  as specified by the coordinates and coded by color and/or size to
  represnt the observed value at the location.  This code is basically
  a wrapper for a call to the function \code{\link[geoR]{points.geodata}} in
  the geoR package.}

\usage{
plot3d.points(x, y, z, zlim = range(z, na.rm = TRUE),
   add = FALSE, col = heat.colors(12), xlab, ylab,
   pch = 21, cex.min = 1, cex.max = 1,
   symbol.border.col = "black",
   plt.beyond.zlim = FALSE, ...)
}


\arguments{
  \item{x}{x-coordinates of locations at which response values \code{z}
    are recorded.}
  \item{y}{y-coordinates of locations at which response values \code{z}
    are recorded.}
  \item{z}{Response values \code{z} observed at locations whose coordinates
    are given by (\code{x}, \code{y}).}

  \item{zlim}{Vector of minimum and maximum values of \code{z} to which
    to assign the two most extreme colors in the \code{col} argument
    (col[1] and col[length(col)]).  Default is to use the range of
    \code{z}.  This is very much like the \code{zlim} argument to the
    \code{\link{image}} function.}

  \item{add}{If FALSE (default), the function will begin a new plot. If
    TRUE, adds scatterplot to a pre-existing plot.}
  \item{col}{Color range to use for the scatterplot, with the first
    color assigned to zlim[1] and last color assigned to zlim[2].
    Default is \dQuote{heat.colors(12)}, as it is for \code{\link{image}}.}

  \item{xlab}{The label for the x-axis.  If not specified by the user,
    defaults to the expression the user named as parameter \code{x}.
    This behavior is similar to that for \code{\link{plot}}.}
  \item{ylab}{The label for the y-axis.  If not specified by the user,
    defaults to the expression the user named as parameter \code{y}.
    This behavior is similar to that for \code{\link{plot}}.}
  
  \item{pch}{The point symbol to use.  Possible values are 21, 22, 23,
    24, and 25.  This is because \code{\link[geoR]{points.geodata}}
    requires these points, which have outlines around them.  Default is
    a circle (\sQuote{pch=21}).}
  \item{cex.min}{Minimum amount to shrink/expand the point symbols.}
  \item{cex.max}{Maximum amount to shrink/expand the point symbols.}
  
  Parameters \code{cex.min} and \code{cex.max} control the minimum
  and maximum amounts to shrink/expland the points, based on the
  value of \code{z}.  By default, these are both set to one, which
  makes all the points the same size. For more information, see
  the help page for \code{\link{points.geodata}}.

  \item{symbol.border.col}{This controls the color of the border around
  the plotting symbol.  By default, it is black.  If a border is not
  desired, use \sQuote{symbol.border.col=\dQuote{transparent}}.}

  \item{plt.beyond.zlim}{IF TRUE, and if \code{zlim} is specified by the
  user, z values beyond the limits given in \code{zlim} are
  plotted. Values less than zlim[1] are plotted in the same color as
  zlim[1]; values greater than zlim[2] are plotted in the same color as
  zlim[2].  If TRUE, and \code{zlim} is not specified by the user,
  zlim[1] and zlim[2] will be assigned the minimum and maximum values of
  \code{z}.  In this case, user is warned and plt.beyond.zlim is set to
  FALSE.  Default is plt.beyond.zlim==FALSE.}

  \item{\dots}{Any other parameters the user adds will be passed to the
    \code{\link{plot}} function if \sQuote{add=FALSE}, and may include
    options for axis labels, titles, etc.}
}


\details{This function is a wrapper to the \code{\link{points.geodata}}
  function in the geoR package.
}


\value{A scatterplot with points at (\code{x},\code{y}).  These points
  are colored according to the correspoinding value of \code{z} and
  the colors specified in \code{col}.  They are sized according to the
  corresponding value of \code{z} and the minimum and maximum sizes
  specified by \code{cex.min} and \code{cex.max}.
}

\author{Jenise Swall}


\seealso{\code{\link{points.geodata}}, \code{\link{points}}}

\examples{

## Plot ozone at each location using colors from rainbow.colors
## and differently-sized points.  Add a legend using function
## vertical.image.legend (included in this package).
data(ozone1)
col.rng <- rev(rainbow(n=10, start=0, end=4/6))
z.rng <- c(40, 90)
plot3d.points(x=ozone1$longitude, y=ozone1$latitude, z=ozone1$daily.max,
              xlab="longitude", ylab="latitude", col=col.rng,
              zlim=z.rng, cex.min=0.5, cex.max=1.5)
## To verify, label the points with their concentrations.
text(ozone1$longitude, ozone1$latitude+0.15, ozone1$daily.max, cex=0.7)
## If maps package is available, put on state lines.
if (require("maps"))  map("state", add=TRUE, col="lightgray")
## Put on legend.
vertical.image.legend(col=col.rng, zlim=z.rng)


## Plot second day of ozone data.  Note that day 2 experienced a smaller
## range of concentrations, so we plot day 2 on same scale as day 1.
data(ozone1)
data(ozone2)
z.rng <- c(40, 90)
col.rng <- rev(rainbow(n=10, start=0, end=4/6))
X11()
plot3d.points(x=ozone2$longitude, y=ozone2$latitude, z=ozone2$daily.max,
              xlab="longitude", ylab="latitude", col=col.rng,
              zlim=z.rng, cex.min=0.5, cex.max=1.5)
## To verify, label the points with their concentrations.
text(ozone2$longitude, ozone2$latitude+0.15, ozone2$daily.max, cex=0.7)
## If maps package is available, put on state lines.
if (require("maps"))  map("state", add=TRUE, col="lightgray")
vertical.image.legend(col=col.rng, zlim=z.rng)



## When some z value(s) is/are much lower/higher than the others,
## the outlying value(s) may appear in color at the extent
## of the range, with the remainder of the data clustered in one (or
## just a few) color bin(s).
x <- 1:9
y <- 1:9
z <- c(0, 47:53, 100)
X11()
par(mfrow=c(1,3))
col.rng <- rev(rainbow(n=7, start=0, end=4/6))
plot3d.points(x, y, z, col=col.rng)
text(x, y+0.2, z, cex=0.8)
## In vain, you might try to "fix" this by setting zlim so that the
## color range reflects the main portion of the z values.  You may
## assume that the outlying value(s) will show up in the extreme edges
## of the color range, but what will actually happen is that the
## outlying values won't be plotted.
plot3d.points(x, y, z, col=col.rng, zlim=c(47, 53))
text(x, y+0.2, z, cex=0.8)
## Instead, specify zlim to reflect the main porition of the z values,
## and set plt.beyond.zlim=TRUE.  Now, z values below zlim[1] will be
## plotted in the same color as zlim[1]; those above zlim[2] will be
## plotted like z values of zlim[2].  But, remember, now there are
## outlying values whose maginitudes cannot be easily ascertained!
plot3d.points(x, y, z, zlim=c(47, 53), col=col.rng, plt.beyond.zlim=TRUE)
text(x, y+0.2, z, cex=0.8)
}


\keyword{ hplot }
