\name{ragged.image}
\alias{ragged.image}

\title{Produces a "ragged" image plot}

\description{This code produces an image plot in the case in which there
  is not a known response value \code{z} for every possible combination
  of \code{x} and \code{y}.  This ragged image plot is a variant of an
  image plot which is not complete across the entire rectangle of the
  gridded area.}

\usage{
ragged.image(x, y, z, zlim = range(z, na.rm = TRUE), add = FALSE,
   col = heat.colors(12), xlab, ylab, plt.beyond.zlim = FALSE, ...)
}


\arguments{
  \item{x}{x-coordinates of grid cell centers at which response values
    \code{z} are available.}
  \item{y}{y-coordinates of grid cell centers at which response values
    \code{z} are available.}
  \item{z}{Response values recorded at the grid cell centers whose
    coordinates are given by (\code{x}, \code{y}).}
  
  \item{zlim}{Vector of minimum and maximum values of \code{z} to which to
    assign the two most extreme colors in the 'col' argument (col[1] and
    col[length(col)]).  Default is to use the range of \code{z}.  This is very
    much like the 'zlim' argument to the \code{\link{image}} function.}

  \item{add}{If FALSE (default), the ragged image will begin a new plot.
    If TRUE, adds ragged image to a pre-existing plot. }
  \item{col}{Color range to use for the ragged image, with the first
    color assigned to zlim[1] and last color assigned to zlim[2].
    Default is "heat.colors(12)", as it is for \code{\link{image}}.}

  \item{xlab}{The label for the x-axis.  If not specified by the user,
    defaults to the expression the user named as parameter \code{x}.
    This behavior is similar to that for \code{\link{image}}.}
  \item{ylab}{The label for the y-axis.  If not specified by the user,
    defaults to the expression the user named as parameter \code{y}.
    This behavior is similar to that for \code{\link{image}}.}
  
  \item{plt.beyond.zlim}{IF TRUE, and if \code{zlim} is specified by the
  user, z values beyond the limits given in \code{zlim} are
  plotted. Values less than zlim[1] are plotted in the same color as
  zlim[1]; values greater than zlim[2] are plotted in the same color as
  zlim[2].  If TRUE, and \code{zlim} is not specified by the user,
  zlim[1] and zlim[2] will be assigned the minimum and maximum values of
  \code{z}.  In this case, user is warned and plt.beyond.zlim is set to
  FALSE.  Default is plt.beyond.zlim==FALSE.}

  \item{\dots}{Any additional parameters to be passed to the
    \code{\link{image}} function, if add=FALSE.}
}


\details{This code produces a ragged image plot.  This is in contrast to
the standard \code{\link{image}} function, which assumes that there is a
known response value \code{z} for every combination of the elements of
\code{x} and \code{y}, i.e. that there is a complete rectangular grid,
or image.  A ragged image plot is a variant of the regular image plot
which is not complete across the entire rectangle.  The user specifies
vectors \code{x}, \code{y}, and \code{z}, such that \code{x} and
\code{y} identify a portion of the grid. This function maps the vector
\code{z} onto a matrix of the type needed for the \code{\link{image}}
function, but has NA entries for combinations of \code{x} and \code{y}
that are not listed.  The NA values are not plotted by
\code{\link{image}}, so a ragged image will appear.}


\value{A ragged image, i.e. a portion of an image for which we have
  specified grid cell centers \code{x} and \code{y}.}

\author{Jenise Swall}

\note{This function is slow if \code{x}, \code{y}, and \code{z} are long
  vectors.}


\seealso{\code{\link{image}}, \code{\link{heat.colors}}}

\examples{
# Build x, y, and z.
x <- c(1, 2, 3, 1, 2, 3)
y <- c(1, 1, 1, 2, 2, 2)
z <- 1:6
z.mat <- matrix(c(1:6), ncol=2)
col.rng <- terrain.colors(6)
# Show complete matrix.
image(x=unique(x), y=unique(y), z.mat, zlim=range(z), col=col.rng,
      xlab="x", ylab="y")
# Plot only part of this as a ragged image.  Set z range so that this
# image will use colors consistent with the previous one.
ragged.image(x=x[1:4], y=y[1:4], z=z[1:4], zlim=range(z), col=col.rng,
             xlab="x", ylab="y")



# When some z value(s) is/are much lower/higher than the others,
# the outlying value(s) may appear in color at the extent
# of the range, with the remainder of the data clustered in one (or
# just a few) color bin(s).
x <- c(1, 2, 3, 1, 3, 2, 3, 1, 3)
y <- c(4, 4, 4, 3, 3, 2, 2, 1, 1)
z <- c(0, 47:53, 100)
col.rng <- rev(rainbow(n=7, start=0, end=4/6))
ragged.image(x, y, z, col=col.rng)
text(x, y, z, cex=0.8)

# In vain, you might try to "fix" this by setting zlim so that the
# color range reflects the main portion of the z values.  You may
# assume that the outlying value(s) will show up in the extreme edges
# of the color range, but what will actually happen is that the
# outlying values won't be plotted.
ragged.image(x, y, z, col=col.rng, zlim=c(47, 53))
text(x, y, z, cex=0.8)

# Instead, specify zlim to reflect the main porition of the z values,
# and set plt.beyond.zlim=TRUE.  Now, z values below zlim[1] will be
# plotted in the same color as zlim[1]; those above zlim[2] will be
# plotted like z values of zlim[2].  But, remember, now there are
# outlying values whose maginitudes cannot be easily ascertained!
ragged.image(x, y, z, zlim=c(47, 53), col=col.rng, plt.beyond.zlim=TRUE)
text(x, y, z, cex=0.8)
}


\keyword{ hplot }
