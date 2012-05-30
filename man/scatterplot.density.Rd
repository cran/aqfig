\name{scatterplot.density}
\alias{scatterplot.density}

\title{Use color to show the density of points in a scatterplot}

\description{The plotting region of the scatterplot is divided into
  bins. The number of data points falling within each bin is summed and
  then plotted using the image function.  This is particularly useful when
  there are so many points that each point cannot be distinctly
  identified.
}

\usage{
scatterplot.density(x, y, zlim, xylim, num.bins=64,
   col=kristen.colors(32), xlab, ylab, main, density.in.percent=TRUE,
   col.regression.line=1, col.one.to.one.line=grey(0.4),
   col.bar.legend=TRUE, plt.beyond.zlim=FALSE, ...)
}


\arguments{
  \item{x}{Vector or matrix of x-coordinates of points to be plotted.
    Missing values are not permitted.
  }
  \item{y}{Vector or matrix of y-coordinates of points to be plotted.
    Missing values are not permitted.
  }
  
  \item{zlim}{Vector defining the minimum and maximum of the data
    density values, to which to assign the two most extreme colors in the
    \code{col} argument.  If not specified, the range of the calculated
    density values to be plotted is used.
  }

  \item{xylim}{Specification of extreme values that the first and last
    bins are expected to contain in the x- and y-directions.  May be a
    single vector of the limits for the x and y axes; e.g., using
    \samp{xylim=c(0,120)} specifies that, in both the x- and
    y-directions, the first bin should contain 0 and the last contain
    120.  May also be a list in the form: \samp{xylim=list(xlim=c(x1
    ,x2), ylim=c(y1, y2))}, allowing for the different ranges on the
    axes.  If not specified, xlim is the range of \code{x} and ylim is
    the range of \code{y}.
    
    Note that \code{xylim} and \code{num.bins} together determine how
    the bins are defined.  For more information, see \dQuote{Details}
    below.
  }

  \item{num.bins}{Number of bins to be used when calculating the data
    density in both the x- and y-directions.  May be a single number,
    e.g. \samp{num.bins=50}, which produces 50 bins in each direction.
    May also be a list in the form
    \samp{num.bins=list(num.bins.x=n1, num.bins.y=n2)} to specify
    differing numbering of bins for the x- and y-directions.  The
    default is to use 64 bins for both axes (\samp{num.bins=64}).

    Note that \code{xylim} and \code{num.bins} together determine how
    the bins are defined.  For more information, see \dQuote{Details}
    below.
  }

  \item{col}{Color range to use when drawing bins, with the first color
    assigned to \samp{zlim[1]} and last color assigned to
    \samp{zlim[2]}.  Default is \samp{kristen.colors(32)}.
  }

  \item{xlab}{The label for the x-axis.  If not specified by the user,
    defaults to the expression the user named as parameter \code{x}.
    This behavior is similar to that for \code{\link{image}}.
  }
  \item{ylab}{The label for the y-axis.  If not specified by the user,
    defaults to the expression the user named as parameter \code{y}.
    This behavior is similar to that for \code{\link{image}}.
  }
  \item{main}{The main title for the density scatterplot.  If not
    specified, the default is \dQuote{Data Density Plot (\%)} when
    \samp{density.in.percent=TRUE}, and \dQuote{Data Frequency Plot
      (counts)} otherwise.
    }

  \item{density.in.percent}{A logical indicating whether the density
    values should represent a percentage of the total number of data
    points, rather than a count value.  Default is
    \samp{density.in.percent=TRUE}.
  }

  \item{col.regression.line}{A color number or color name for the
    regression line and estimated regression equation (\code{y} as a
    linear function of \code{x}) to be overlaid on density scatterplot.
    If NULL, the regression line and equation are not displayed.
    Defaults to a black line and equation text.
  }
  \item{col.one.to.one.line}{A color number or color name for the
    regression one-to-one line to be overlaid on density scatterplot.  If
    NULL, the one-to-one line is not displayed.  Defaults to a dark grey
    line.  If the one-to-one line is displayed, it will be as a dashed
    line (\samp{lty=3}).
  }

  \item{col.bar.legend}{A logical indicating whether a
    \dQuote{color legend} of the form given by
    \code{\link{vertical.image.legend}} should be displayed.  The
    default is \samp{col.bar.legend=TRUE}.
  }
  
  \item{plt.beyond.zlim}{IF TRUE, and if \code{zlim} is specified by
    the user, density values beyond the limits given in \code{zlim}
    are plotted. Values less than \samp{zlim[1]} are plotted in the same
    color as \samp{zlim[1]}; values greater than \samp{zlim[2]} are
    plotted in the same color as \samp{zlim[2]}.  If TRUE, and
    \code{zlim} is not specified by the user, \samp{zlim[1]} and
    \samp{zlim[2]} will be assigned the minimum and maximum values of
    \code{z}.  In this case, user is warned and \code{plt.beyond.zlim}
    is set to FALSE.  Default is \samp{plt.beyond.zlim=FALSE}.
  }

  \item{\dots}{Any additional parameters to be passed to the
    \code{\link{image}} function.
  }
}



\details{The plotting region of the scatterplot is divided into bins.
  The number of data points falling within each bin is summed and then
  plotted using the \code{\link{image}} function. The default is to
  plot the percent of the data falling within each bin, rather than a
  raw count value. The arguments xylim and num.bins can include
  different settings for the x- and y-axis.  This makes it easier to
  plot different variables on each axis, e.g. temperature
  vs. ozone. Note that \code{xylim} and \code{num.bins} together
  determine how the bins are defined.

  Note that \code{xylim} and \code{num.bins} together determine how the
  bins are defined.  This is done using the \code{\link{cut}} function.
  Assigning values to bins is more complicated than might be expected.
  For example, values that fall at cutoff points between bins are
  difficult to deal with.  This function accepts the default setting for
  \code{\link{cut}}, which assigns values which fall on a cutoff point
  to the bin on the left; that is, the intervals are open on the left
  and closed on the right.  This means that a point with x-value equal
  to \samp{xlim[1]} and/or y-value equal to \samp{ylim[1]} would not be
  assigned to any interval, which is probably not what the user intends
  in this circumstance.  Therefore, this code determines the number of
  bins in the x-direction so that \samp{xlim[1]} and \samp{xlim[2]} are
  at the center of the first and last bin in the x-direction (and
  similarly for the y-direction).  This means that the first and last
  bins actually extend a bit past the limits specified.  For most
  applicatons, which use large numbers of data points and bins, this
  shouldn't be noticeable, but it may be in smalled examples like the
  first one given below.}


\value{A density scatterplot; that is, a pattern of shaded squares
  representing the counts/percentages of the points falling in each
  square.}

\author{Original version (\code{plot.density.scatter.plot}) by Kristen
  Foley, adapted for \pkg{aqfig} by Jenise Swall}


\seealso{\code{\link{vertical.image.legend}},
  \code{\link{kristen.colors}}, \code{\link{image}}, \code{\link{cut}}
}


\examples{
## As a simple test case, build x and y vectors consisting only of the
## integers 1-3.
x <- c( rep(1, 7), rep(2, 12), rep(3, 6) )
y <- c( rep(1, 5), rep(2, 2), rep(1, 2), rep(2, 8), rep(3, 2),
        rep(2, 2), rep(3, 4) )

## For this test case, I've totaled the counts below.
count.df <- data.frame(x=rep(1:3, each=3), y=rep(1:3, times=3), ct=c(5,
2, 0, 2, 8, 2, 0, 2, 4) )

## Make a density scatterplot with counts and percentages.
par(mfrow=c(1,2))
scatterplot.density(x, y, num.bins=3, col=heat.colors(7),
                    density.in.percent=FALSE,
                    col.one.to.one.line="green")
text(count.df$x, count.df$y, count.df$ct, col="purple")
scatterplot.density(x, y, num.bins=3, col=heat.colors(7), col.one.to.one.line=1)
text(count.df$x, count.df$y, count.df$ct/sum(count.df$ct))


## An example closer to actual usage.
x <- rnorm(100000,50,5)
y <- 3 + (.89*x) + rnorm(100000,0,5)
par(mfrow=c(1,1))
scatterplot.density(x, y)
}



\keyword{ hplot }
