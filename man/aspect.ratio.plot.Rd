\name{aspect.ratio.plot}
\alias{aspect.ratio.plot}

\title{
  Makes/initializes a plot with proper aspect ratio and of a
  specified width/height.
}


\description{ This code allows the user to make/initialize a figure with
a proper aspect ratio and of a specified width/height.  (Note that this
size includes the actual plot and the margins around it.)  The function
has default specifications for the margins, intended to minimize the
margin space for many commonly generated plots, but these can be
changed.  The user should not use this function if more than one plot is
desired on the device (e.g., if using mfrow, mfcol, layout, etc.)  It
may be used to generate plots in encapsulated PostScript, PDF, Windows
metafile, and TIFF formats.  }


\usage{
aspect.ratio.plot(x, y, file, dev = "pdf", type = "p", xlim, ylim,
   width, height, mai = c(0.4, 0.4, 0.1, 0.1), mgp = c(1.4, 0.3, 0),
   tcl=-0.3, cex=0.8, cex.axis=0.8, cex.lab=0.8, cex.main=0.8,
   cex.sub=0.8,...)
}


\arguments{
  \item{x}{x-coordinates to plot.}
  \item{y}{y-coordinates to plot.}

  If either \code{x} or \code{y} is missing, \code{xlim} and \code{ylim}
  are used to initialize an empty plot.

  \item{file}{Name of file to which to save the plot.}
  \item{dev}{Type of file to create.  Must be one of \dQuote{eps}
    (encapsulated PostScript), \dQuote{pdf} (portable document format),
    \dQuote{wmf} (Windows metafile, available on Windows operating
    systems only), or \dQuote{tif} (tagged image format).}

  \item{type}{Default is \dQuote{p} for points if \code{x} and \code{y} are
    given.  If they are not, type is set to \dQuote{n}, so that the plot region
    is only initialized.  Information about other choices can be found
    in the help for the \code{\link{plot}} function.}

  \item{xlim}{Extent of the x-axis.}
  \item{ylim}{Extent of the y-axis.}

  If either \code{xlim} or \code{ylim} is missing, it is set based on
  the range of values in \code{x} and \code{y}.

  \item{width}{Width of the figure in inches.}
  \item{height}{Height of the figure in inches.}

  For more information about what happens if either of these is missing,
  see 'Details' section below. 

  \item{mai}{Settings for margins in inches, as defined for parameter
    \code{mai} in the help for the par() function.}
  \item{mgp}{Settings determining how close axis titles, labels, and
    lines are to the plot, as defined for parameter \code{mgp} in the
    help for the \code{\link{par}} function.}
  \item{tcl}{Tick mark length, as explained in the help for the
    \code{\link{par}} function.  By default, the value is -0.3, to
    better match the default size of plots produced by this function.
    }
  \item{cex, cex.axis, cex.lab, cex.main, cex.sub}{Amount of
    magnification for plotting text, symbols, axis annotation, titles,
    etc., as explained in the help for \code{\link{par}} function.
    Default for each of these is 0.8, to better match the default size
    of plots produced by this function.}
  
  \item{\dots}{Any other parameters the user adds will be passed to
    either the \code{\link{init.fig.dimen}} function (and thence to
    \code{\link{par}}) or to the \code{\link{plot}} function.  Please
    note that some such arguments could legitimately be passed to either
    of these two functions, but we assumes that any of these which are
    valid arguments to \code{\link{par}} are intended to be passed to
    \code{\link{init.fig.dimen}}, which initializes the figure on the
    given device.  All other arguments will be assumed to be valid
    arguments for \code{\link{plot}} and will be passed as such.}

}


\details{
  Usually only one of \code{height} and \code{width} will be specified.
  In this case, a figure will be initialized with the given dimension,
  and with the other dimension determined according to the relative
  limits of \code{y} and \code{x} (to preserve the aspect ratio).  If
  both are missing, a figure will be generated of width 3.5in, with the
  height determined to preserve the aspect ratio.  If both are present,
  the plot with be made with the specified dimensions, disregarding the
  aspect ratio and generating a warning.

  By default, if the user specifies \code{x} and \code{y}, a scatterplot
  with an aspect ratio based on the relative range of \code{x} and
  \code{y} will be generated.  If the user wishes to make another kind
  of plot (e.g., an image plot), s/he can specify
  \sQuote{type=\dQuote{n}}.  If \code{x} and \code{y} are missing, then
  a plot will be set up based on 'xlim' and 'ylim' and with
  \sQuote{type=\dQuote{n}}.

  Device \dQuote{wmf} (Windows metafile) is only available when running R on
  the Windows operating system.  An error message will be returned if
  the user chooses this device on a non-Windows platform.
}


\value{By default, a scatterplot is written to the device 'dev' with
  margins and overall figure dimensions as specified.  If type=\dQuote{n},
  sets up a figure and plotting region with the specified dimensions
  and margins on the device 'dev'.}


\author{Jenise Swall}


\seealso{\code{\link{par}}, \code{\link{plot}}, \code{\link{image}}, \code{\link{Devices}}}


\examples{
## Load dataset containing longitude and latitude of some ozone
## concentration measurements.
data(ozone1)
## Make plot showing monitoring locations.
aspect.ratio.plot(ozone1$longitude, ozone1$latitude,
                  file="aspect_ratio_tst.pdf", xlab="longitude",
                  ylab="latitude")
## If maps package is available, put on state lines.
if (require("maps"))  map("state", add=TRUE, col="lightgray")
dev.off()
}

\keyword{ hplot }
\keyword{ device }
