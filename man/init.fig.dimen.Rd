\name{init.fig.dimen}
\alias{init.fig.dimen}

\title{Initializes a device for a figure with specified dimensions.}

\description{This function allows the user to initialize a device for
  a figure of a specified size, where this size includes the actual
  plot and the margins around it.  It may be used to generate plots
  in encapsulated PostScript, PDF, Windows metafile, and TIFF formats.}


\usage{
init.fig.dimen(file, dev = "pdf", width, height,
   mai = c(0.6, 0.6, 0.1, 0.1), mgp = c(1.8, 0.5, 0), ...)
}


\arguments{
  \item{file}{File name to which to save figure.}
  \item{dev}{Type of file to create.  Must be one of \dQuote{eps}
    (encapsulated PostScript), \dQuote{pdf} (portable document format),
    \dQuote{wmf} (Windows metafile, available on Windows operating
    systems only), or \dQuote{tif} (tagged image format).}

  \item{width}{Width of the figure in inches.}
  \item{height}{Height of the figure in inches.}
  
  \item{mai}{Settings for margins in inches, as defined for parameter
 \code{mai} in the help for the \code{\link{par}} function.}
  \item{mgp}{Settings determining how close axis titles, labels, and
    lines are to the plot, as defined for parameter \code{mgp} in the help
    for the \code{\link{par}} function.}

  \item{\dots}{Any other parameters the user adds will be passed to the
    \code{\link{par}} function, and may include options to control axes,
     symbol sizes, etc.}
}


\details{The function has default specifications for the margins,
  intended to minimize the margin space for many commonly generated
  plots, but these can be changed to use other \code{mai} options or
  other arguments to \code{\link{par}}.  The user should not use this
  function if more than one plot is desired on the device (e.g. if
  using mfrow, mfcol, layout, etc.)

  This function is used to initialize figures inside the function
  \code{\link{aspect.ratio.plot}}.
}


\value{Sets up a figure and plotting region with the specified
  dimensions and margins on the device \code{dev}.}

\note{This function is called by the functin aspect.ratio.plot (also
  included in this package).}

\author{Jenise Swall}


\seealso{\code{\link{par}}}



\keyword{ hplot }
\keyword{ device }
