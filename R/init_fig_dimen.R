## ###########################################################
## PURPOSE: This function allows the user to initialize a device for a
##    figure of a specified size, where this size includes the actual
##    plot and the margins around it.  The function has default
##    specifications for the margins, intended to minimize the margin
##    space for many commonly generated plots, but these can be
##    changed. of a specified size.  The user should not use this
##    function if more than one plot is desired on the device (e.g. if
##    using mfrow, mfcol, layout, etc.)  It may be used to generate
##    plots in encapsulated PostScript, PDF, Windows metafile, and 
##    TIFF formats.
##
## 
## INPUT:
##   file: File name to which to save figure.
##   dev: "pdf" for PDF (default)
##        "eps" for encapsulated PostScript
##        "wmf" for Windows metafile format (only available on Windows)
##        "tif" for TIFF (Note: Image will have resolution of 300 dpi.)
##   width, height: Width and height of the figure in inches, including
##      the space to be taken by the actual plot and the margins
##      around it.
##   mai: Settings for margins in inches, as defined for parameter
##        "mai" in the help for the par() function.
##   mgp: Settings determining how close axis titles, labels, and
##        lines are to the plot, as defined for parameter "mgp" in
##        the help for the par() function.
##   ...: Any other parameters the user adds will be passed to the
##        par() function, and may include options to control axes,
##        symbol sizes, etc.
##
##
## RETURNS: Opens a new plotting device as specified by "dev"
##    parameter and either generates a plot or initializes a
##    plot, depending on whether x and y are given.
##
##
## DETAILS: The default margin specifications may help the user
##    minimize the extra white space in the margins, assuming that
##    the user:
##    1. does not want to place a main or sub title on the plot.
##    2. does want to place tick marks and axis labels on the plot,
##       but wants these to be a bit more compact than they are by
##       default.
##    3. does not want to place text, legends, etc. to the right
##       of the plot.
##
##
##
## ASSUMES:
##    1. The user wants to save the plot in encapsulated PostScript,
##       PDF, Windows metafile, or TIFF format.
##    2. Only one plot will be generated.  (The user is not using
##       layout, mfrow, mfcol, etc. on this device.)
##
## 
## 
## REVISION HISTORY:
##   Prototype: Jenise Swall, 2011-02-28  (Based on code taken from
##      an older version of Jenise's aspect.plot.ratio function.)
## ###########################################################
init.fig.dimen <- function(file, dev="pdf", width, height,
                              mai=c(0.6, 0.6, 0.1, 0.1),
                              mgp=c(1.8, 0.5, 0), ...){


  ## Check that the user has passed in the figure width and figure
  ## height.
  if ( missing(height) || missing(width) ){
      
    ## If both height and width are missing, exit and warn user.
    if( missing(height) && missing(width) )
      stop("Missing arguments 'height' and 'width'.")

    ## If only height is missing, exit and warn user.
    if( missing(height) )
      stop("Missing argument  'height'.")

    ## If only width is missing, exit and warn user.
    if( missing(width) )
      stop("Missing argument 'width'.")
  }


  ## Initialize postscript device (default), PDF, Windows metafile, or
  ## TIFF for a figure of this size.  If 'dev' does not match one of
  ## these, exit and warn the user.
  if (dev=="eps")
    postscript(file=file, horizontal=FALSE, width=width, height=height,
               onefile=FALSE, paper="special")
  else if (dev=="pdf")
    pdf(file=file, width=width, height=height, onefile=FALSE,
        paper="special")    
  else if (dev=="wmf"){
    if (.Platform$OS.type != "windows")
      stop("Cannot use argument ", dQuote(dev), " on a non-Windows platform.")
    win.metafile(file=file, width=width, height=height)
  }
  else if (dev=="tif")
    tiff(filename=file, width=width, height=height, units="in",
         res=300, pointsize=10, compression="none")
  else
    stop(paste(dQuote(dev), " is not a valid option for argument 'dev'.", sep=""))


  ## Initialize figure region and margins.  We're working from the
  ## outside to the inside here, so once we specify the size of the
  ## total figure, and then the margins, everything that's left is
  ## taken by the actual plotting region.
  par(fin=c(width=width, height=height), mai=mai, mgp=mgp, ...)
}
