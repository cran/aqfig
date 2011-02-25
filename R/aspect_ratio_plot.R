## ###########################################################
## Purpose: This code allows the user to make/initialize a plot with
##    a proper aspect ratio and of a specified width/height.  (Note 
##    that this size includes the actual plot and the margins around
##    it.)  The function has default specifications for the margins,
##    intended to minimize the margin space for many commonly
##    generated plots, but these can be changed. of a specified size.
##    The user should not use this function if more than one plot is
##    desired on the device (e.g., if using mfrow, mfcol, layout,
##    etc.)  It may be used to generate plots in encapsulated
##    PostScript, PDF, Windows metafile, and TIFF formats.
##
##
## Input:
##    x, y: x-coordinates and y-coordinates to plot.
##       If x or y is missing, xlim and ylim are used to initialize
##       an empty plot.
##    file: File name to which to save plot.
##    dev: "pdf" for PDF (default)
##         "eps" for encapsulated PostScript
##         "wmf" for Windows metafile format (only available on Windows)
##         "tif" for TIFF (Note: Image will have resolution of 300 dpi.)
##    type: Default is "p" for points if x and y are given.  If they
##       are not, type is set to "n", and the plot is only initialized.
##       Information about other choices can be found in the help for
##       the plot() function.
##    xlim, ylim: Extent of the x and y axes.
##       If xlim or ylim is missing, these are set based on the range of
##       values in x and y.
##    width, height: Width and height of the figure in inches.
##       Usually only one of these will be specified, and a figure will
##       be initialized with the given dimension, with the other
##       dimension determined according to the relative limits of y and
##       x (to preserve the aspect ratio).  If both are missing, a
##       figure will be generated of width 3.5in, with the height
##       determined to preserve the aspect ratio.  If both are present,
##       the plot with be made with the specified dimensions,
##       disregarding the aspect ratio and generating a warning.  
##    mai: Settings for margins in inches, as defined for parameter
##       "mai" in the help for the par() function.
##    mgp: Settings determining how close axis titles, labels, and
##       lines are to the plot, as defined for parameter 'mgp' in
##       the help for the par() function.
##    ...: Any other parameters the user adds will be passed to the
##       plot() function, and may include options for color and
##       type ("l" for lines, etc.), among many others.
##
##
## Returns: Opens a new plotting device as specified by "dev" parameter
##    and either generates a plot or initializes a plot (using type="n"),
##    depending on values of 'x', 'y', and 'type'.
##
##
## Assumes:
##    1. The user wants to save the plot in encapsulated PostScript,
##       PDF, Windows metafile, or TIFF format.
##    2. Only one plot will be generated.  (The user is not using
##       layout, mfrow, mfcol, etc. on this device.)
##
## 
## 
## REVISION HISTORY:
##   Prototype: Jenise Swall, 2010-04-09 (based closely on Jenise's
##      function initialize.aspect.plot(), now deprecated).
##
##   2010-04-12 (JLS): Type added as option, problem with xlim, ylim fixed.
##   2010-04-15 (JLS): Option mgp added.
##   2010-06-16 (JLS): Option to make TIFF files with resolution of 300
##      dpi added.  Originally intended to allow user to pass in dpi as 
##      an argument, but encountered as yet unexplained problems with the
##      format.  (For example, an image that could be produced at 300 and
##      600 dpi encountered an error when set at 450 dpi.)
##   2011-02-28 (JLS): Altered code to use a call to function
##      init.fig.dimen() to open the device and make the call to par().
##      Changed function arguments 'fig.height' and 'fig.width' to
##      'height' and 'width', respectively.  Adjusted/clarified comments.
## ###########################################################
aspect.ratio.plot <- function(x, y, file, dev="pdf", type="p",
                              xlim, ylim, width, height,
                              mai=c(0.6, 0.6, 0.1, 0.1),
                              mgp=c(1.8, 0.5, 0), ...){

  ## If xlim and x are missing, we have no info about the x-axis.
  if ( missing(xlim) && missing(x) )
    stop("Need information about x-axis; xlim and x are missing.")
  ## If ylim and y are missing, we have no info about the y-axis.
  if ( missing(ylim) && missing(y) )
    stop("Need information about y-axis; ylim and y are missing.")
  

  ## If xlim and/or ylim are missing, set them based on the range
  ## of x and y.
  if (missing(xlim))
    xlim <- range(x, na.rm=TRUE)
  if (missing(ylim))
    ylim <- range(y, na.rm=TRUE)


  ## If x or y is missing, we assume that we're just supposed to
  ## initialize the figure, and we set type="n".
  if ( missing(x) || missing(y) ) {
    if (exists("type")){
      if (type != "n")
        warning('Setting type="n", since x or y is missing.')
    }
    type <- "n"
  }



  ## Unless the user actually passes in the width and height, we
  ## should set it based on the idea of preserving the aspect ratio.
  if ( missing(height) || missing(width) ){

    ## If both height and width are missing, then set width to 4in.
    if( missing(height) && missing(width) )
      width <- 4.0


    ## What are the relative proportions of the x and y ranges?
    prop.y.to.x <- (ylim[2]-ylim[1]) / (xlim[2]-xlim[1])


    ## If height is missing, then determine it based on the width,
    ## keeping the correct aspect ratio.
    if (missing(height)){

      ## Calculate the total horizontal plotting space (inches) left
      ## over after we have substracted the space needed for the
      ## horizontal margins from width.
      plot.width <- width - mai[2] - mai[4]
      ## The plot's height is based on this width and the relative
      ## length of the y-axis to the x-axis.
      plot.height <- plot.width * prop.y.to.x

      ## Based on the calculated plot height and the margins, calculate
      ## the total height of the figure.
      height <- plot.height + mai[1] + mai[3]
    }


    ## If width is missing, then determine it based on the height,
    ## keeping the correct aspect ratio.
    else if (missing(width)){

      ## Calculate the total vertical plotting space (inches) left
      ## over after we have substracted the space needed for the
      ## vertical margins from width.
      plot.height <- height - mai[1] - mai[3]
      ## The plot's width is based on this height and the relative
      ## length of the x-axis to the y-axis.
      plot.width <- plot.height / prop.y.to.x

      ## Based on the calculated plot height and the margins, calculate
      ## the total height of the figure.
      width <- plot.width + mai[2] + mai[4]
    } 
  }

  ## Else if both figure height and width are given, then use these
  ## values and give a warning that the aspect ratio may not be right.
  else
    warning("Using provided 'height' and 'width' without checking aspect ratio.")


  ## Initialize figure and margins on specified device.
  init.fig.dimen(file=file, dev=dev, width=width, height=height,
                 mai=mai, mgp=mgp)


  ## Initialize/make plot.
  if (type == "n")
    plot(xlim, ylim, type=type, xlim=xlim, ylim=ylim, ...)
  else
    plot(x, y, type=type, xlim=xlim, ylim=ylim, ...)
}
