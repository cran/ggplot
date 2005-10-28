# Grob function: histogram
# Draw a histogram
# 
# Aesthetic mappings that this grob function understands:
#
# Conceptually, the histogram is one of the most complicated
# of the grob functions, becuase it takes a 1D data set and makes
# it two dimensional.  This necessitates an extra step, the \code{pre_histogram}
# function which bins the data and returns the bins with their counts.  
# This data is then used my \code{grob_histogram} 
# to plot the points.
# 
# \itemize{
#   \item x: x position (required)
# }
# 
# These can be specified in the plot defaults (see \code{\link{ggplot}}) or
# in the \code{aesthetics} argument.  If you want to modify the position 
# of the points or any axis options, you will need to add a position scale to
# the plot.  These functions start with \code{ps}, eg. 
# \code{\link{pscontinuous}} or \code{\link{pscategorical}}
# 
# Other options:
# 
# \itemize{
#   \item breaks: breaks argument passed to \code{\link{hist}}
#   \item scale: scale argument passed to \code{\link{hist}}
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
#X m <- ggplot(movies, aesthetics=list(x=rating))
#X gghistogram(m)
gghistogram <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	plot <- pscontinuous(plot, "y", range=c(0,NA), expand=c(0.05,0))
	plot <- pscontinuous(plot, "x", expand=c(0.15, 0))
	plot$ylabel <- "Density"

	gg_add("histogram", plot, aesthetics, ..., data=data)
}
pre_histogram <- function(data, breaks="Sturges", scale="prob", ...) {
	h <- hist(data$x, breaks=breaks, freq=FALSE, plot=FALSE, ...)

	if (is.character(scale) && scale == "prob") {
		y <- h$density
	} else if (is.numeric(scale) && length(scale == 2)){
		y <- rescale(h$density, scale)
	} else {
		y <- h$counts
	}
	
	data.frame(y = y, x=h$breaks[-1], width = diff(h$breaks), height=y)
}
grob_histogram <- function(aesthetics, ...) {
	grob_rect(aesthetics, justification=c("right", "top"), colour="grey")
}

# Grob function: quantiles
# Add quantile lines from a quantile regression
# 
# This can be used a continuous analogue of a boxplot (see \code{\link{grob_boxplot}})
# Lines will be automatically sized to reflect their distance from the median.
#
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
# }
# 
# These can be specified in the plot defaults (see \code{\link{ggplot}}) or
# in the \code{aesthetics} argument.  If you want to modify the position 
# of the points or any axis options, you will need to add a position scale to
# the plot.  These functions start with \code{ps}, eg. 
# \code{\link{pscontinuous}} or \code{\link{pscategorical}}
# 
# Other options:
# 
# \itemize{
#   \item quantiles: quantiles to display
#   \item formula: formula to use in quantile regression
#   \item colour: colour of lines
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
# @seealso \code{\link[quantreg]{rq}} for the code used to fit the quantile regression
#X m <- ggplot(movies, aesthetics=list(y=length, x=rating))
#X ggquantile(gghexagon(m))
ggquantile <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("quantile", plot, aesthetics, ..., data=data)
}
grob_quantile <- function(aesthetics, quantiles=c(0.05, 0.25, 0.5, 0.75, 0.95), formula=y ~ splines::ns(x, 5), colour="blue", ...) {
	if (!require(quantreg, quietly=TRUE)) stop("You need to install the quantreg package!")
	
	xseq <- seq(min(aesthetics$x, na.rm=TRUE), max(aesthetics$x, na.rm=TRUE), length=30)
	
	model <- rq(formula, data=aesthetics, tau=quantiles) #
	yhats <- predict(model, data.frame(x=xseq))
	qs <- data.frame(y = as.vector(yhats), x = xseq, id = rep(quantiles, each=length(xseq)))
	qs$size <- (0.5 - abs(0.5 - qs$id))*5 + 0.5
	qs$colour <- colour
	
	grob_path(qs)
}

# Grob function: boxplot
# Add box and whiskers
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
# }
# 
# These can be specified in the plot defaults (see \code{\link{ggplot}}) or
# in the \code{aesthetics} argument.  If you want to modify the position 
# of the points or any axis options, you will need to add a position scale to
# the plot.  These functions start with \code{ps}, eg. 
# \code{\link{pscontinuous}} or \code{\link{pscategorical}}
# 
# Other options:
# 
# \itemize{
#   \item breaks: how to break up the x axis
#   \item other arguments passed \code{\link{boxplot}}
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
# @seealso \code{\link{ggquantile}} for a continuous analogue of the boxplot
#X p <- ggplot(mtcars, aesthetics=list(y=mpg, x=cyl))
#X ggpoint(p)
#X ggboxplot(p)
#X ggpoint(ggboxplot(p))
ggboxplot <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("boxplot", plot, aesthetics, ..., data=data)
}
grob_boxplot <- function(aesthetics, breaks=length(unique(aesthetics$x)), ...) {
	aesthetics$x <- as.numeric(aesthetics$x)
	breakpoints <- cut(aesthetics$x, breaks, labels=FALSE)
	xrange <- list(
		min = tapply(aesthetics$x, breakpoints, min, na.rm=TRUE),
		max = tapply(aesthetics$x, breakpoints, max, na.rm=TRUE),
		median = tapply(aesthetics$x, breakpoints, median, na.rm=TRUE),
		width = tapply(aesthetics$x, breakpoints, function(x) diff(range(x, na.rm=TRUE))) * 0.5 + 0.5,
	)
	
	boxes <- boxplot(aesthetics$y ~ breakpoints, plot=FALSE, ...)
	# lower whisker, lower hinge, median, upper hinge and upper whisker
	
	outliers <- list(y = boxes$out, x = as.vector(xrange$median[boxes$group]), colour="red")
	uwhiskers <- list(y = c(boxes$stats[1,], boxes$stats[2, ]), x=rep(xrange$median, 2), id=rep(xrange$median, 2))
	lwhiskers <- list(y = c(boxes$stats[4,], boxes$stats[5, ]), x=rep(xrange$median, 2), id=rep(xrange$median, 2))
	hinges <- list(x = xrange$median, width=xrange$width, y=boxes$stats[2,], height=boxes$stats[4,] - boxes$stats[2,], fill="white", colour="grey50")
	medians <- list(x= xrange$median, width=xrange$width, y=boxes$stats[3,], height=unit(0.8,"mm"), colour="grey50", fill="grey50")
	
	gTree(children = gList(
		grob_path(uwhiskers),
		grob_path(lwhiskers),
		grob_rect(hinges, justification=c("centre", "bottom")),
		grob_rect(medians, justification=c("centre", "bottom")),
		grob_point(outliers)
	))
}

# Grob function: smooth
# Add a smooth line to a plot
# 
# This grob adds a smoother to the graphic to aid the eye in
# seeing important patterns, especially when there is a lot of overplotting.
# 
# You can customise this very freely, firstly by choosing the function used
# to fit the smoother (eg. \code{\link{loess}}, \code{\link{lm}}, \code{\link{rlm}}, 
# \code{\link{gam}}, \code{\link{glm}}) and the formula used to related the y and x 
# values (eg. \code{y ~ x}, \code{y ~ poly(x,3)}).
# 
# This smoother is automatically restricted to the range of the data.  If you
# want to perform predictions (or fit more complicated variabels with covariates)
# then you should fit the model and plot the predicted results.
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
#   \item size: size of the point, in mm (see \code{\link{scsize})}
#   \item colour: point colour (see \code{\link{sccolour})}
# }
# 
# These can be specified in the plot defaults (see \code{\link{ggplot}}) or
# in the \code{aesthetics} argument.  If you want to modify the position 
# of the points or any axis options, you will need to add a position scale to
# the plot.  These functions start with \code{ps}, eg. 
# \code{\link{pscontinuous}} or \code{\link{pscategorical}}
# 
# Other options:
# 
# \itemize{
#   \item method: smoothing method (function) to use
#   \item formula: formula to use in smoothing function
#   \item se: display one standard error on either side of fit? (true by default)
#   \item other arguments are passed to smoothing function
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot 
#X p <- ggpoint(ggplot(mtcars, aesthetics=list(y=wt, x=qsec)))
#X ggsmooth(p)
#X ggsmooth(p, span=0.9)
#X ggsmooth(p, method=lm)
#X ggsmooth(p, method=lm, formula = y~splines::ns(x,3))
#X ggsmooth(p, method=MASS::rlm, formula = y~splines::ns(x,3))
ggsmooth <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("smooth", plot, aesthetics, ..., data=data)
}
grob_smooth <- function(aesthetics, method=loess, formula=y~x, se = TRUE, ...) {
	aesthetics <- defaults(aesthetics, list(colour="black", size=1, ))
	xseq <- seq(min(aesthetics$x, na.rm=TRUE), max(aesthetics$x, na.rm=TRUE), length=30)
	method <- match.fun(method)

	colour <- as.character(uniquedefault(aesthetics$colour, "black"))
	size <- uniquedefault(aesthetics$size, 1)

	model <- method(formula, data=aesthetics, ...)
	pred <- predict(model, data.frame(x=xseq), se=se)
	
	if (se) {
		gTree(children=gList(
			grob_path(list(y = as.vector(pred$fit), x = xseq, colour=colour, size=size)),
			grob_path(list(y = as.vector(pred$fit + pred$se), x = xseq, colour="grey80")),
			grob_path(list(y = as.vector(pred$fit - pred$se), x = xseq, colour="grey80"))
		))
	} else {
		grob_path(list(y = as.vector(pred), x = xseq, colour=colour, size=size))
	}
}

# Grob function: contours
# Create a grob to display contours of a 3D data set.
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
#   \item z: z position (required)
# }
# 
# These can be specified in the plot defaults (see \code{\link{ggplot}}) or
# in the \code{aesthetics} argument.  If you want to modify the position 
# of the points or any axis options, you will need to add a position scale to
# the plot.  These functions start with \code{ps}, eg. 
# \code{\link{pscontinuous}} or \code{\link{pscategorical}}
# 
# Other options:
# 
# \itemize{
#   \item nlevels: number of contours to draw
#   \item levels: contour positions
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot 
# @seealso \code{\link{gg2density}}
#X volcano3d <- data.frame(expand.grid(x = 1:nrow(volcano), y=1:ncol(volcano)), z=as.vector(volcano))
#X p <- ggplot(volcano3d, aesthetics=list(x=x,y=y,z=z))
#X ggcontour(p)
#X ggcontour(p, nlevels=3)
#X ggcontour(ggtile(p, list(colour=z)))
ggcontour <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("contour", plot, aesthetics, ..., data=data)
}
grob_contour <- function(aesthetics, nlevels=10, levels) {
	if (missing(levels)) levels <- pretty(range(aesthetics$z,na.rm=TRUE),nlevels)
	gridise <- function(x) {
		unique <- sort(unique(x[!is.na(x)]))
		id <- match(x, unique)
		list(unique=unique, id=id)
	}
	
	gridx <- gridise(aesthetics$x)
	gridy <- gridise(aesthetics$y)
	
	gridz <- matrix(NA, nrow = length(gridx$unique), ncol = length(gridy$unique))
	gridz[(gridy$id - 1) * length(gridx$unique) + gridx$id] <- aesthetics$z
	
	clines <- contourLines(x = gridx$unique, y = gridy$unique, z = gridz, nlevels = nlevels, levels = levels)
	clines <- lapply(clines, function(x) {
	  x$colour <- "blue"
	  x
	})
		
	gTree(children = do.call(gList, lapply(clines, grob_path)))
}

# Grob function: 2d density
# Perform a 2D kernel density estimatation using \code{\link{kde2d}} and
# display the results with contours.
# 
# This is another function useful for dealing with overplotting.
#
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
# }
# 
# These can be specified in the plot defaults (see \code{\link{ggplot}}) or
# in the \code{aesthetics} argument.  If you want to modify the position 
# of the points or any axis options, you will need to add a position scale to
# the plot.  These functions start with \code{ps}, eg. 
# \code{\link{pscontinuous}} or \code{\link{pscategorical}}
# 
# Other options:
# 
# \itemize{
#   \item passed to \code{\link{ggcontour}}, see it for details
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
# @seealso \code{\link{ggcontour}}, \code{\link{gghexagon}} for another way of dealing with over plotting
#X m <- ggpoint(ggplot(movies, aesthetics=list(y=length, x=rating)))
#X dens <- MASS::kde2d(movies$rating, movies$length)
#X densdf <- data.frame(expand.grid(rating = dens$x, length = dens$y), z=as.vector(dens$z))
#X ggcontour(m, list(z=z), data=densdf)
#X gg2density(m)
#X # they don't look the same due to scaling effects on kde2d
gg2density <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("2density", plot, aesthetics, ..., data=data)
}

grob_2density <- function(aesthetics, ...) {
	dens <- MASS::kde2d(aesthetics$x, aesthetics$y)
	densdf <- data.frame(expand.grid(x = dens$x, y = dens$y), z=as.vector(dens$z))
	grob_contour(densdf, ...)
}


# Grob function: groups
# Create multiple of grobs based on id aesthetic.
# 
# This grob function provides a general means of creating 
# multiple grobs based on groups in the data.  This is useful
# if you want to fit a separate smoother for each group in the data.
# 
# You will need an id variable in your aesthetics list with determines
# how the data is broken down.
#
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
#   \item id: 
#   \item any other grobs used by the grob function you choose
# }
# 
# These can be specified in the plot defaults (see \code{\link{ggplot}}) or
# in the \code{aesthetics} argument.  If you want to modify the position 
# of the points or any axis options, you will need to add a position scale to
# the plot.  These functions start with \code{ps}, eg. 
# \code{\link{pscontinuous}} or \code{\link{pscategorical}}
# 
# Other options:
# 
# \itemize{
#   \item grob: grob function to use for subgroups
#   \item anything else used by the grob function you choose
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
#X p <- ggplot(mtcars, aesthetics=list(y=wt, x=qsec, id=cyl, colour=cyl))
#X gggroup(p)
#X gggroup(ggpoint(p), grob=grob_smooth, se=FALSE, span=1)
#X gggroup(ggpoint(p), aes=list(id=cyl, size=cyl), grob=grob_smooth, span=1)
gggroup <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("group", plot, aesthetics, ..., data=data)
}
grob_group <- function(aesthetics, grob = grob_point, ...) {
	if(length(aesthetics$id) != length(aesthetics$x)) stop("You need to set an id variable to use grob_group")
	parts <- by(aesthetics, aesthetics$id, grob, ...)
	
	gTree(children = do.call(gList, parts))	
}


# Grob function: hexagons
# Create hexagon binning of data points as created by Dan Carr.
# 
# This grob is useful for scatterplots with a lot of overplotting.  It bins the
# region into hexagons, counts the number of points in each hexagonal bin and
# then plots them.
#
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
# }
# 
# These can be specified in the plot defaults (see \code{\link{ggplot}}) or
# in the \code{aesthetics} argument.  If you want to modify the position 
# of the points or any axis options, you will need to add a position scale to
# the plot.  These functions start with \code{ps}, eg. 
# \code{\link{pscontinuous}} or \code{\link{pscategorical}}
# 
# Other options:
# 
# \itemize{
#   \item xbins: number of bins to use
#   \item ...: other arguments passed to \code{\link[hexbin]{grid.hexagons}}
# }
# 
# @keyword hplot 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @seealso \code{\link[hexbin]{grid.hexagon}}, \code{\link[hexbin]{grob_2density}} for another way of dealing with overplotting
#X m <- ggplot(movies, aesthetics=list(y=length, x=rating))
#X gghexagon(m)
#X gghexagon(m, xbins=50)
#X gghexagon(m, style="lattice")
gghexagon <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("hexagon", plot, aesthetics, ..., data=data)
}
grob_hexagon <- function(aesthetics, xbins=30, ...) {
	if (!require("hexbin", quietly=TRUE)) stop("You need to install the hexbin package !")
	
	hexes <- hexbin(aesthetics$x, aesthetics$y, xbins=xbins)
	#grobs <- grid.grabExpr(grid.hexagons(hexes, ...))
	
	gTree(children=gList(hexagonsGrob(hexes, ...)))
}


