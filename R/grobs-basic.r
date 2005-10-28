
# gg add
# Convenience method to make writing gg\_XXXX functions easier.
# 
# Will automatically add scales if needed.
# 
# @arguments type of grob mapping to add
# @arguments plot object
# @arguments list of aesthetic mappings
# @arguments parameters passed to grob function
# @arguments data frame
# @keyword hplot  
# @keyword internal 
# @value modified plot object 
gg_add <- function(map, plot, aesthetics=list(), ..., data=plot$data) {
	aesthetics <- substitute(aesthetics, parent.frame())
	
	new_aesthetics <- setdiff(names(uneval(aesthetics)), input(plot$scales))
	plot <- add_defaults(plot, new_aesthetics)
	do.call(plot_add, list(plot=plot, data=data, map=map, aesthetics=aesthetics, ...))
}

# Grob grid
# Build up a subtle background grid 
# 
# @keyword hplot
# @keyword internal 
# @arguments not used 
# @arguments x axis lines
# @arguments y axis lines 
# @arguments not used
grob_grid <- function(aesthetics, xbreaks, ybreaks, ...) {
	gp <- gpar(fill=ggplot.options()$grid.fill, col=ggplot.options()$grid.col)
	gTree(children = gList(
		rectGrob(gp=gpar(fill=ggplot.options()$grid.fill, col=ggplot.options()$grid.col, lwd=4)),
		grid.segments(xbreaks, unit(0, "npc"), xbreaks, unit(1, "npc"), gp = gp, default.units="native"),
		grid.segments(unit(0, "npc"), ybreaks, unit(1, "npc"), ybreaks, gp = gp, default.units="native")
	))	
}


# Grob function: point
# Add points to a plot
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
#   \item size: size of the point, in mm (see \code{\link{scsize})}
#   \item shape: shape of the glyph used to draw the point (see \code{\link{scshape})}
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
#   \item unique: if \code{TRUE, draw at most one point at each unique location}
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
#X p <- ggplot(mtcars, aesthetics=list(x=wt, y=mpg))
#X ggpoint(p)
#X ggpoint(p, list(colour=cyl))
#X ggpoint(p, list(blahbalh=cyl)) #unknown aesthetics are ignored
#X ggpoint(p, list(shape=cyl))
#X ggpoint(p, list(shape=cyl, colour=cyl))
#X ggpoint(p, list(size=mpg))
#X ggpoint(p, list(size=mpg/wt))
#X ggpoint(p, list(x=cyl, colour=cyl))
#X p <- ggplot(mtcars)
#X ggpoint(p, aesthetics=list(x=wt, y=mpg))
ggpoint <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("point", plot, aesthetics, ..., data=data)
}
grob_point <- function(aesthetics, unique=TRUE, ...) {
  if (length(aesthetics$x) + length(aesthetics$y) == 0) return();
	aesthetics <- defaults(aesthetics, list(colour="black", size=2, shape=16, rotation=0))
  
	if (unique) aesthetics <- unique(data.frame(aesthetics))
	if (length(aesthetics$x) + length(aesthetics$y)==0) return()
		
	pointsGrob(
		aesthetics$x, aesthetics$y, size=unit(aesthetics$size, "mm"), pch=aesthetics$shape, gp = gpar(col=as.character(aesthetics$colour), rot=aesthetics$rotation)
	)
}

# Grob function: abline 
# Add line specified by slope and intercept to a plot
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item none
# }
#
# Other options:
# 
# \itemize{
#   \item intercept: intercept(s) of line
#   \item slope: slope(s) of line
#   \item colour: line colour
#   \item size: line thickness, in mm
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
#X p <- ggplot(mtcars, aesthetics=list(x = wt, y=mpg))
#X ggabline(ggpoint(p), intercept=30, slope=-5)
#X ggabline(ggpoint(p), intercept=c(30,40,50), slope=-5)
#X ggsmooth(ggpoint(p), method=lm, formula=y~x) 
ggabline <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("abline", plot, aesthetics, ..., data=data)
}
grob_abline <- function(aesthetics, intercept=0, slope=1, colour="grey", size=1, ...) {
	xrange <- range(range(aesthetics$x, na.rm=TRUE), c(-100,100))

	build_line <- function(intercept, slope, colour, size) {
		y <- function(x) x * slope + intercept
		list(x=xrange, y=y(xrange), colour=colour, size=size)
	}
	
	aesthetics <- mapply(build_line, intercept, slope, colour, size, SIMPLIFY=FALSE)
	gTree(children = do.call(gList,
		lapply(aesthetics, grob_line)
	))
}


# Grob function: jittered points
# Add jittered points to a plot
# 
# This is useful when plotting points with a categorical axis so to
# avoid overplotting.
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
#   \item size: size of the point, in mm (see \code{\link{scsize})}
#   \item shape: shape of the glyph used to draw the point (see \code{\link{scshape})}
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
#   \item xjitter: degree of jitter in x direction, see \code{\link{jitter} for details, defaults to 1}
#   \item yjitter: degree of jitter in y direction, see \code{\link{jitter} for details, defaults to 0}
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
#X p <- ggplot(movies, aes=list(x=mpaa, y=rating))
#X ggjitter(p)
#X ggboxplot(ggjitter(p))
#X ggboxplot(ggjitter(p), xjitter=2)
#X ggboxplot(ggjitter(p), yjitter=1)
ggjitter <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("jitter", plot, aesthetics, ..., data=data)
}
grob_jitter <- function(aesthetics, xjitter=1, yjitter=0, ...) {
	aesthetics <- defaults(aesthetics, list(colour="black", size=2, shape=16, rotation=0))
	aesthetics$x <- jitter(aesthetics$x, xjitter)
	aesthetics$y <- jitter(aesthetics$y, yjitter)

	grob_point(aesthetics)
}

# Grob function: text
# Add text to a plot
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
#   \item label: text label to display
#   \item size: size of the text, as a multiple of the default size, (see \code{\link{scsize})}
#   \item rotation: angle, in degrees, of text label
#   \item colour: text colour (see \code{\link{sccolour})}
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
#   \item justification: justification of the text relative to its (x, y) location, see \code{\link{textGrob} for more details}
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
#X p <- ggplot(mtcars, aesthetics=list(x=wt, y=mpg, labels = rownames(mtcars)))
#X ggtext(p)
#X ggtext(p, list(size=wt))
#X scsize(ggtext(p, list(size=wt)), c(0.5, 1.5))
#X ggtext(p, list(colour=cyl))
ggtext <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("text", plot, aesthetics, ..., data=data)
}
grob_text  <- function(aesthetics, justification="centre", ...) {
	aesthetics <- defaults(aesthetics, list(colour="black", size=1, rotation=0))
	textGrob(aesthetics$label, aesthetics$x, aesthetics$y, default.units="native", just=justification, rot=aesthetics$rotation, gp=gpar(col=as.character(aesthetics$colour), cex=aesthetics$size))
}


# Grob function: path
# Add a path (a line between points in the order that they appear in the dataset) to the plot
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
#   \item id: identifier variable used to break up into multiple paths
#   \item size: size of the line, in mm (see \code{\link{scsize}})
#   \item colour: line colour (see \code{\link{sccolour}})
#   \item line\_type: line style/type (see \code{\link{sclinetype}})
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
#   \item none
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
#X myear <- do.call(rbind, by(movies, movies$year, function(df) data.frame(year=df$year[1], mean.length = mean(df$length), mean.rating=mean(df$rating))))
#X p <- ggplot(myear, aesthetics=list(x=mean.length, y=mean.rating))
#X ggpath(p)
#X ggpath(p, list(size=year))
#X ggpath(p, list(colour=year))
#X ggpath(scsize(p, c(0.5,1)), list(size=year))
#X ggpath(scsize(p, c(0.5,1)), list(size=year))
#X p <- ggplot(mtcars, aesthetics=list(x=drat, y=wt))
#X ggpath(p)
#X ggpath(p, list(id=cyl))
#x ggpath(p, list(id=cyl, linetype=cyl))
ggpath <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("path", plot, aesthetics, ..., data=data)
}
grob_path  <- function(aesthetics, ...) {
  if (length(aesthetics$x) + length(aesthetics$y) == 0) return();
	aesthetics <- defaults(aesthetics, list(id=1, colour="black", size=1, linetype=1))
	
	data <- data.frame(aesthetics)

	polygonGrob()
	path <- function(data) {
		n <- nrow(data)
		if (n<2) return(NULL)
		segmentsGrob(as.numeric(data$x[-n]), as.numeric(data$y[-n]),as.numeric(data$x[-1]),as.numeric(data$y[-1]), default.units="native", gp=gpar(col=as.character(data$colour), lwd=data$size, lty=data$linetype))
	}
	segs <- by(data, data$id, path)
	
	gTree(children = do.call(gList, segs))
}

# Grob function: polygon
# Add polygons to a plot
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
#   \item id: identifier variable used to break up into multiple polygons
#   \item size: size of the outline, in mm (see \code{\link{scsize})}
#   \item colour: outline colour (see \code{\link{sccolour})}
#   \item fill: internal colour (see \code{\link{sccolour})}
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
#   \item none
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot 
ggpolygon <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("polygon", plot, aesthetics, ..., data=data)
}
grob_polygon  <- function(aesthetics, ...) {
	aesthetics <- defaults(aesthetics, list(id=1, colour="black", size=1, pattern=1))
	
	polygonGrob(aesthetics$x, aesthetics$y, default.units="native", gp=gpar(col=aesthetics$colour, fill=aesthetics$fill, lwd=aesthetics$lwd, pattern=aesthetics$pattern))
}

# Grob function: line
# Add a line to the plot
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
#   \item id: identifier variable used to break up into multiple paths
#   \item size: size of the line, in mm (see \code{\link{scsize}})
#   \item colour: line colour (see \code{\link{sccolour}})
#   \item line\_type: line style/type (see \code{\link{sclinetype}})
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
#   \item none
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
#X mry <- do.call(rbind, by(movies, round(movies$rating), function(df) { 
#X 	nums <- tapply(df$length, df$year, length)
#X 	data.frame(rating=round(df$rating[1]), year = as.numeric(names(nums)), number=as.vector(nums))
#X }))
#X p <- ggplot(mry, aesthetics = list(x=year, y=number, id=rating))
#X ggpath(p)
#X ggpath(p, list(size=rating))
#X ggpath(p, list(colour=rating))
ggline <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("line", plot, aesthetics, ..., data=data)
}
grob_line  <- function(aesthetics, ...) {
  aesthetics <- defaults(aesthetics, list(id=1,  colour="black", size=1, linetype=1))

  if (length(aesthetics$x) == 1 ) {
    return(linesGrob(x = unit(c(0, 1), "npc"), y=unit(c(0.5, 0.5), "npc"), gp=gpar(col=as.character(aesthetics$colour), lwd=aesthetics$size, lty=aesthetics$linetype)))
  }

	aesthetics <- data.frame(aesthetics)
  aesthetics <- aesthetics[order(aesthetics$id, aesthetics$x), ]
	grob_path(aesthetics)
}

# Area grob
#
#
# @arguments x positions
# @arguments y positions
# @arguments id variable used to separate observations into different areas
# @arguments colour
# @arguments pattern
# @arguments ...
# @keyword hplot
# @keyword internal 
ggarea <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("area", plot, aesthetics, ..., data=data)
}
grob_area  <- function(aesthetics, ...) {
	aesthetics <- defaults(aesthetics, list(id=1, colour="black", pattern=1))

	data <- data.frame(aesthetics$x, aesthetics$y, aesthetics$id, aesthetics$colour, aesthetics$pattern)
	data <- data[order(data$id, data$x), ]

	poly <- function(data) {
		n <- nrow(data)
		with(data, 
			polygonGrob(c(min(x), x, max(x)), c(0, y, 0), default.units="native", gp=gpar(fill=as.character(colour)))
		)
	}
	segs <- by(data, data$id, poly)
	
	do.call(gList, segs)
}

# Grob function: rectangle
# Add rectangles to a plot
# 
# The default arguments will draw a barchart.
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
#   \item width: width of the rectangle (required)
#   \item height: height of the rectangle (required)
#   \item fill: fill colour (see \code{\link{sccolour})}
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
#   \item justification: justification of the bar relative to its (x, y) location, see \code{\link{rectGrob} for more details}
#   \item colour: a character vector describing the line colour}
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
#X p <- ggplot(mtcars, aes=list(y=mpg, x=factor(cyl)))
#X ggrect(p)
#X ggrect(p, list(fill=mpg))
#X pscontinuous(ggrect(p, list(fill=mpg), colour="black"), "y", range=c(0,NA))
ggrect <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("rect", plot, aesthetics, ..., data=data)
}
grob_rect   <- function(aesthetics, justification = c("centre","top"), colour="white", ...) {
	aesthetics <- defaults(aesthetics, list(fill="grey50", pattern=1, height=aesthetics$y, width=resolution(aesthetics$x)*0.9))

	rectGrob(aesthetics$x, aesthetics$y, width=aesthetics$width, height=aesthetics$height, default.units="native", just=justification, gp=gpar(col=colour, fill=as.character(aesthetics$fill)))
}


# Grob function: tile
# Add tiles to a plot
# 
# The tile grob will tile the plot surface as densly as possible, assuming
# that every tile is the same size.  It is similar to \code{\link{levelplot}}
# or \code{\link{image}}.  
# 
# Aesthetic mappings that this grob function understands:
#
# \itemize{
#   \item x: x position (required)
#   \item y: y position (required)
#   \item width: width of the rectangle
#   \item height: height of the rectangle
#   \item fill: fill colour (see \code{\link{sccolour})}
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
#   \item none
# }
# 
# @arguments the plot object to modify
# @arguments named list of aesthetic mappings, see details for more information
# @arguments other options, see details for more information
# @arguments data source, if not specified the plot default will be used
# @keyword hplot
# @seealso \code{\link{ggrect}}, \code{\link{resolution}}
#X pp <- function (n,r=4) {
#X  x <- seq(-r*pi, r*pi, len=n)
#X  df <- expand.grid(x=x, y=x)
#X  df$r <- sqrt(df$x^2 + df$y^2)
#X  df$z <- cos(df$r^2)*exp(-df$r/6)
#X  df
#X }
#X p <- ggplot(pp(20), aes=list(x=x,y=y))
#X ggtile(p) #pretty useless!
#X ggtile(p, list(fill=z))
#X ggtile(p, list(height=abs(z), width=abs(z)))
#X ggtile(ggplot(pp(100), aes=list(x=x,y=y,fill=z)))
#X ggtile(ggplot(pp(100, r=2), aes=list(x=x,y=y,fill=z)))
#X p <- ggplot(pp(20)[sample(20*20, size=200),], aes=list(x=x,y=y,fill=z))
#X ggtile(p)
ggtile <- function(plot = .PLOT, aesthetics=list(), ..., data=plot$data) {
	gg_add("tile", plot, aesthetics, ..., data=data)
}
grob_tile  <- function(aesthetics, ...) {
	if (length(aesthetics$x) == 1) {
	  colour <- if(is.null(aesthetics$colour)) aesthetics$fill else aesthetics$colour
		return(rectGrob(gp=gpar(col=NA, fill=as.character(colour))))
	}

	aesthetics <- defaults(aesthetics, list(
	  width=resolution(aesthetics$x), 
	  height=resolution(aesthetics$y)
	))
	
	grob_rect(aesthetics, colour="NA", justification=c("centre","centre"))
}

# Resolution
# Compute the "resolution" of a data vector, ie. what's the smallest non-zero
# distance between adjacent values.
#
# @arguments numeric vector
# @seealso \code{\link{ggtile}}
# @keyword hplot
# @keyword internal 
resolution <- function(x) min(diff(sort(unique(x))))