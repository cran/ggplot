# Add default scales.
# Add default scales to a plot.
# 
# You shouldn't need to call this function yourself.  If you want to add a
# scale to a plot, use \code{\link{add_scale}}.
# 
# @arguments plot object, if not specified will use current plot
# @arguments character vector of needed scales to add, see \code{\link{scales}} for possible options
# @keyword hplot 
# @keyword internal
add_defaults <- function(p = .PLOT, new) {
	scales <- compact(lapply(paste("scale", new, sep="_"), match.fun.null))
	for(s in scales) {
		p <- add_scale(p, s())
	}
	p
}

# Plot add.
# Add graphical objects using specific mapping.
#
# This is the powerhouse function that you use to actually display
# stuff on your plot.  
#
# You should really keep track of the new plot object that is created
# by this function, but if you're lazy and don't want to, it automatically
# stores the result in the "global" variable \code{.PLOT}
#
# @arguments plot object
# @arguments data to use
# @arguments how to map data into graphics object
# @arguments arguments passed down to mapping object specifying (eg.) aesthetics to use
# @keyword hplot
# @keyword internal
plot_add <- function(plot=.PLOT, data=NULL, map="point", aesthetics=list(), ...) {
	dots <- match.call(expand.dots=FALSE)$"..."
	aesthetics <- uneval(substitute(aesthetics, parent.frame()))
	
	plot_add_grobs(plot, grobPromise(map, data, params=list(...), aesthetics=aesthetics))
}

# Add grobs to plot
# Add grobs to plot grob list
#
# @arguments plot object
# @arguments matrix of grobs
# @keyword hplot
# @keyword internal
plot_add_grobs <- function(plot, grob_matrix) {
	plot$grobs <- c(plot$grobs, list(grob_matrix))
	(.PLOT <<- plot)	
}

# Ggplot plot
# Creates a complete ggplot grob.
#
# Delegates almost everything to its arguments.  Responsible for the 
# transformation chain and for collecting everything into one grob with the
# appropriate viewports
#
# @arguments plot object
# @arguments viewports
# @arguments panels
# @arguments guides
# @arguments should the plot be wrapped up inside the pretty accountrements (labels, legends, etc)
# @keyword hplot
# @keyword internal
ggplot_plot <- function(plot, viewport=viewport_default(plot, guides, plot$scales), panels=panels_default(plot, grobs), guides=guides_basic(plot, plot$scales), pretty=TRUE) {
	if (length(plot$grobs) == 0) stop("No grobs to plot")
	
	new_aesthetics <- setdiff(names(plot$defaults), input(plot$scales))
	plot <- add_defaults(plot, new_aesthetics)
	pre <- lapply(plot$grobs, preprocess_all, plot=plot)
	plot <- add_position(add_position(plot, pre, "x"), pre, "y")
	
	update(plot$scales) <- pre

	aes <- map_all(plot$scales, pre)
	grobs <- mapply(make_all_grobs, plot$grobs, aes, SIMPLIFY=FALSE)
	
	plotgrob <- gTree(children=do.call(gList, c(unlist(guides, recursive=FALSE), panels)), childrenvp = viewport)
	if (!pretty) return(plotgrob)
	prettyplot(plotgrob, plot$title, plot$xlabel, plot$ylabel, legends(plot$scales))
}

