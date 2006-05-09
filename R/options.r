# Set ggplot options
# Set global options for ggplot.
# 
# These are aliased into every plot object, so that \code{p$grid.col} will
# return the default grid colour, unless it has been overriden for a particular
# plot object.  You can change the global options using the function, or the
# options for a specific plot by setting the values directly on the object.  See
# the examples for more details.
# 
# Colour settings:
# 
# \itemize{
# 	\item background.colour: background text colour ("black"), used for title and axis text
# 	\item background.fill:   background fill ("white")
# 	\item grid.colour: plot grid colour ("white")
# 	\item grid.fill:   plot grid background fill ("grey90")
# 	\item strip.colour: strip text colour ("white")
# 	\item strip.fill:   strip background fill ("grey80")
# }
# 
# Strip settings
# 
# \itemize{
# 	\item strip.text:   function with two arguments (variable, and value) used for
# 		generating strip labels
# }
# 
# Legend settings
# 
# \itemize{
# 	\item legend.position:   position of legend: "none" to hide legend;
# 		"left", "right", "top", "bottom", for positioning outside of plot;
# 		% c(x, y) for positioning on top of plot
# }
# 
# Other settings:
# 
# \itemize{
# 	\item aspect.ratio: aspect ratio of facets.  Set to \code{NULL} to allow
#				 to vary with device size
# }
# 
# @arguments list of options to get/set
# @keyword manip 
# @alias ggopt
#X ggopt(background.fill = "black", background.color ="white") # all new plots will use this
#X p <- ggpoint(ggplot(tips, smoker ~ sex,aesthetics = list(y = tip, x = total_bill)))
#X p
#X p$background.fill = "white"
#X p
#X p$strip.colour <- "red"
#X p$strip.fill <- "yellow"
#X p$background.colour <- "pink"
#X p$grid.colour <- "green"
#X p$grid.fill <- "blue"
#X p # a very ugly plot!
#X ggopt(background.fill = "white", background.color ="black")
.ggopt.build <- function(...) {
	opt <- list(
		background.fill = "white",
		background.colour = "black",
		grid.colour = "white",
		grid.fill = "grey90",
		strip.colour = "white",
		strip.fill = "grey80",
		strip.text = function(variable, value) {
			paste(variable, value, sep=": ")
		},
		legend.position = "right",
		aspect.ratio = NULL,
	)
	
	function(...) {
		opt <<- updatelist(opt, list(...))
		invisible(opt)
	}
}
ggopt <- .ggopt.build()

# Access ggplot options
# Alias default options to plot object
# 
# @keyword internal
"$.ggplot" <- function(x, i) {
	val <- x[[i]]
	if (is.null(val))
		val <- ggopt()[[i]]
	val
}