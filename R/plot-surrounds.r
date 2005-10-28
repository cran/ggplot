# Pretty plot
# Build a plot with all the usual bits and pieces.
# 
# As well as the plotting area, a plot need:
#  \item main title
#  \item x and y axis labels
#  \item space for legends (currently on the right hand side)
# 
# This function sets up the appropriate viewports and packs the
# various components in.  The viewport is set up so that each component
# will only take up the amount of space that it requires.  
# 
# @arguments plot
# @arguments title (character vector)
# @arguments x axis label (character vector)
# @arguments y axis label (character vector)
# @arguments legend grobs (list of grobs)
# @keyword hplot 
prettyplot <- function(plot, title, xlabel, ylabel, legend=NULL) {
	title <- textGrob(title, gp=gpar(fontsize=13), just=c("centre", "centre"))
	xlabel <- textGrob(xlabel)
	ylabel <- textGrob(ylabel, rot=90)
	
	if (!is.null(legend)) {
		layout <- grid.layout(3, 3, 
			widths = unit.c(unit(1, "grobwidth", ylabel), unit(1, "null"), unit(1, "grobwidth", legend)),
			height = unit.c(unit(1.2, "grobheight", title), unit(1, "null"), unit(1, "grobheight", xlabel))
		)
	} else {
		layout <- grid.layout(3, 2, 
			widths = unit.c(unit(1, "grobwidth", ylabel), unit(1, "null")),
			height = unit.c(unit(2, "grobheight", title), unit(1, "null"), unit(1, "grobheight", xlabel))
		)
	}
	
	lf <- frameGrob(layout)
	lf <- placeGrob(lf, plot,   row=2,   col=2)
	if (!is.null(legend)) lf <- packGrob(lf, legend, row=2:3, col=3)
	lf <- placeGrob(lf, title,  row=1,   col=2)
	lf <- placeGrob(lf, xlabel,  row=3,   col=2)
	lf <- placeGrob(lf, ylabel,  row=2,   col=1)

	lf
}