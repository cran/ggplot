# Two basic types of legends
#   * continuous data (pretty breaks along range)
#   * categorical
# 
# This (interaction between scales and grobs), make independent? 
# (all default to same format of vertical list, say 6 distinct (pretty) values if continuous, otherwise all)
# 
# * colour on points
# * size on lines or points
# * shape on points
# * line type on lines



# Default guides
# Construct a default guide (legend) for a scale
# 
# This is used for automatic legends.
# 
# @arguments scale
# @keyword hplot 
guides.default <- function(scale, ...) {
	labels <- labels(scale)
	breaks <- breaks(scale)
	grob <- defaultgrob(scale)
	
	nkeys <- length(labels)
	hgap <- vgap <- unit(0.1, "lines")
	
	values <- data.frame(breaks)
	names(values) <- output(scale)

	widths <- unit.c(unit(2, "lines"), max(unit(rep(1, nkeys), "strwidth", as.list(labels))), hgap)
	heights <- unit.pmax(unit(1.4, "lines"), vgap + unit(rep(1, nkeys), "strheight", as.list(labels)))
	
	# Make a table, 
  legend.layout <- grid.layout(nkeys, 3, widths = widths, heights = heights, just=c("left","top"))
  fg <- frameGrob(layout = legend.layout)
	fg <- placeGrob(fg, rectGrob(gp=gpar(fill="NA", col="NA")))

	for (i in 1:nkeys) {
		df <- as.list(values[i,, drop=FALSE])
		df$x <- unit(0.5, "npc")
		df$y <- unit(0.5, "npc")
		fg <- placeGrob(fg, do.call(grob, list(df)), col = 1, row = i)
		fg <- placeGrob(fg, textGrob(labels[i], x = 0, y = 0.5, just = c("left", "centre")), col = 2, row = i)
	}

	fg
}
