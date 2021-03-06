# Grob grid
# Build up a subtle background grid 
# 
# @keyword hplot
# @keyword internal 
# @arguments not used 
# @arguments x axis lines
# @arguments y axis lines 
# @arguments not used
grob_grid <- function(aesthetics, xbreaks, ybreaks, fill=ggopt()$grid.fill, colour=ggopt()$grid.colour, ...) {
	gp <- gpar(fill=fill, col=colour)
	gTree(name = "grill", children = gList(
		rectGrob(gp=gpar(fill=fill, col=NA), name="grill-background"),
		segmentsGrob(xbreaks, unit(0, "npc"), xbreaks, unit(1, "npc"), gp = gp, default.units="native", name="grill-vertical"),
		segmentsGrob(unit(0, "npc"), ybreaks, unit(1, "npc"), ybreaks, gp = gp, default.units="native", name="grill-horizontal"),
		rectGrob(gp=gpar(col=colour, lwd=3, fill=NA), name="grill-border")
	))	
}


# Grob axis
# Grob for axes
# 
# @arguments position of ticks
# @arguments labels at ticks
# @arguments position of axis (top, bottom, left or right)
# @arguments range of data values
# @keyword hplot 
# @keyword internal
ggaxis <- function(at, labels, position="right", scale=c(0,1)) {
	#assert.equal(length(at), length(labels))
	
	positions <- c("top","bottom", "right","left")
	#position <- match(positions, position)
	
	line_grob <- ggaxis_line(at, position)
	ticks_grob <- ggaxis_ticks(at, position)
	labels_grob <- ggaxis_labels(at, labels, position)
	
	gTree(childrenvp=ggaxis_vp(position, labels, scale), children=gList(ticks_grob, labels_grob), gp=gpar(col=ggopt()$axis.colour), name="axis")
}
 
# Axis viewport path
# Compute viewport path for specified component of axis
# 
# @arguments position of axis
# @arguments component name
# @keyword hplot 
# @keyword internal
axis_vp_path <- function(position, name) {
	vpPath(paste(position, "axis", sep="_"), name)
}

# Grob axis line
# Grob for axis baseline
# 
# @arguments position of ticks
# @arguments position of axis (top, bottom, left or right)
# @keyword hplot 
# @keyword internal
ggaxis_line <- function(at, position) {
	vp <- axis_vp_path(position, "ticks")
	ends <- unit(range(at), "native")
	
	switch(position,
		top =    linesGrob(ends, unit(c(0,0), "npc"), name = "axis-major", vp=vp),
		bottom = linesGrob(ends, unit(c(1,1), "npc"), name = "axis-major", vp=vp),
		left =   linesGrob(unit(c(1,1), "npc"), ends, name = "axis-major", vp=vp),
		right =  linesGrob(unit(c(0,0), "npc"), ends, name = "axis-major", vp=vp)
	)
	
}

# Grob axis ticks
# Grob for axis ticks
# 
# @arguments position of ticks
# @arguments position of axis (top, bottom, left or right)
# @keyword hplot 
# @keyword internal
ggaxis_ticks <- function(at, position) {
	vp <- axis_vp_path(position, "ticks")
	switch(position,
		top =    ,
		bottom = segmentsGrob(unit(at, "native"), unit(0.1, "npc"), unit(at, "native"), unit(1, "npc"), vp=vp, name="axis-ticks"),
		left =   ,
		right =  segmentsGrob(unit(0.1, "npc"), unit(at, "native"), unit(1, "npc"), unit(at, "native"), vp=vp, name="axis-ticks"),
	)	
}

# Grob axis labels
# Grob for axis lables
# 
# @arguments position of ticks
# @arguments labels at ticks
# @arguments position of axis (top, bottom, left or right)
# @keyword hplot 
# @keyword internal
ggaxis_labels <- function(at, labels, position) {
	vp <- axis_vp_path(position, "labels")
	gp <- gpar(cex = 0.95)

	switch(position,
		top =    textGrob(labels, unit(at, "native"), unit(0.7, "npc"), just = c("centre","top"), rot = 0, check.overlap = TRUE, vp=vp, name="axis-labels", gp=gp),
		bottom = textGrob(labels, unit(at, "native"), unit(0.7, "npc"), just = c("centre","top"), rot = 0, check.overlap = TRUE, vp=vp, name="axis-labels", gp=gp),
		left =   textGrob(labels, unit(0.9, "npc"), unit(at, "native"), just = c("right","centre"), rot = 0, check.overlap = TRUE, vp=vp, name="axis-labels", gp=gp),
		right =  textGrob(labels, unit(0.9, "npc"), unit(at, "native"), just = c("right","centre"), rot = 0, check.overlap = TRUE, vp=vp, name="axis-labels", gp=gp),
	)	
}

# Grob axis viewport
# Generate viewport for axis grobs
# 
# @arguments position of axis (top, bottom, left or right)
# @arguments labels at ticks
# @arguments range of data values
# @returns viewport tree
# @keyword hplot 
# @keyword internal
ggaxis_vp <- function(position, labels, scale=c(0,1)) {
	tick_size <- unit(0.4, "lines")
	label_size <- switch(position, 
		top = ,
		bottom = max(unit(rep(1.1,length(labels))*1.4, "strheight", as.list(labels))),
		left = ,
		right = max(unit(rep(1.1,length(labels))*1.4, "strwidth", as.list(labels)))
	)
	
	#viewport with named parts labels and text
	layout <- switch(position,
		top =    grid.layout(nrow=2, ncol=1, heights=unit.c(label_size, tick_size), widths=unit(1,"npc")),
		bottom = grid.layout(nrow=2, ncol=1, heights=unit.c(tick_size, label_size), widths=unit(1,"npc")),
		left =   grid.layout(nrow=1, ncol=2, widths=unit.c(label_size, tick_size), heights=unit(1,"npc")),
		right =  grid.layout(nrow=1, ncol=2, widths=unit.c(tick_size, label_size), heights=unit(1,"npc")),
	)

	vp_top <- switch(position,
		top =    ,
		bottom = viewport(layout=layout, height=label_size + tick_size, width=unit(1,"npc"), name=paste(position, "axis", sep="_")),
		left =   ,
		right =  viewport(layout=layout, width=label_size + tick_size, height=unit(1,"npc"), name=paste(position, "axis", sep="_"))
	)
	
	vp_labels <- switch(position,
		top =    viewport(layout.pos.row = 1, layout.pos.col = 1, name="labels", xscale=scale, clip="off"),
		bottom = viewport(layout.pos.row = 2, layout.pos.col = 1, name="labels", xscale=scale, clip="off"),
		left =   viewport(layout.pos.row = 1, layout.pos.col = 1, name="labels", yscale=scale, clip="off"),
		right =  viewport(layout.pos.row = 1, layout.pos.col = 2, name="labels", yscale=scale, clip="off")
	)

	vp_ticks <- switch(position,
		top =    viewport(layout.pos.row = 2, layout.pos.col = 1, name="ticks", xscale=scale),
		bottom = viewport(layout.pos.row = 1, layout.pos.col = 1, name="ticks", xscale=scale),
		left =   viewport(layout.pos.row = 1, layout.pos.col = 2, name="ticks", yscale=scale),
		right =  viewport(layout.pos.row = 1, layout.pos.col = 1, name="ticks", yscale=scale)
	)
	
	vpTree(vp_top, vpList(vp_labels, vp_ticks))
	
}