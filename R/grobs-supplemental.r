# Grob strip
# Grob for strip labels
# 
# @arguments text to display
# @arguments orientation, horizontal or vertical
# @keyword hplot 
# @keyword internal
ggstrip <- function(text, horizontal=TRUE) {
	gTree(children = gList(
		rectGrob(gp=gpar(fill="grey80", col="white")),
		textGrob(text, rot=-90 * (1 - horizontal))
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
	
	gTree(childrenvp=ggaxis_vp(position, labels, scale), children=gList(ticks_grob, labels_grob), gp=gpar(col="grey50"))
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
		top =    linesGrob(ends, unit(c(0,0), "npc"), name = "major", vp=vp),
		bottom = linesGrob(ends, unit(c(1,1), "npc"), name = "major", vp=vp),
		left =   linesGrob(unit(c(1,1), "npc"), ends, name = "major", vp=vp),
		right =  linesGrob(unit(c(0,0), "npc"), ends, name = "major", vp=vp),
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
		bottom = segmentsGrob(unit(at, "native"), unit(0, "npc"), unit(at, "native"), unit(1, "npc"), name = "ticks", vp=vp),
		left =   ,
		right =  segmentsGrob(unit(0, "npc"), unit(at, "native"), unit(1, "npc"), unit(at, "native"), name = "ticks", vp=vp),
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
	
	switch(position,
		top =    textGrob(labels, unit(at, "native"), unit(0.8, "npc"), just = c("centre","top"), rot = 0, check.overlap = FALSE, name = "labels", vp=vp),
		bottom = textGrob(labels, unit(at, "native"), unit(0.8, "npc"), just = c("centre","top"), rot = 0, check.overlap = FALSE, name = "labels", vp=vp),
		left =   textGrob(labels, unit(1, "npc"), unit(at, "native"), just = c("right","centre"), rot = 0, check.overlap = TRUE, name = "labels", vp=vp),
		right =  textGrob(labels, unit(1, "npc"), unit(at, "native"), just = c("right","centre"), rot = 0, check.overlap = TRUE, name = "labels", vp=vp),
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
		bottom = max(unit(rep(2,length(labels))*1.4, "strheight", as.list(labels))),
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
		top =    viewport(layout.pos.row = 1, layout.pos.col = 1, name="labels", xscale=scale),
		bottom = viewport(layout.pos.row = 2, layout.pos.col = 1, name="labels", xscale=scale),
		left =   viewport(layout.pos.row = 1, layout.pos.col = 1, name="labels", yscale=scale),
		right =  viewport(layout.pos.row = 1, layout.pos.col = 2, name="labels", yscale=scale)
	)

	vp_ticks <- switch(position,
		top =    viewport(layout.pos.row = 2, layout.pos.col = 1, name="ticks", xscale=scale),
		bottom = viewport(layout.pos.row = 1, layout.pos.col = 1, name="ticks", xscale=scale),
		left =   viewport(layout.pos.row = 1, layout.pos.col = 2, name="ticks", yscale=scale),
		right =  viewport(layout.pos.row = 1, layout.pos.col = 1, name="ticks", yscale=scale)
	)
	
	vpTree(vp_top, vpList(vp_labels, vp_ticks))
	
}