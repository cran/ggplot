# Quick plot.
# Quick plot is a convenient wrapper function for creating simple ggplot plot objects.
# You can use it like you'd use the \code{\link{plot}} function.
# 
# \code{qplot} provides a quick way to create simple plots.
# 
# @arguments x values
# @arguments y values
# @arguments grob type(s) to draw (can be a vector of multiple names)
# @arguments vector to use for colours
# @arguments vector to use for sizes
# @arguments vector to use for glyph types
# @arguments vector to use for line type
# @arguments vector to use for fill colour
# @arguments limits for x axis (defaults to range of data)
# @arguments limits for y axis (defaults to range of data)
# @arguments which variables to log transform ("x", "y", or "xy")
# @arguments character vector or expression for plot title
# @arguments character vector or expression for x axis label
# @arguments character vector or expression for y axis label
# @keyword hplot 
#X qplot(LETTERS[1:5], 1:5, type="rect", main="Blah", xlab="Hi")
#X qplot(LETTERS[1:5], 1:5, type=c("tile", "point"), main="Blah", xlab="Hi", ylim=c(0,10), col=1:5)
qplot <- function(x, y = NULL, data, facets = . ~ ., types = "point", col = NULL, size = NULL, glyph = NULL, line_type = NULL, fill = NULL, xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...) {
	if (!missing(data)) {
		df <- df2 <- data
	} else {
		df <- df2 <- do.call(data.frame, compact(list(x=x, y=y, colour=col, glyph=glyph, size=size, line_type=line_type, fill=fill)))
	}
	
	if (missing(data)) {
		facetvars <- all.vars(facets)
		facetvars <- facetvars[facetvars != "."]
		facetsdf <- as.data.frame(sapply(facetvars, get))
		if (nrow(facetsdf)) df <- cbind(df, facetsdf)
	}
	
	p <- ggplot(df, formula=deparse(substitute(facets)))
	if (!missing(data)) {
		p$defaults <- uneval(substitute(list(x=x, y=y, colour=colour, glyph=glyph, size=size, line_type=line_type, fill=fill)))[!c(missing(x), missing(y), missing(col), missing(glyph), missing(size), missing(line_type))]
	} else {
		p$defaults <- uneval(substitute(list(x=x, y=y, colour=colour, glyph=glyph, size=size, line_type=line_type, fill=fill)))[names(df2)]
	}
	
	p$title <- main
	if (!is.null(xlab)) p$xlabel <- xlab
	if (!is.null(ylab)) p$ylabel <- ylab

	for(type in types) {
		ggtype <- get(paste("gg", type, sep=""))
		p <- ggtype(p, ...)
	}
	
	
	logv <- function(var) var %in% log
	transf <- function(var) if (logv(var)) trans_log10 else trans_none
	

	if (logv("x") || !missing(xlim)) p <- pscontinuous(p, "x", range=xlim, trans=transf("x"))
	if (logv("y") || !missing(ylim)) p <- pscontinuous(p, "y", range=ylim, trans=transf("y"))
	
	p
}