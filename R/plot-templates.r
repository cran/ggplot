# These functions provide template for creating common plots.
# They are also useful to illustrate some different capabilities of
# ggplot.

# Parallel coordinates plot.
# Generate a plot ``template'' for a paralell coordinates plot.
# 
# One way to think about a parallel coordinates plot, is as plotting 
# the data after it has transformation been transformed to gain a new
# variable.  This function does this using \code{\link[reshape]{melt}}.
# 
# This gives us enormous flexibility as we have separated out the 
# type of drawing (lines by tradition) and can now use any of the existing
# grob functions.  In particular this makes it very easy to create parallel
# boxplots, as shown in the example.
# 
# Three different scaling function are available:
# \itemize{
#   \item "range": scale coordinates to have common range $[0, 1]
#   \item "var": scale coordinates to have mean 0 and variance 1
#   \item "I": don't scale the coordinates at all 
# }
# @arguments data frame
# @arguments variables to include in parallel coordinates plot
# @arguments scaling function, one of "range", "var" or "I"
# @arguments other arguments passed on plot creation
# @keyword hplot 
#X ggline(ggpcp(mtcars))
#X ggline(ggpcp(mtcars, scale="var"))
#X ggline(ggpcp(mtcars, vars=names(mtcars)[3:6], formula= . ~cyl, scale="I"))
#X ggboxplot(ggpcp(mtcars, scale="I"))
#X ggline(ggpcp(mtcars, vars=names(mtcars[2:6])))
#X p <- ggpcp(mtcars, vars=names(mtcars[2:6]), formula=.~vs)
#X ggline(p)
#X ggline(p, aes=list(colour=mpg)) 
ggpcp <- function(data, vars=names(data), scale="range", ...) {
  force(vars)
	scale.range <- function(x) (x - min(x))/diff(range(x))
	scale.var <- function(x) scale(as.numeric(x))
	
	scaled <- switch(scale, 
		I = data[, vars],
		var = do.call(data.frame, lapply(data[, vars], scale.var)),
		range = do.call(data.frame, lapply(data[, vars], scale.range))
	)
  
  data <- cbind(scaled, data[, setdiff(names(data), vars), drop=FALSE])
  
	data$ROWID <- rownames(data)
	molten <- melt(data, m=vars)
	
	p <- ggplot(molten, aesthetics=list(x=variable, y=value, id=ROWID), ...)
	pscategorical(p, "x")
}

# Create a fluctuation diagram.
# 
# A fluctutation diagram is a graphical representation of a contingency
# table.  This fuction currently only supports 2D contingency tabless
# but extension to more should be relatively straightforward.
# 
# With the default size fluctuation diagram, area is proportional to the 
# count (length of sides proportional to sqrt(count))
# 
# @arguments a table of values, or a data frame with three columns, the last column being frequency
# @arguments size, or colour to create traditional heatmap
# @keyword hplot
#X ggfluctuation(table(warpbreaks[,c(2,3)]))
#X ggfluctuation(table(warpbreaks[,c(1,3)]))
ggfluctuation <- function(table, type="size") {
  if (is.table(table)) table <- as.data.frame(t(table))

  oldnames <- names(table)
  names(table) <- c("x","y", "freq")

  table$x <- as.factor(table$x)
  table <- transform(table,
    y = as.factor(y), 
    freq = sqrt(freq / max(freq))
  )
  
  if (type=="size") {
    p <- ggrect(ggplot(table, aesthetics = list(x=x, y=y, height=freq, width=freq)), justification=c("left", "bottom"))
    p <- pscategorical(p, "x", c(0, 1))
    p <- pscategorical(p, "y", c(0, 1))
  } else {
    p <- ggtile(ggplot(table, aesthetics = list(x=x, y=y, fill=freq)))
    p <- scfillgradient(p, low="white", high="red")
  }

  p$xlabel <- oldnames[1]
  p$ylabel <- oldnames[2]
  p
}