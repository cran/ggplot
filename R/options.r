# Set ggplot options
# Set global options for ggplot.
# 
# \item[grid.col] colour of background grid lines, ("white")
# \item[grid.fill] colour of background, ("grey90")
# 
# @arguments list of options to get/set
# @keyword manip 
ggplot.options <- function(...) {
    if (nargs() == 0)  return(.ggplot.Options)
    current <- .ggplot.Options
    temp <- list(...)
    if (length(temp) == 1 && is.null(names(temp))) {
        arg <- temp[[1]]
        switch(mode(arg), list = temp <- arg, character = return(.ggplot.Options[arg]), 
            stop("invalid argument: ", sQuote(arg)))
    }
    if (length(temp) == 0) 
        return(current)
    n <- names(temp)
    if (is.null(n)) stop("options must be given by name")
    changed <- current[n]
    current[n] <- temp
    if (sys.parent() == 0) 
        env <- asNamespace("ggplot")
    else env <- parent.frame()
    #assign(".ggplot.Options", current, envir = env)
    .ggplot.Options <<- current
    invisible(current)
}
.ggplot.Options <- list(
	grid.col = "white",
	grid.fill = "grey90"	
)
