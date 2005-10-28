# Useful little functions

# Compact list
# Remove all NULL entries from a list
# 
# @arguments list
# @keyword manip 
compact <- function(l) {
  l[!sapply(l, is.null)]
}

# GG Pretty
# Pretty axis breaks
#
# Same as \code{\link{grid.pretty}} but contains minimum and
# maximum of data as well.  Useful for legends.
#  
# @arguments values to prettify
# @keyword internal
ggpretty <- function(x) {
  unique(c(min(x), grid.pretty(x), max(x)))
}

# Defaults
# Convience method for combining a list of values with their defaults.
# 
# @arguments list of values
# @arguments defaults
# @keyword manip 
defaults <- function(x, y)  {
	c(x, y[setdiff(names(y), names(x))])
} 


# Cleaner version of match.fun
# Version of \code{\link{match.fun}} that returns NULL on failure
# 
# @arguments function name to find (character vector)
# @value function if found, otherwise NULL
# @keyword internal 
match.fun.null <- function(x) {
  f <- NULL
  try(f <- match.fun(x), silent=TRUE)
  f
}


# Unique default
# Convenience function for setting default if not unique
# 
# @arguments vector of values
# @arguments default to use if values not unique
# @keyword manip 
uniquedefault <- function(values, default) {
	unq <- unique(values)
	if (length(unq) == 1) unq[1] else "black"
}
