library(ggplot)
source.with.err <- function(path) {
	tryCatch(source(path), error = function(x) print(path))
}
lapply(dir("~/documents/ggplot/ggplot/R", full.name=T), source.with.err)


