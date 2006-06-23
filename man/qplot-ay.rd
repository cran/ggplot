\name{qplot}
\alias{qplot}
\title{Quick plot.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Quick plot is a convenient wrapper function for creating simple ggplot plot objects.
}
\usage{qplot(x, y = NULL, data, facets = . ~ ., types = "point", col = NULL, size = NULL, glyph = NULL, line_type = NULL, fill = NULL, xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...)}
\arguments{
\item{x}{x values}
\item{y}{y values}
\item{data}{grob type(s) to draw (can be a vector of multiple names)}
\item{facets}{vector to use for colours}
\item{types}{vector to use for sizes}
\item{col}{vector to use for glyph types}
\item{size}{vector to use for line type}
\item{glyph}{vector to use for fill colour}
\item{line_type}{limits for x axis (defaults to range of data)}
\item{fill}{limits for y axis (defaults to range of data)}
\item{xlim}{which variables to log transform ("x", "y", or "xy")}
\item{ylim}{character vector or expression for plot title}
\item{log}{character vector or expression for x axis label}
\item{main}{character vector or expression for y axis label}
\item{xlab}{}
\item{ylab}{}
\item{...}{}
}

\details{\code{qplot} provides a quick way to create simple plots.}

\examples{qplot(LETTERS[1:5], 1:5, type="rect", main="Blah", xlab="Hi")
qplot(LETTERS[1:5], 1:5, type=c("tile", "point"), main="Blah", xlab="Hi", ylim=c(0,10), col=1:5)
qplot(wt, mpg, data=mtcars, col=cyl, glyph=cyl, size=wt)}
\keyword{hplot}
