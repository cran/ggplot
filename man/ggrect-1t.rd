\name{ggrect}
\alias{ggrect}
\title{Grob function: rectangle}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Add rectangles to a plot
}
\usage{ggrect(plot = .PLOT, aesthetics=list(), ..., data=plot$data)}
\arguments{
\item{plot}{the plot object to modify}
\item{aesthetics}{named list of aesthetic mappings, see details for more information}
\item{...}{other options, see details for more information}
\item{data}{data source, if not specified the plot default will be used}
}

\details{The default arguments will draw a barchart.

Aesthetic mappings that this grob function understands:

\itemize{
\item x: x position (required)
\item y: y position (required)
\item width: width of the rectangle (required)
\item height: height of the rectangle (required)
\item fill: fill colour (see \code{\link{sccolour})}
}

These can be specified in the plot defaults (see \code{\link{ggplot}}) or
in the \code{aesthetics} argument.  If you want to modify the position
of the points or any axis options, you will need to add a position scale to
the plot.  These functions start with \code{ps}, eg.
\code{\link{pscontinuous}} or \code{\link{pscategorical}}

Other options:

\itemize{
\item justification: justification of the bar relative to its (x, y) location, see \code{\link{rectGrob} for more details}
\item colour: a character vector describing the line colour}
}}

\examples{p <- ggplot(mtcars, aes=list(y=mpg, x=factor(cyl)))
ggrect(p)
ggrect(p, list(fill=mpg))
pscontinuous(ggrect(p, list(fill=mpg), colour="black"), "y", range=c(0,NA))}
\keyword{hplot}
