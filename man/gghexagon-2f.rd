\name{gghexagon}
\alias{gghexagon}
\title{Grob function: hexagons}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create hexagon binning of data points as created by Dan Carr.
}
\usage{gghexagon(plot = .PLOT, aesthetics=list(), ..., data=plot$data)}
\arguments{
\item{plot}{the plot object to modify}
\item{aesthetics}{named list of aesthetic mappings, see details for more information}
\item{...}{other options, see details for more information}
\item{data}{data source, if not specified the plot default will be used}
}

\details{This grob is useful for scatterplots with a lot of overplotting.  It bins the
region into hexagons, counts the number of points in each hexagonal bin and
then plots them.

Aesthetic mappings that this grob function understands:

\itemize{
\item x: x position (required)
\item y: y position (required)
}

These can be specified in the plot defaults (see \code{\link{ggplot}}) or
in the \code{aesthetics} argument.  If you want to modify the position
of the points or any axis options, you will need to add a position scale to
the plot.  These functions start with \code{ps}, eg.
\code{\link{pscontinuous}} or \code{\link{pscategorical}}

Other options:

\itemize{
\item xbins: number of bins to use
\item ...: other arguments passed to \code{\link[hexbin]{grid.hexagons}}
}}
\seealso{\code{\link[hexbin]{grid.hexagon}}, \code{\link[hexbin]{grob_2density}} for another way of dealing with overplotting}
\examples{m <- ggplot(movies, aesthetics=list(y=length, x=rating))
gghexagon(m)
gghexagon(m, xbins=50)
gghexagon(m, style="lattice")}
\keyword{hplot}
