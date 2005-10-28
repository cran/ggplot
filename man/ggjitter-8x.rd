\name{ggjitter}
\alias{ggjitter}
\title{Grob function: jittered points}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Add jittered points to a plot
}
\usage{ggjitter(plot = .PLOT, aesthetics=list(), ..., data=plot$data)}
\arguments{
\item{plot}{the plot object to modify}
\item{aesthetics}{named list of aesthetic mappings, see details for more information}
\item{...}{other options, see details for more information}
\item{data}{data source, if not specified the plot default will be used}
}

\details{This is useful when plotting points with a categorical axis so to
avoid overplotting.

Aesthetic mappings that this grob function understands:

\itemize{
\item x: x position (required)
\item y: y position (required)
\item size: size of the point, in mm (see \code{\link{scsize})}
\item shape: shape of the glyph used to draw the point (see \code{\link{scshape})}
\item colour: point colour (see \code{\link{sccolour})}
}

These can be specified in the plot defaults (see \code{\link{ggplot}}) or
in the \code{aesthetics} argument.  If you want to modify the position
of the points or any axis options, you will need to add a position scale to
the plot.  These functions start with \code{ps}, eg.
\code{\link{pscontinuous}} or \code{\link{pscategorical}}

Other options:

\itemize{
\item xjitter: degree of jitter in x direction, see \code{\link{jitter} for details, defaults to 1}
\item yjitter: degree of jitter in y direction, see \code{\link{jitter} for details, defaults to 0}
}}

\examples{p <- ggplot(movies, aes=list(x=mpaa, y=rating))
ggjitter(p)
ggboxplot(ggjitter(p))
ggboxplot(ggjitter(p), xjitter=2)
ggboxplot(ggjitter(p), yjitter=1)}
\keyword{hplot}
