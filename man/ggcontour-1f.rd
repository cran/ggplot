\name{ggcontour}
\alias{ggcontour}
\title{Grob function: contours}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create a grob to display contours of a 3D data set.
}
\usage{ggcontour(plot = .PLOT, aesthetics=list(), ..., data=plot$data)}
\arguments{
\item{plot}{the plot object to modify}
\item{aesthetics}{named list of aesthetic mappings, see details for more information}
\item{...}{other options, see details for more information}
\item{data}{data source, if not specified the plot default will be used}
}

\details{Aesthetic mappings that this grob function understands:

\itemize{
\item x: x position (required)
\item y: y position (required)
\item z: z position (required)
}

These can be specified in the plot defaults (see \code{\link{ggplot}}) or
in the \code{aesthetics} argument.  If you want to modify the position
of the points or any axis options, you will need to add a position scale to
the plot.  These functions start with \code{ps}, eg.
\code{\link{pscontinuous}} or \code{\link{pscategorical}}

Other options:

\itemize{
\item nlevels: number of contours to draw
\item levels: contour positions
}}
\seealso{\code{\link{gg2density}}}
\examples{volcano3d <- data.frame(expand.grid(x = 1:nrow(volcano), y=1:ncol(volcano)), z=as.vector(volcano))
p <- ggplot(volcano3d, aesthetics=list(x=x,y=y,z=z))
ggcontour(p)
ggcontour(p, nlevels=3)
ggcontour(ggtile(p, list(colour=z)))}
\keyword{hplot}
