\name{gghistogram}
\alias{gghistogram}
\title{Grob function: histogram}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Draw a histogram
}
\usage{gghistogram(plot = .PLOT, aesthetics=list(), ..., data=plot$data)}
\arguments{
\item{plot}{the plot object to modify}
\item{aesthetics}{named list of aesthetic mappings, see details for more information}
\item{...}{other options, see details for more information}
\item{data}{data source, if not specified the plot default will be used}
}

\details{Aesthetic mappings that this grob function understands:

Conceptually, the histogram is one of the most complicated
of the grob functions, becuase it takes a 1D data set and makes
it two dimensional.  This necessitates an extra step, the \code{pre_histogram}
function which bins the data and returns the bins with their counts.
This data is then used my \code{grob_histogram}
to plot the points.

\itemize{
\item x: x position (required)
}

These can be specified in the plot defaults (see \code{\link{ggplot}}) or
in the \code{aesthetics} argument.  If you want to modify the position
of the points or any axis options, you will need to add a position scale to
the plot.  These functions start with \code{ps}, eg.
\code{\link{pscontinuous}} or \code{\link{pscategorical}}

Other options:

\itemize{
\item breaks: breaks argument passed to \code{\link{hist}}
\item scale: scale argument passed to \code{\link{hist}}
}}

\examples{m <- ggplot(movies, aesthetics=list(x=rating))
gghistogram(m)}
\keyword{hplot}
