\name{prettyplot}
\alias{prettyplot}
\title{Pretty plot}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Build a plot with all the usual bits and pieces.
}
\usage{prettyplot(plot, plotgrob)}
\arguments{
\item{plot}{plot}
\item{plotgrob}{title (character vector)}
\item{}{x axis label (character vector)}
\item{}{y axis label (character vector)}
\item{}{legend grobs (list of grobs)}
}

\details{As well as the plotting area, a plot need:
\item main title
\item x and y axis labels
\item space for legends (currently on the right hand side)

This function sets up the appropriate viewports and packs the
various components in.  The viewport is set up so that each component
will only take up the amount of space that it requires.}

\examples{}
\keyword{hplot}
