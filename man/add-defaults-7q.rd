\name{add_defaults}
\alias{add_defaults}
\title{Add default scales.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Add default scales to a plot.
}
\usage{add_defaults(p = .PLOT, new)}
\arguments{
\item{p}{plot object, if not specified will use current plot}
\item{new}{character vector of needed scales to add, see \code{\link{scales}} for possible options}
}

\details{You shouldn't need to call this function yourself.  If you want to add a
scale to a plot, use \code{\link{add_scale}}.}

\examples{}
\keyword{hplot}
\keyword{internal}
