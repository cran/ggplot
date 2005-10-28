\name{labels_default}
\alias{labels_default}
\title{Default lables}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Generate default facet labels.
}
\usage{labels_default(plot)}
\arguments{
\item{plot}{plot object}
}
\value{gList containg text grobs with appropriate viewports}
\details{Facet labels are only displayed when there are facets in a particular
direction.  By default the labels consist of the variable name : value.
You can't currently change this display. but it will be an option in the near
future.}

\examples{}
\keyword{hplot}
\keyword{internal}
