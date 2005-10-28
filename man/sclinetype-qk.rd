\name{sclinetype}
\alias{sclinetype}
\title{Scale: line type}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create a scale for categorical line types.
}
\usage{sclinetype(plot = .PLOT)}
\arguments{
\item{plot}{plot to add scale to}
}

\details{This scale is automatically added to the plot when you use the linetype
aesthetic.  As there are no options to this scale, you shouldn't ever
need to add it yourself.}
\seealso{\code{\link{scale_categorical}}, \code{\link{map_linetype}}}
\examples{p <- ggplot(mtcars, aes=list(x=mpg, y=wt, linetype=cyl))
ggline(p)
ggline(sclinetype(p))}
\keyword{hplot}
