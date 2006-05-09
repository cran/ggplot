\name{scshape}
\alias{scshape}
\title{Scale: shape}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create a scale for categorical shapes.
}
\usage{scshape(plot = .PLOT, name="", solid=TRUE)}
\arguments{
\item{plot}{plot to add scale to}
\item{name}{should points be solid or hollow?}
\item{solid}{}
}

\details{This scale is automatically added when you use the shape aesthetic
mapping.  By using this scale you can explicitly decide whether the
points used should be hollow or solid.}
\seealso{\code{\link{scale_categorical}}, \code{\link{map_shape}}}
\examples{p <- ggplot(mtcars, aes=list(x=mpg, y=wt, shape=cyl))
ggpoint(p)
ggpoint(scshape(p, FALSE))}
\keyword{hplot}
