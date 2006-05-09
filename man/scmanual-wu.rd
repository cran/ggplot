\name{scmanual}
\alias{scmanual}
\title{Scale: manual}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create a manual scale
}
\usage{scmanual(plot = .PLOT, variable="x", name="", breaks=NULL, labels=as.character(breaks), grob=function(x) grob_point(x, unique=FALSE))}
\arguments{
\item{plot}{plot object to add scale to}
\item{variable}{variable to scale}
\item{name}{numeric vector of break points}
\item{breaks}{character vector of break labels}
\item{labels}{grob function to use when drawing legend}
\item{grob}{}
}

\details{This scale function allows you complete control over the
scale.}
\seealso{\code{\link{ggfluctuation}} for a use}
\examples{}
\keyword{hplot}
