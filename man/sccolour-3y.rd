\name{sccolour}
\alias{sccolour}
\alias{sccolor}
\alias{scfill}
\title{Scale: categorical colour}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create a scale for categorical colours.
}
\usage{sccolour(plot = .PLOT, name="", h=c(0,270), l=60, c=90)}
\arguments{
\item{plot}{plot to add scale to}
\item{name}{Color Brewer palette to use, see \code{\link[RColorBrewer]{brewer.pal}} for details.  Note that palette type is chosen automatically.}
\item{h}{}
\item{l}{}
\item{c}{}
}

\details{Continuous variables will automatically be converted to categorical
using \code{\link{chop_auto}}.  You may want to use \code{\link{chop}}
to convert the values yourself for finer control.

This scale is automatically added when you have colour in your list of
aesthetics.  For finer control, you will need to set the scale
yourself.  See the example for some ideas.}
\seealso{\code{\link{scale_categorical}}, \code{\link{map_colour}}}
\examples{p <- ggplot(movies, aes=list(x=mpaa, y=rating))
ggjitter(p, list(colour=rating))
ggjitter(p, list(colour=length))
ggjitter(p, list(colour=chop(length)))
ggjitter(p, list(colour=chop(length,3)))
sccolour(ggjitter(p, list(colour=chop(length,3))), 2)}
\keyword{hplot}
