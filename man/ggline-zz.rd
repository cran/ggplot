\name{ggline}
\alias{ggline}
\title{Grob function: line}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Add a line to the plot
}
\usage{ggline(plot = .PLOT, aesthetics=list(), ..., data=plot$data)}
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
\item id: identifier variable used to break up into multiple paths
\item size: size of the line, in mm (see \code{\link{scsize}})
\item colour: line colour (see \code{\link{sccolour}})
\item line\_type: line style/type (see \code{\link{sclinetype}})
}

These can be specified in the plot defaults (see \code{\link{ggplot}}) or
in the \code{aesthetics} argument.  If you want to modify the position
of the points or any axis options, you will need to add a position scale to
the plot.  These functions start with \code{ps}, eg.
\code{\link{pscontinuous}} or \code{\link{pscategorical}}

Other options:

\itemize{
\item none
}}

\examples{mry <- do.call(rbind, by(movies, round(movies$rating), function(df) { 
nums <- tapply(df$length, df$year, length)
data.frame(rating=round(df$rating[1]), year = as.numeric(names(nums)), number=as.vector(nums))
}))
p <- ggplot(mry, aesthetics = list(x=year, y=number, id=rating))
ggpath(p)
ggpath(p, list(size=rating))
ggpath(p, list(colour=rating))}
\keyword{hplot}
