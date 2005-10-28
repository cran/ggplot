\name{ggabline}
\alias{ggabline}
\title{Grob function: abline}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Add line specified by slope and intercept to a plot
}
\usage{ggabline(plot = .PLOT, aesthetics=list(), ..., data=plot$data)}
\arguments{
\item{plot}{the plot object to modify}
\item{aesthetics}{named list of aesthetic mappings, see details for more information}
\item{...}{other options, see details for more information}
\item{data}{data source, if not specified the plot default will be used}
}

\details{Aesthetic mappings that this grob function understands:

\itemize{
\item none
}

Other options:

\itemize{
\item intercept: intercept(s) of line
\item slope: slope(s) of line
\item colour: line colour
\item size: line thickness, in mm
}}

\examples{p <- ggplot(mtcars, aesthetics=list(x = wt, y=mpg))
ggabline(ggpoint(p), intercept=30, slope=-5)
ggabline(ggpoint(p), intercept=c(30,40,50), slope=-5)
ggsmooth(ggpoint(p), method=lm, formula=y~x) }
\keyword{hplot}
