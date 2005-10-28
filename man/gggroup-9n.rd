\name{gggroup}
\alias{gggroup}
\title{Grob function: groups}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create multiple of grobs based on id aesthetic.
}
\usage{gggroup(plot = .PLOT, aesthetics=list(), ..., data=plot$data)}
\arguments{
\item{plot}{the plot object to modify}
\item{aesthetics}{named list of aesthetic mappings, see details for more information}
\item{...}{other options, see details for more information}
\item{data}{data source, if not specified the plot default will be used}
}

\details{This grob function provides a general means of creating
multiple grobs based on groups in the data.  This is useful
if you want to fit a separate smoother for each group in the data.

You will need an id variable in your aesthetics list with determines
how the data is broken down.

Aesthetic mappings that this grob function understands:

\itemize{
\item x: x position (required)
\item y: y position (required)
\item id:
\item any other grobs used by the grob function you choose
}

These can be specified in the plot defaults (see \code{\link{ggplot}}) or
in the \code{aesthetics} argument.  If you want to modify the position
of the points or any axis options, you will need to add a position scale to
the plot.  These functions start with \code{ps}, eg.
\code{\link{pscontinuous}} or \code{\link{pscategorical}}

Other options:

\itemize{
\item grob: grob function to use for subgroups
\item anything else used by the grob function you choose
}}

\examples{p <- ggplot(mtcars, aesthetics=list(y=wt, x=qsec, id=cyl, colour=cyl))
gggroup(p)
gggroup(ggpoint(p), grob=grob_smooth, se=FALSE, span=1)
gggroup(ggpoint(p), aes=list(id=cyl, size=cyl), grob=grob_smooth, span=1)}
\keyword{hplot}
