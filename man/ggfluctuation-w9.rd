\name{ggfluctuation}
\alias{ggfluctuation}
\title{Fluctuation plot}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create a fluctuation plot.
}
\usage{ggfluctuation(table, type="size", floor=0, ceiling=max(table$freq, na.rm=TRUE))}
\arguments{
\item{table}{a table of values, or a data frame with three columns, the last column being frequency}
\item{type}{size, or colour to create traditional heatmap}
\item{floor}{don't display cells smaller than this value}
\item{ceiling}{}
}

\details{A fluctutation diagram is a graphical representation of a contingency
table.  This fuction currently only supports 2D contingency tabless
but extension to more should be relatively straightforward.

With the default size fluctuation diagram, area is proportional to the
count (length of sides proportional to sqrt(count))}

\examples{ggfluctuation(table(action=movies$Action, comedy=movies$Comedy))
ggfluctuation(table(action=movies$Action, rating=movies$mpaa))
ggfluctuation(table(action=movies$Action, comedy=movies$Comedy), type="colour")
ggfluctuation(table(warpbreaks[,c(1,3)]))}
\keyword{hplot}
