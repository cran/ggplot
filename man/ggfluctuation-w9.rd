\name{ggfluctuation}
\alias{ggfluctuation}
\title{Create a fluctuation diagram.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{

}
\usage{ggfluctuation(table, type="size")}
\arguments{
\item{table}{a table of values, or a data frame with three columns, the last column being frequency}
\item{type}{size, or colour to create traditional heatmap}
}

\details{A fluctutation diagram is a graphical representation of a contingency
table.  This fuction currently only supports 2D contingency tabless
but extension to more should be relatively straightforward.

With the default size fluctuation diagram, area is proportional to the
count (length of sides proportional to sqrt(count))}

\examples{ggfluctuation(table(warpbreaks[,c(2,3)]))
ggfluctuation(table(warpbreaks[,c(1,3)]))}
\keyword{hplot}
