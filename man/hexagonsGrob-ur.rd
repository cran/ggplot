\name{hexagonsGrob}
\alias{hexagonsGrob}
\alias{hexpolygonGrob}
\title{Hexgaons Grob}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Minor changes to functions from hexbin to allow output directly to grobs.
}
\usage{hexagonsGrob(dat, style = c("colorscale", "centroids", "lattice",    "nested.lattice", "nested.centroids", "constant.col"), use.count = TRUE, cell.at = NULL, minarea = 0.05, maxarea = 0.8, check.erosion = TRUE, mincnt = 1, maxcnt = max(dat@count), trans = NULL, colorcut = seq(0, 1, length = 17), density = NULL, border = NULL, pen = NULL,colramp = function(n) {LinGray(n, beg = 90, end = 15)}, def.unit = "native", verbose = getOption("verbose"))}
\arguments{
\item{dat}{}
\item{style}{}
\item{use.count}{}
\item{cell.at}{}
\item{minarea}{}
\item{maxarea}{}
\item{check.erosion}{}
\item{mincnt}{}
\item{maxcnt}{}
\item{trans}{}
\item{colorcut}{}
\item{density}{}
\item{border}{}
\item{pen}{}
\item{colramp}{}
\item{def.unit}{}
\item{verbose}{}
}

\details{}

\examples{}
\keyword{hplot}
\keyword{internal}
