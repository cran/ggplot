\name{.build_options}
\alias{.build_options}
\alias{ggopt}
\alias{theme_default}
\alias{theme_bw}
\title{Set ggplot options}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Set global options for ggplot.
}
\usage{.build_options(opt)}
\arguments{
\item{opt}{list of options to get/set}
}

\details{These are aliased into every plot object, so that \code{p$grid.col} will
return the default grid colour, unless it has been overriden for a particular
plot object.  You can change the global options using the function, or the
options for a specific plot by setting the values directly on the object.  See
the examples for more details.

Colour settings:

\itemize{
\item axis.colour: axis text and line colour ("black")
\item background.colour: background text colour ("black"), used for title
\item background.fill:   background fill ("white")
\item grid.colour: plot grid colour ("white")
\item grid.fill:   plot grid background fill ("grey90")
}

Strip settings

\itemize{
\item strip.text:   function with two arguments (variable, and value) used for
generating strip labels
\item strip.gp: graphic parameter settings for the strip
\item strip.text.gp:  graphic parameter settings for the strip text
}

Legend settings

\itemize{
\item legend.position:   position of legend: "none" to hide legend;
"left", "right", "top", "bottom", for positioning outside of plot;
c(x, y) for positioning on top of plot
}

Other settings:

\itemize{
\item aspect.ratio: aspect ratio of facets.  Set to \code{NULL} to allow
to vary with device size
}}

\examples{ggopt(background.fill = "black", background.color ="white") # all new plots will use this
p <- ggpoint(ggplot(tips, smoker ~ sex,aesthetics = list(y = tip, x = total_bill)))
p
p$background.fill = "white"
p
p$strip.text.gp <- gpar(col="red", fontsize=8)
p$background.colour <- "pink"
p$grid.colour <- "green"
p$grid.fill <- "blue"
p # a very ugly plot!
ggopt(background.fill = "white", background.color ="black")}
\keyword{manip}
