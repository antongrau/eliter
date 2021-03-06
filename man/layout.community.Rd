% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layout.R
\name{layout.community}
\alias{layout.community}
\title{Create a network layout with community clusters}
\usage{
layout.community(graph, community, strength = 2, ...)
}
\arguments{
\item{graph}{an igraph object}

\item{community}{a vector with commmunity memberships}

\item{strength}{the strength of attraction between community members}
}
\value{
a matrix with coordinates
}
\description{
Create a network layout with community clusters
}
\examples{
data(den)
den.culture     <- has.tags(den, "Culture", result = "den")
graph.culture   <- elite.network(den.culture)
com             <- membership(walktrap.community(graph.culture))
lay             <- layout.community(graph.culture, com, 2, cloud.size = 1.5)
graph.plot(graph.culture, layout = lay, vertex.fill = as.factor(com)) + scale_fill_discrete(guide = "none")
}
