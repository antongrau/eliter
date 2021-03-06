% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core identification.R
\name{vertex.communities}
\alias{vertex.communities}
\title{Vertex communities}
\usage{
vertex.communities(graph, weight = 1/E(graph)$weight)
}
\arguments{
\item{graph}{a weighted graph from \link{igraph}}

\item{weight}{the edge weights. Larger values are stronger ties.}
}
\value{
a data.frame with community memberships
}
\description{
Memberships for several community detection algorithms and a meta community structure based on all communities.
}
\examples{
data(den)
den.corp        <- has.tags(den, "Corporation", res = "relations", silent = TRUE)
graph.corp      <- elite.network.org(den.corp)
graph.corp      <- as.undirected(graph.corp)
vc              <- vertex.communities(graph.corp)
head(vc)
table(table(vc$"Meta louvain"))
}
