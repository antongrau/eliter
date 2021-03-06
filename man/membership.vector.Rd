% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variables.R
\name{membership.vector}
\alias{membership.vector}
\title{A vector of memberships for each individual}
\usage{
membership.vector(x, den)
}
\arguments{
\item{x}{a character vector with names of individuals}

\item{den}{}
}
\value{
a vector
}
\description{
A vector of memberships for each individual
}
\examples{
data(den)
den.culture   <- has.tags(den, "Culture", result = "den")
graph.culture <- elite.network(den.culture, result = "ind")
membership.vector(V(graph.culture)$name, den)
}
