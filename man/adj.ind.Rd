% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph creation.R
\name{adj.ind}
\alias{adj.ind}
\title{Adjacency matrix for individuals}
\usage{
adj.ind(den)
}
\arguments{
\item{den}{an affiliation list}
}
\value{
a sparse adjacency matrix of individual * individual
}
\description{
Create an adjacency matrix from an affilation list
}
\examples{
data(den)
den.culture   <- has.tags(den, "Culture", result = "den")
adj.ind(den.culture)
}
