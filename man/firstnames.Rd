% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variables.R
\name{firstnames}
\alias{firstnames}
\title{Extract first names}
\usage{
firstnames(x)
}
\arguments{
\item{x}{a character vector of full nmaes}
}
\value{
a character vector of first names
}
\description{
Extract first names from full names
}
\examples{
data(den)
first          <- table(firstnames(den$NAME))
head(sort(first, decreasing = TRUE), 20)
}
