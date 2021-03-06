% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/k-circles.R
\name{k.circles}
\alias{k.circles}
\title{K-circles decomposition}
\usage{
k.circles(incidence, minimum.memberships = 3, check.for.nested = TRUE)
}
\arguments{
\item{incidence}{a sparse incidence matrix}

\item{minimum.memberships}{the minimum number of memberships for the individuals (rows)}
}
\value{
an object of class "k.circles"
}
\description{
K-circles decomposition
}
\examples{
data(den)
den         <- den[den$SOURCE != "Events",]
incidence   <- xtabs(~NAME + AFFILIATION, droplevels(den), sparse = TRUE)
l.inc       <- k.circles(incidence, 3, check.for.nested = TRUE)
level.membership(l.inc)
l.inc[[5]] \%>\% colSums() \%>\% sort() \%>\% as.matrix()
l.inc[[5]] \%>\% rowSums() \%>\% sort() \%>\% as.matrix()
}
