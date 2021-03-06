% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edges.from.path.R
\name{edges.from.path}
\alias{edges.from.path}
\title{edges.from.path}
\usage{
edges.from.path(x, by.vertex.name = TRUE)
}
\arguments{
\item{x}{A vector that lists the sequence of vertices in a path (from start to end if a directed path)  [vector]}

\item{by.vertex.name}{Optional (Default=TRUE) [boolean]}
}
\value{
a 2-column matrix, each row representing the start and end vetices of an edge [array]
}
\description{
Creates an edge list from input path
}
\seealso{
Other Path Operations: \code{\link{AOPN.short.path.color}}
}
