% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add.KE_LAOC.R
\name{add.KE_LAOC}
\alias{add.KE_LAOC}
\title{add.KE_LAOC}
\usage{
add.KE_LAOC(g, use_KE_PD = FALSE)
}
\arguments{
\item{g}{AOP igraph object, with vertex attribute KE_KED (values "MIE", "KE", or "AO") [igraph object]}

\item{use_KE_PD}{Optional (Default=FALSE): Can use KE_PD ("origin" and "terminus") instead of KE_KED [boolean]}
}
\value{
Identical igraph object with new vertex attribute called KE_LAOC (linear AOP Occurence) [igraph object]
}
\description{
Counts the number of times a KE occurs in different linear AOP paths
}
\seealso{
Other KE Attribution: \code{\link{add.KE_PD}}
}
