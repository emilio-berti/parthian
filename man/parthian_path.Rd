% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path-functions.R
\name{parthian_path}
\alias{parthian_path}
\title{Calculate least-cost path}
\usage{
parthian_path(g, r, from = NULL, to = NULL)
}
\arguments{
\item{g}{igraph object, the weighted graph of energy landscape.}

\item{r}{SpatRaster object, the energy landscape calculated using the
enerscape package.}

\item{from}{Numeric or SpatVector, origin location.}

\item{to}{Numeric or SpatVector, destination location.}
}
\value{
list with:

  - 'costs', the total costs of travel along the least-cost path.
  - 'lcp', the SpatVector (lines) of the least-cost path.
}
\description{
Calculate least-cost path
}
\details{
The parameters 'from' and 'to' can be a SpatVector with one point
each or a number. When they are numbers, it refers to the cell ID of the
energy landscape raster. The recommended usage it to pass 'from' and 'to' as
SpatVectors, unless this operation needs to be repeated several times.
}
