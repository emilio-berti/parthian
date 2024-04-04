#' @title Calculate least-cost path
#'
#' @param g igraph object, the weighted graph of energy landscape.
#' @param r SpatRaster object, the energy landscape calculated using the
#'   enerscape package.
#' @param from Numeric or SpatVector, origin location.
#' @param to Numeric or SpatVector, destination location.
#' 
#' @returns list with:
#' 
#'   - 'costs', the total costs of travel along the least-cost path
#'   - 'lcp', the SpatVector (lines) of the least-cost path.
#'
#' @details The parameters 'from' and 'to' can be a SpatVector with one point
#' each or a number. When they are numbers, it refers to the cell ID of the
#' energy landscape raster. The recommended usage it to pass 'from' and 'to' as
#' SpatVectors, unless this operation needs to be repeated several times.
#' 
parthian_path <- function(g, r, from = NULL, to = NULL) {
  
  # checks ----
  stopifnot(is(g, "igraph"))
  if (is.null(from) || is.null(to)) {
    stop("'from' and 'to' must be specified.")
  }
  stopifnot(is(from, "SpatVector") || is(from, "numeric"))
  stopifnot(is(to, "SpatVector") || is(to, "numeric"))
  stopifnot(length(from) == 1)
  stopifnot(length(to) == 1)
  stopifnot(is(from, class(to)))
  
  # least cost path ----
  if (is(from, "SpatVector")) {
    xy <- geom(rbind(from, to))[, c("x", "y")]
    from <- xy[["cell"]][1]
    to <- xy[["cell"]][2]
  }
  lcp <- shortest_paths(g, from, to)$vpath[[1]]
  
  # to vector -------
  lcp <- xyFromCell(r, as.numeric(lcp))
  lcp <- vect(lcp, crs = crs(r))
  costs <- sum(extract(r, xy)[["EnergyScape"]]) #fix enerscape to cast the class
  lcp <- as.lines(lcp)
  
  # return ----------
  return (list(
    lcp = lcp,
    costs = costs
  ))
}