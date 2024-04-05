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
#'   - 'costs', the total costs of travel along the least-cost path.
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
  stopifnot(is(r, "SpatRaster"))
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
    xy <- extract(r, rbind(from, to), cells = TRUE)
    from <- xy[["cell"]][1]
    to <- xy[["cell"]][2]
  }
  lcp <- shortest_paths(g, from, to)$vpath[[1]]
  
  # to vector -------
  lcp <- xyFromCell(r, as.numeric(lcp))
  lcp <- vect(lcp, crs = crs(r))
  costs <- sum(extract(r, lcp, ID = FALSE)[, 1]) #fix enerscape to cast the class
  lcp <- as.lines(lcp)
  
  # return ----------
  return (list(
    lcp = lcp,
    costs = costs
  ))
}

#' @title Calculate least-cost paths
#'
#' @param g igraph object, the weighted graph of energy landscape.
#' @param r SpatRaster object, the energy landscape calculated using the
#'   enerscape package.
#' @param locations SpatVector, locations.
#'
#' @returns list with:
#'
#'   - 'costs', matrix of the total costs of travel along the least-cost paths.
#'   - 'lcps', the SpatVector (lines) of the least-cost paths.
#'
#' @details The parameters 'locations' must be a SpatVector with points
#'   geometry. These points are the locations between which least-cost paths
#'   will be calculated pairwise.
#' 
parthian_paths <- function(g, r, locations) {
  
  # checks ----
  stopifnot(is(g, "igraph"))
  stopifnot(is(r, "SpatRaster"))
  stopifnot(is(locations, "SpatVector"))
  stopifnot(geomtype(locations) == "points")
  
  # least cost path ----
  xy <- extract(r, locations, cells = TRUE)
  cells <- xy[["cell"]]
  
  costs <- matrix(0, nrow = nrow(xy), ncol = nrow(xy))
  lcps <- as.lines(vect(cbind(0, 0), crs = crs(r))) #dummy line
  for (i in seq(1, nrow(xy) - 1)) {
    for (j in seq(i + 1, nrow(xy))) {
        lcp <- parthian_path(g, r, cells[i], cells[j])
        costs[i, j] <- lcp$costs
        lcps <- rbind(lcps, lcp$lcp)
    }
  }
  costs <- costs + t(costs)
  lcps <- lcps[-1] #remove dummy lines
  
  # return ----------
  return (list(
    lcps = lcps,
    costs = costs
  ))
}
