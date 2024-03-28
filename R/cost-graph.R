#' @title Weighted network of travel costs
#' 
#' @param r SpatRaster of energy costs.
#' 
#' @description
#' Takes a raster of the energy landscape and builds the weighted network
#' with vertices the cells and edges connecting the 8 adjacent cells,
#' weighted by their cost of travel.
#' 
#' 
#' @return igraph object, the weighted graph of travel costs
cost_graph <- function(r) { 
  stopifnot(is(r, "SpatRaster"))
  m <- matrix(r, nrow = nrow(r), ncol = ncol(r), byrow = TRUE)
  d <- edgelist(m)
  d <- d[!is.na(d[, "to"]), ]
  d <- d[!is.na(d$to), ]
  # check size
  size <- nrow(d)
  expected_size <- length(m) * 8 - 2 * nrow(m) * 3 - 2 * ncol(m) * 3 + 4
  stopifnot(size == expected_size)
  g <- graph_from_edgelist(as.matrix(d[, c("from", "to")]))
  E(g)$weight <- d[["weight"]]
  g <- delete_edges(g, which(is.na(d[["weight"]])))
  # check graph is correct
  stopifnot(is_weighted(g))
  return (g)
}
