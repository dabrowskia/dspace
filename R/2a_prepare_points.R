#' prepare_points
#'
#' Prepares points for regionalization - changes simples features to SpatialPolygonsDataFrame and calculates neighbourhood objects
#'
#' @param x point object of class sf
#' @param method the distance/similarity to calculate
#' @param n.neigh at least how many neighbours should be taken into consideration
#' @param plot logical if TRUE a plot showing neighbourhoods is beeing presented
#' @return neighbourhoods for coummunity finding
#'
prepare_points <- function(x,
                           method,
                           n.neigh,
                           plot)
{
  #Ensuring that points are of appropraite type and sf class
  if (!inherits(x, "sf")) stop("object is not of class 'sf'")
  
  geometry <- sf::st_geometry(x)
  
  if(!inherits(geometry, "sfc_POINT")) stop("object is not of type 'POINT'")
  
  #Convering to neghbour representation
  x.knn <- spdep::knearneigh(geometry, k = n.neigh)
  # x.knn <- spdep::knearneigh(x$geometry, k = n.neigh)
  x.nb <- spdep::knn2nb(x.knn)
  gn <- spdep::gabrielneigh(geometry, nnmult = 3)
  g.nb <- spdep::graph2nb(gn)
  x.nb <- spdep::union.nb(x.nb, g.nb)

  #this should plot the neighbourhoods
  if (plot == TRUE) {
    plot(geometry, lwd = 2)
    plot(x.nb, geometry, add = TRUE)
  }
  res <- list()
  res[["x.nb"]] <- x.nb
  res[["x"]] <- x
  res
}
