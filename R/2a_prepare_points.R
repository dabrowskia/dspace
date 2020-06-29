#' prepare_points
#'
#' Prepares points for regionalization - changes simple features to 
#' SpatialPolygonsDataFrame and calculates neighborhood objects
#'
#' @param x point object of class sf
#' @param method the distance/similarity to calculate
#' @param n.neigh at least how many neighbors should be taken into 
#' consideration
#' @param plot logical if TRUE a plot showing neighborhoods 
#' is being presented
#' @return neighborhoods for community finding
#'
prepare_points <- function(x,
                           method,
                           n.neigh,
                           plot)
{
  #Ensuring that points are of appropriate type and sf class
  if (!inherits(x, "sf")) stop("Error: Object is not of class 'sf'")
  
  geometry <- sf::st_geometry(x)
  
  if(!inherits(geometry, "sfc_POINT")) stop("Error: Object is not of type 'POINT'")
  
  #Converting to neighbor representation
  x.knn <- spdep::knearneigh(geometry, k = n.neigh)
  x.nb <- spdep::knn2nb(x.knn)
  gn <- spdep::gabrielneigh(geometry, nnmult = 3)
  g.nb <- spdep::graph2nb(gn)
  x.nb <- spdep::union.nb(x.nb, g.nb)

  #this should plot the neighborhoods
  if (plot == TRUE) {
    plot(geometry, lwd = 2)
    plot(x.nb, geometry, add = TRUE)
  }
  res <- list()
  res[["x.nb"]] <- x.nb
  res[["x"]] <- x
  res
}
