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
  #Ensuring that points are of appropraite type and sp class
  in.class <- class(x)[1]
  if (in.class != "sf") stop("object is not of class 'sf'")
  
  x.type <-class(x$geometry)[1]
  if(x.type != "sfc_POINT") stop("object is not of type 'POINT'")
  
  #Convering to neghbour representation
  coords <- x$geometry
  x.knn <- spdep::knearneigh(coords, k = n.neigh)
  x.knn <- spdep::knearneigh(x$geometry, k = n.neigh)
  x.nb <- spdep::knn2nb(x.knn)
  gn <- spdep::gabrielneigh(x$geometry, nnmult = 3)
  g.nb <- spdep::graph2nb(gn)
  x.nb <- spdep::union.nb(x.nb, g.nb)


  if (plot == TRUE) #this should plot the neighbourhoods
  {
    plot(x[1], lwd = 2)
    plot(x.nb, x$geometry, add = TRUE)
  }
  res <- list()
  res[["x.nb"]] <- x.nb
  res[["x"]] <- x
  res
}
