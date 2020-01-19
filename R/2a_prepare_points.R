#' prepare_points
#'
#' Prepares points for regionalization - changes simples features to SpatialPolygonsDataFrame and calculates neighbourhood objects
#'
#' @param x point object
#' @param method the distance/similarity to calculate
#' @param n.neigh at least how many neighbours should be taken into consideration
#' @param plot logical if TRUE a plot showing neighbourhoods is beeing presented
#'
#' @return neighbourhoods for coummunity finding
#'
prepare_points <- function(x,
                           method = "euclidean",
                           n.neigh = 8,
                           plot = TRUE)
{
  #Ensuring that points are of appropraite type and sp class
  in.class <- class(x)[1]
  if (in.class == 'sf')
  {
    x.type = class(x$geom)[1]
    x <- sf::as_Spatial(x)
  }
  if(exists('x.type')) {
    if ((in.class == 'sf' &
         !x.type %in% c("sfc_MULTIPOINT", "sfc_POINT")) |
        !in.class %in% c("sf", 'SpatialPointsDataFrame'))
    {
      stop('x not a point layer')
    }
  }

  #Convering to neghbour representation
  coords <- sp::coordinates(x)
  x.knn <- spdep::knearneigh(coords, k = n.neigh)
  x.nb <- spdep::knn2nb(x.knn)
  gn <- spdep::gabrielneigh(coords, nnmult = 3)
  g.nb <- spdep::graph2nb(gn)
  x.nb <- spdep::union.nb(x.nb, g.nb)


  if (plot == TRUE)
  {
    plot(x, lwd = 2)
    plot(x.nb, coords, add = TRUE)
  }
  res <- list()
  res[["x.nb"]] <- x.nb
  res[["x"]] <- x
  res
}
