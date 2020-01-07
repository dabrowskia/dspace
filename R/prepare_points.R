
#' prepare_points
#'
#' Prepares points for regionalization - changes simplea features to SpatailPolygonsDataFrame an calculates neighbourhood objects
#'
#' @param x point object
#' @param method the distance/similarity to calculate
#' @param n.neigh at least how many neighbours should be taken into consideration
#' @param plot logical if TRUE a plot showing neighbourhoods is beeing presented
#'
#' @return neighbourhoods for coummunity finding
#' @export
#'
prepare_points <- function(x,
                           method = "euclidean",
                           n.neigh = 8,
                           plot = TRUE)
{
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

  # x@data[,data]<-apply(x@data[,data],2,scale)
  coords <- sp::coordinates(x)
  x.knn <- knearneigh(coords, k = n.neigh)
  x.nb <- knn2nb(x.knn)
  gn <- gabrielneigh(coords, nnmult = 3)
  g.nb <- graph2nb(gn)
  x.nb <- union.nb(x.nb, g.nb)


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
