
#' prepare_polygon
#'
#' Prepares polygons for regionalization - changes simplea features to SpatailPolygonsDataFrame an calculates neighbourhood objects
#'
#' @param x point object
#' @param queen logical should the wueen or the rook neighbourhood be calculated
#' @param method the distance/similarity to calculate
#' @param disjoint logical if polygons are not continuous
#' @param n.neigh at least how many neighbours should be taken into regionalization if disjoint==T
#' @param plot logical if TRUE a plot showing neighbourhoods is beeing presented
#'
#' @return neighbourhoods for coummunity finding
#'
prepare_polygons <- function(x, queen,
                             method,
                             disjoint, n.neigh, plot)
{
  in.class <- class(x)[1]
  if (in.class == 'sf')
  {
    x.type = class(x$geom)[1]
    x <- sf::as_Spatial(x)
  }
  if ((in.class == 'sf' &
       !x.type %in% c("sfc_MULTIPOLYGON", "sfc_POLYGON")) |
      !in.class %in% c("sf", 'SpatialPolygonsDataFrame'))
  {
    stop('x not a Polygon layer')
  }

  # x@data[,data]<-apply(x@data[,data],2,scale)
  coords <- centroid(x)
  x.nb <- poly2nb(x, queen = queen)
  if (disjoint == TRUE)
  {
    coords <- centroid(x)
    x.knn <- knearneigh(coords, k = n.neigh)
    x.nb <- knn2nb(x.knn)
    gn <- gabrielneigh(coords, nnmult = 3)
    g.nb <- graph2nb(gn)
    x.nb <- union.nb(x.nb, g.nb)
  }

  if (plot == TRUE)
  {
    plot(x)
    plot(x.nb, coords, add = TRUE)
  }
  res <- list()
  res[["x.nb"]] <- x.nb
  res[["x"]] <- x
  res
}
