#' prepare_polygon
#'
#' Prepares polygons for regionalization - changes simple features to SpatialPolygonsDataFrame and calculates neighbourhood objects
#'
#' @param x point object
#' @param queen logical should the wueen or the rook neighbourhood be calculated
#' @param method the distance/similarity to calculate
#' @param disjoint logical if polygons are not continuous
#' @param n.neigh at least how many neighbours should be taken into regionalization if disjoint==TRUE
#' @param plot logical if TRUE a plot showing neighbourhoods is beeing presented
#' @return neighbourhoods for coummunity finding
#'
prepare_polygons <- function(x, queen,
                             method,
                             disjoint, n.neigh, plot)
{
  geometry <- sf::st_geometry(x)
  if(!inherits(geometry, "sfc_POLYGON")) stop("object is not of type 'POLYGON'")
  
  options(warn=-1)
  coords <- sf::st_centroid(x)$geometry
  options(warn=0)
  x.nb <- spdep::poly2nb(x, queen = queen)
  if (disjoint == TRUE)
  {
    # coords <- geosphere::centroid(x)
    x.knn <- spdep::knearneigh(coords, k = n.neigh)
    x.nb <- spdep::knn2nb(x.knn)
    gn <- spdep::gabrielneigh(coords, nnmult = 3)
    g.nb <- spdep::graph2nb(gn)
    x.nb <- spdep::union.nb(x.nb, g.nb)
  }

  
  if (plot == TRUE)
  {
    plot(x[1])
    plot(x.nb, coords, add = TRUE)
  }
  res <- list()
  res[["x.nb"]] <- x.nb
  res[["x"]] <- x
  res
}
