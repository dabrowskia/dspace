#' prepare_polygon
#'
#' Prepares polygons for regionalization - calculates 
#' neighborhood objects
#'
#' @param x point object
#' @param queen logical should the queen or the rook neighborhood be calculated
#' @param disjoint logical if polygons are not continuous
#' @param n.neigh number of nearest neighbors that should be taken into 
#' consideration for building graph if disjoint==TRUE
#' @param plot logical if TRUE a plot showing neighborhoods is being presented
#' @return neighborhoods for community finding
#'
prepare_polygons <- function(x, queen,
                             disjoint, n.neigh, plot)
{
  #Ensuring that polygons are of appropriate type and sf class
  
  if (!inherits(x, "sf")) stop("Error: Object is not of class 'sf'")
  
  geometry <- sf::st_geometry(x)
  
  if(!inherits(geometry, "sfc_POLYGON")) stop("Error: Object is not of type 'POLYGON'")
  
  options(warn=-1)
  coords <- sf::st_centroid(x)$geometry
  options(warn=0)
  x.nb <- spdep::poly2nb(x, queen = queen)
  if (disjoint == TRUE)
  {
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
