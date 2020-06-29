#' regionalize
#'
#' Creates a vector of community assignment based on neighboring data. It
#' creates a topological structure in which nodes represent points or centroids of polygons
#' and the edge represents the similarity between nodes.
#' Communities are created using fast greedy
#' algorithm that maximizes their modularity.
#'
#' @param x point or polygon shapefile data;
#' @param k number of clusters;
#' @param n.neigh number of neighbors considered in the k-nearest neighbour
#'   algorithm that builds topology
#' @param data attributes of the spatial data frame to calculate similarity or
#'   distance measure;
#' @param similarity.measure Character or function to declare distance method. If method is
#'   character, method must be "mahalanobis" or "euclidean", "maximum",
#'   "manhattan", "canberra", "binary" or "minkowski". If method is one of
#'   "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski",
#'   see dist for details, because this function as used to compute the distance.
#'   If method="mahalanobis", the mahalanobis distance is computed between
#'   neighbor areas. If method is a function, this function is used to compute
#'   the distance.
#' @param style style can take values “W”, “B”, “C”, “U”, “minmax” and “S”. 
#'   For more details see `?spdep::nb2listw`
#' @param disjoint if default settings generate error occurring to disjoint
#'   subgraphs it means, that in some places points or polygons are to disjoint
#'   to generate one connected graph. Use disjoint = T to enforce that one graph
#'   will be created. This is a slower option.
#' @param plot should the neighborhood be plotted
#' @param explain logical. If TRUE a machine learning (randomForest 
#' using 5 fold cross validation) model is being constructed based 
#' on the data provided for regionalization. The accuracy of this model
#' explains how much of the regionalization can be attributed to the data
#' and how much to the spatial distribution.
#' @param queen if TRUE, a single shared boundary point meets the contiguity condition,
#' if FALSE, more than one shared point is required; note that more than one shared boundary
#' point does not necessarily mean a shared boundary line
#'
#' @return vector of numbers representing regions to which each element
#' @export
#' @examples 
#' data("socioGrid")
#' modularity <- find_no_clusters(socioGrid, disjoint = TRUE, n.neigh = 6)
#' plot_modularity(modularity)
#' socioGrid$class <- regionalize(socioGrid, k = 7,
#'     disjoint = TRUE, plot = TRUE)
#'     
#' data("realEstate")
#' realEstate$class <- regionalize(realEstate, k = 5, explain = FALSE)
regionalize <- function(x,
                        k = 2,
                        data = -grep(names(x), pattern = '^geom'),
                        similarity.measure = "euclidean",
                        style = "B",
                        n.neigh = 8,
                        plot = TRUE,
                        queen = TRUE,
                        disjoint = FALSE,
                        explain = TRUE) {
  geometry <- sf::st_geometry(x)
  
  if(inherits(geometry, "sfc_POINT")){
    ds_points(
      x,
      k,
      data,
      similarity.measure,
      style,
      n.neigh,
      plot ,
      explain
    )
  } else if(inherits(geometry, "sfc_POLYGON")){
    ds_polygon(
      x,
      k,
      queen ,
      data ,
      similarity.measure,
      style,
      disjoint,
      n.neigh,
      plot,
      explain
    )
  }
}