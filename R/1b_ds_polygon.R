#' ds_polygon
#'  
#' Creates a vector of community assignment based on neighboring polygons. It
#' creates a topological structure in which nodes represent polygons and the edge
#' is the similarity between nodes. Communities are created using fast greedy
#' algorithm that maximizes their modularity.
#' 
#' @param x point or polygon shapefile data;
#' @param k number of clusters;
#' @param n.neigh number of neighbors considered in the k-nearest neighbor
#'   algorithm that builds topology
#' @param data attributes of the spatial data frame to calculate similarity or
#'   distance measure;
#' @param similarity.measure Character or function to declare distance 
#'   method transformed into similarity measure. If method is
#'   character, method must be "mahalanobis" or "euclidean", "maximum",
#'   "manhattan", "canberra", "binary" or "minkowski". If method is one of
#'   "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski",
#'   see dist for details, because this function as used to compute the distance.
#'   If method="mahalanobis", the mahalanobis distance is computed between
#'   neighbor areas. If method is a function, this function is used to compute
#'   the distance.
#' @param style style can take values “W”, “B”, “C”, “U”, “minmax” and “S”
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
#' @return vector of numbers representing regions to which each element belongs to



ds_polygon <- function(x,
                       k = 2,
                       queen = TRUE,
                       data = -grep(names(x), pattern = '^geom'),
                       similarity.measure = "euclidean",
                       style = "B",
                       disjoint = FALSE,
                       n.neigh = 8,
                       plot = TRUE,
                       explain = TRUE)
{
  #Prepare the polygons for further analysis by ckecking its class and converting to point neghbourhood representations 
  
  res <- prepare_polygons(
    x = x,
    queen = queen,
    disjoint = disjoint,
    n.neigh = n.neigh,
    plot = plot
  )

  fg.graph <-
    build_graph(
      x = res[["x"]],
      x.nb = res[["x.nb"]],
      data = data,
      similarity.measure = similarity.measure,
      style = style
    )

  classes <- part_communities(fg.graph = fg.graph[["fg"]], k = k)
  if (explain == TRUE)
  {
    data <- names(x)[data]
    data.to.accu <-
      sf::st_set_geometry(res[["x"]], NULL) %>%
      dplyr::select(data) %>%
      dplyr::mutate(class = classes)
    accu <- ds_accuracy(data.to.accu = data.to.accu)
    print(paste(accu*100, 
                'percent of the regionalization process', 
                'can be attributed to the data itself while', 
                'the rest is due to spatial location (neghborhoods)'))
  }
  classes
}
