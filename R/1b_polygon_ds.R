#' polygon_ds
#'  
#' Creates a vector of community assignment based on neighbouring polygons. It
#' creates a topological structure in which nodes represent polygons and the edge
#' is the similarity between nodes. Communities are created using fast greedy
#' algorithm that maximizes their modularity.
#' 
#' @param x point or polygon shapefile data;
#' @param k number of clusters;
#' @param n.neigh number of neighbours considered in the k-nearest neighbour
#'   algorithm that builds topology
#' @param data atributes of the spatial data frame to calculate similarity or
#'   distance measure;
#' @param method Character or function to declare distance method. If method is
#'   character, method must be "mahalanobis" or "euclidean", "maximum",
#'   "manhattan", "canberra", "binary" or "minkowisk". If method is one of
#'   "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowisk",
#'   see dist for details, because this function as used to compute the distance.
#'   If method="mahalanobis", the mahalanobis distance is computed between
#'   neighbour areas. If method is a function, this function is used to compute
#'   the distance.
#' @param style style can take values “W”, “B”, “C”, “U”, “minmax” and “S”
#' @param disjoint if default settings generate error occuring to disjoint
#'   subgraphs it means, that in some places points or polygons are to disjoint
#'   to generate one connected graph. Use disjoint = T to enforce that one graph
#'   will be created. This is a slower option.
#' @param plot should the neighbourhood be plotted
#' @param accuracy logical should accuracy be calculated based on randomForest algorithm
#' @param queen if TRUE, a single shared boundary point meets the contiguity condition,
#' if FALSE, more than one shared point is required; note that more than one shared boundary
#' point does not necessarily mean a shared boundary line
#' 
#' @return vector of numbers representing regions to whicheach element
#' @export
#' 
#' @examples
#' library(tmap)
#' data("World",package = "tmap")
#' World <- World[(!is.na(World$life_exp)), ]
#' modularity <- find_no_clusters(World, data = c(9, 10),
#'     disjoint = TRUE, n.neigh = 6)
#' plot_modularity(modularity)
#' World$class <- polygon_ds(World, data = c(9, 10), k = 7, style = "B",
#'     disjoint = TRUE, plot = TRUE, n.neigh = 6)
#' qtm(World, "class")

polygon_ds <- function(x,
                       k = 2,
                       queen = TRUE,
                       data = 2:ncol(x),
                       method = "euclidean",
                       style = "B",
                       disjoint = FALSE,
                       n.neigh = 8,
                       plot = TRUE,
                       accuracy = TRUE)
{
  #Prepare the polygons for further analysis by ckecking its class and converting to point neghbourhood representations 
  
  res <- prepare_polygons(
    x = x,
    queen = queen,
    method = method,
    disjoint = disjoint,
    n.neigh = n.neigh,
    plot = plot
  )

  fg.graph <-
    build_graph(
      x = res[["x"]],
      x.nb = res[["x.nb"]],
      data = data,
      method = method,
      style = style
    )

  classes <- part_communities(fg = fg.graph[["fg"]], k = k)
  if (accuracy == TRUE)
  {
    data.to.accu <-
      res[["x"]]@data %>%
      dplyr::select(data) %>%
      dplyr::mutate(class = classes)
    accu <- accuracy_ds(x = data.to.accu)
    print(paste("Accuracy:", accu))
  }
  classes
}
