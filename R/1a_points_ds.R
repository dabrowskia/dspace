#' points_ds
#'
#' Creates a vector of community assignment based on neighbouring points. It is based
#' on a topological network structure where points represent nodes and the edges
#' are the degree of similarity between those nodes. Communities are created using fast greedy
#' algorithm that maximizes their modularity.
#'
#' @param x point or polygon shapefile data;
#' @param k number of communities;
#' @param n.neigh number of neighbours considered in the k-nearest neighbour
#'   algorithm that builds topology network
#' @param data atributes of the spatial data frame to calculate similarity or
#'   distance measure;
#' @param method Character or function to declare distance method. If method is
#'   character, method must be "mahalanobis" or "euclidean", "maximum",
#'   "manhattan", "canberra", "binary" or "minkowisk". If method is one of
#'   "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowisk",
#'   see dist for details, because this function is used to compute the distance.
#'   If method is set to "mahalanobis", the mahalanobis distance is computed between
#'   neighbour points. If method is a function, this function is used to compute
#'   the distance.
#' @param style style can take values “W”, “B”, “C”, “U”, “minmax” and “S” (see spdep::nb2listw)
#' @param disjoint if default settings generate error occuring to disjoint
#'   subgraphs it means, that in some places points or polygons are to disjoint to
#'   generate one connected graph. Use disjoint = T to enforce that one graph will
#'   be created. This is a slower option.
#' @param plot should the neighbourhood be plotted
#' @param accuracy logical should accuracy be calculated based on randomForest algorithm
#' @param queen if TRUE, a single shared boundary point meets the contiguity condition,
#' if FALSE, more than one shared point is required; note that more than one shared boundary
#' point does not necessarily mean a shared boundary line
#' 
#' @return vector of numbers representing regions to whicheach element
#' 
#' @export
#' 
#' @examples
#' data("realEstate")
#' realEstate <- points_ds(realEstate,k = 5, )
#' \dontrun{
#' library(dismo)
#' pol <- voronoi(xy = as(realEstate,'Spatial'))
#' qtm(pol, "class")
#' }
#' 
points_ds <- function(x,
                      k = 2,
                      queen = TRUE, #is this needed?
                      data = -grep(names(x),pattern = '^geom'),
                      method = "euclidean",
                      style = "B",
                      disjoint = FALSE, #is this needed for points?
                      n.neigh = 8,
                      plot = TRUE,
                      accuracy = TRUE)
{
  #First step is to prepare the points for further analysis 
  res <- prepare_points(
    x = x,
    method = method,
    n.neigh = n.neigh,
    plot = plot
  )
  #Building network/grap representation out of neghbour representation
  fg <-
    build_graph(
      x = res[["x"]],
      x.nb = res[["x.nb"]],
      data = data,
      method = method,
      style = style
    )
  #Dividing the points based on their graph representation using fast greedy algorithm
  classes <- part_communities(fg = fg[["fg"]], k = k)
  #Calculating the accuracy based on random forest algorithm for evaluating suitability of the partition
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
