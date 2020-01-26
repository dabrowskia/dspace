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

#' @param plot should the neighbourhood be plotted
#' @param accuracy logical should accuracy be calculated based on randomForest algorithm
#' 
#' @return vector of numbers representing regions to whicheach element
#' 
#' @export
#' 
#' @examples 
#' data("realEstate")
#' realEstate$class <- points_ds(realEstate, k = 5, accuracy = FALSE)

points_ds <- function(x,
                      k = 2,
                      data = -grep(names(x), pattern = '^geom'),
                      method = "euclidean",
                      style = "B",
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
  #Building network/graph representation out of neghbour representation
  fg <- build_graph(
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
      sf::st_drop_geometry(res[["x"]]) %>%
      dplyr::select(data) %>%
      dplyr::mutate(class = classes)
    accu <- accuracy_ds(x = data.to.accu)
    print(paste("Accuracy:", accu))
  }
  classes
}
