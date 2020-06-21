#' part_communities
#'
#' Divides graph into defined number of regions
#'
#' @param k number of clusters to create regionalization
#' @param fg.graph hierarchical community object created from `build_graph()`
#'
#' @return a vector of classes - numbers of regions that particular 
#' polygon or point are classified to
part_communities <- function(k, fg.graph)
{
  classes <- igraph::cutat(fg.graph, no = k)
  membership <- igraph::membership(fg.graph)
  membership <- data.frame(ID = as.numeric(names(igraph::membership(fg.graph))),
                            class = classes)
  membership <- membership[order(membership$ID), ]
  membership$class

}
