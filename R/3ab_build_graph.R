#' build_graph
#'
#' Creates fastgreedy community graph for extracting regions
#'
#' @param x polygon or point data from witch neighborhood object will be created
#' @param data data frame to build weights between polygons/points
#' @param x.nb neighborhood object created by prepare_points() or prepare_polygons()
#' @param similarity.measure method to calculate similarity/distance between neighboring points or polygons
#' @param style style of neighborhood (see `spdep::nb2listw``)
#'
#' @return a list containing community object ("fg") and graph object ("graph")
#' 
build_graph <- function(x, data, x.nb,
                        similarity.measure, style)
{
  
  #Calculating similarity between the nodes
  cost <- spdep::nbcosts(method = similarity.measure, 
                         x.nb, 
                         sf::st_drop_geometry(x[, data]))
  nb.w <- spdep::nb2listw(x.nb, 
                          cost, 
                          style = style)
  mat <- spdep::listw2mat(nb.w)
  colnames(mat) <- rownames(mat)
  value <- NULL
  data.for.graph <- mat %>%
    reshape2::melt(value.name = "value") %>%
    dplyr::filter(value != 0)

  names(data.for.graph) <- c("from", "to", "weight")
  data.graph <- igraph::graph_from_data_frame(data.for.graph) %>%
    igraph::as.undirected()
  data.graph <-
    igraph::subgraph.edges(data.graph, which(igraph::E(data.graph)$weight != 0), delete.vertices = FALSE)
  igraph::E(data.graph)$weight <-
    1 - ((igraph::E(data.graph)$weight - min(igraph::E(data.graph)$weight)) /
           (max(igraph::E(data.graph)$weight) - min(igraph::E(data.graph)$weight)))
  fg <- igraph::cluster_fast_greedy(data.graph)
  res <- list()
  res[["fg"]] <- fg
  res[["graph"]] <- data.graph
  res
}

