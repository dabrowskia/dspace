
#' build_graph
#'
#' Creates fastgreedy community graph for extracting regions
#'
#' @param x polygon or point data from wich neighbourhood object will be created
#' @param data data frame to build weights between polygons/points
#' @param x.nb neighbourhood object created by prepare_points() or prepare_polygons()
#' @param method method to calculate similarity/distance betweem neighbouring points or polygons
#' @param style style of neighbourhood (see spdep::nb2listw)
#'
#' @return a list containing community object ("fg") and graph object ("graph")
#' 
#' 
build_graph <- function(x, data, x.nb,
                        method, style)
{
  #Calculating similarity between the nodes
  lcosts <- nbcosts(method = method, x.nb, x@data[, data])
  nb.w <- nb2listw(x.nb, lcosts, style = style)
  t <- listw2mat(nb.w)
  colnames(t) <- rownames(t)
  value <- NULL
  data.for.graph <- t %>%
    melt(value.name = "value") %>%
    dplyr::filter(value != 0)

  names(data.for.graph) <- c('from', 'to', 'weight')
  data.graph <- graph_from_data_frame(data.for.graph) %>% as.undirected()
  data.graph <-
    subgraph.edges(data.graph, which(E(data.graph)$weight != 0), delete.vertices = F)
  E(data.graph)$weight <-
    1 - ((E(data.graph)$weight - min(E(data.graph)$weight)) /
           (max(E(data.graph)$weight) - min(E(data.graph)$weight)))
  fg <- cluster_fast_greedy(data.graph)
  res <- list()
  res[["fg"]] <- fg
  res[["graph"]] <- data.graph
  res
}