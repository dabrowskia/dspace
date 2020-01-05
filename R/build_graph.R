
#' build_graph
#'
#' Creates fastgreedy community graph for extracting regions
#'
#' @param x polygon or point data from wich neighbourhood object will be created
#' @param data data frame to build weights between polygons/points
#' @param x.nb neighbourhood object created by prepare_points() or prepare_polygons()
#' @param method method to calculate similarity/distance betweem neighbouring points or polygons
#' @param style style of neighbourhood
#'
#' @return a list containing community object ("fg") and graph object ("graph")
#' @export
#'
build_graph <- function(x, data, x.nb,
                        method, style)
{
  lcosts <- nbcosts(method = method, x.nb, x@data[, data])
  nb.w <- nb2listw(x.nb, lcosts, style = style)
  t <- listw2mat(nb.w)
  colnames(t) <- rownames(t)
  mst.bh.df <- t %>% melt() %>% filter(value != 0)

  names(mst.bh.df) <- c('from', 'to', 'weight')
  PC1.graph <- graph_from_data_frame(mst.bh.df) %>% as.undirected()
  PC1.graph <-
    subgraph.edges(PC1.graph, which(E(PC1.graph)$weight != 0), delete.vertices = F)
  E(PC1.graph)$weight <-
    1 - ((E(PC1.graph)$weight - min(E(PC1.graph)$weight)) /
           (max(E(PC1.graph)$weight) - min(E(PC1.graph)$weight)))
  fg <- cluster_fast_greedy(PC1.graph)
  res <- list()
  res[["fg"]] <- fg
  res[["graph"]] <- PC1.graph
  res
}
