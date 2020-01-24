#' plot_modularity
#'
#' plots the modularity statistic against the number of clusters.
#'
#' @param mod modularity object
#'
#' @return a plot of modularity values calculated for a given range of clusters
#' @export
#' @examples 
#' data("quakes")
#' data("socioGrid")
#' modularity <- find_no_clusters(socioGrid, disjoint = TRUE, n.neigh = 6)
#' plot_modularity(modularity)

plot_modularity <- function(mod)
{
  mod %>%
    as.data.frame() %>%
    ggplot2::ggplot(ggplot2::aes(x = as.numeric(names(mod)), y = mod)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = which(mod == max(mod)) + 1) +
    ggplot2::xlab("number of clusters") +
    ggplot2::ylab("Modularity") +
    ggplot2::theme_light()
}
