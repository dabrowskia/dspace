
#' plot_modularity
#'
#' plots the modularity statistic against the number of clusters.
#'
#' @param mod modularity object
#'
#' @return a plot of modularity values calculated for a given range of clusters
#' @export
#'
plot_modularity<-function(mod)
{
  mod %>%as.data.frame() %>%  ggplot(aes(x=as.numeric(names(mod)),y=mod))+
    geom_point()+
    geom_vline(xintercept = which(mod==max(mod))+1)+
    xlab('number of clusters')+
    ylab("Modularity")+
    theme_light()
}
