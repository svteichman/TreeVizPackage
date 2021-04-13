#' Plot support
#' Plot a tree with support values on each internal branch.
#'
#' @param main_tree A tree to check support for.
#' @param n_trees The number of trees providing support for main_tree.
#' @param branch_support A list of branch support values for each internal branch of main_tree.
#' @param rooted A boolean, TRUE if the main tree is rooted
#' @param lab_size The size of the tip labels, default is 2.
#' @param supp_size The size of the support labels, default is 2.
#' @param xlim_max The length of the x axis.
#'
#' @return A ggtree object.
#'
#' @import ggtree
#'
#' @export
plot_support <- function(main_tree, n_trees, branch_support, rooted = TRUE,
                         lab_size = 2, supp_size = 2, xlim_max = 0.5) {
  n_tips <- length(main_tree$tip.label)
  num <- n_tips + 2
  if (!rooted) {num <- n_tips+1}
  support_val <- round(c(rep(n_trees,num),branch_support)/n_trees,2)
  support_lab <- paste0(support_val)
  plot <- main_tree %>%
    ggtree +
    geom_tiplab(size = lab_size) +
    geom_text2(aes(subset=!isTip, label=support_lab, color = support_val),
               hjust=1, vjust = -0.7, size = supp_size) +
    scale_colour_gradient(low = "red", high = "blue") +
    theme(legend.position = 'none') +
    ggplot2::xlim(0, xlim_max)
  return(plot)
}
