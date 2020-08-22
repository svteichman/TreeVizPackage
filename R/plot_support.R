#' Plot support
#' Plot a tree with support values on each internal branch.
#'
#' @param main_tree A tree to check support for.
#' @param n_trees The number of trees providing support for main_tree.
#' @param branch_support A list of branch support values for each internal branch of main_tree.
#'
#' @return A ggtree object.
#'
#' @import ggtree
#'
#' @export
plot_support <- function(main_tree, n_trees, branch_support) {
  n_tips <- length(main_tree$tip.label)
  support_val <- round(c(rep(n_trees,(n_tips + 2)),branch_support)/n_trees,2)
  support_lab <- paste0(support_val)
  plot <- main_tree %>%
    ggtree +
    geom_tiplab(size = 2) +
    geom_text2(aes(subset=!isTip, label=support_lab, color = support_val),
               hjust=1, vjust = -0.7, size = 2) +
    scale_colour_gradient(low = "red", high = "blue") +
    theme(legend.position = 'none')
  return(plot)
}
