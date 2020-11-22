#' Count tips
#' Counts the number of tips that are children of the longest branch
#'
#' @param tree A phylo or multiphylo object.
#'
#' @return The number of tips that are children of the longest branch of a single tree (for a phylo object) and
#' a vector of the number of tips that are children of the longest branch of each tree (for a multiPhylo object).
#' @export
count_tips <- function(tree) {
  num <- vector(length = length(tree))
  if (class(tree) == "phylo") {
    long_branch <- which.max(tree$edge.length)
    branch_node <- tree$edge[long_branch,2]
    if (branch_node <= length(tree$tip.label)) {
      num <- 1
    } else {
      ind <- branch_node - length(tree$tip.label)
      num <- length(adephylo::listTips(tree)[[ind]])
    }
  } else if (class(tree) == "multiPhylo") {
    for (i in 1:length(tree)) {
      temp_tree <- tree[[i]]
      long_branch <- which.max(temp_tree$edge.length)
      branch_node <- temp_tree$edge[long_branch,2]
      if (branch_node <= length(temp_tree$tip.label)) {
        num[i] <- 1
      } else {
        ind <- branch_node - length(temp_tree$tip.label)
        num[i] <- length(adephylo::listTips(temp_tree)[[ind]])
      }
    }
  } else {num = 0}
  return(num)
}
