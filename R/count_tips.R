#' Count tips
#' Counts the number of tips that are children of the longest branch
#'
#' @param tree A phylo object.
#'
#' @return The number of tips that are children of the longest branch.
#' @export
count_tips <- function(tree) {
  long_branch <- which.max(tree$edge.length)
  branch_node <- tree$edge[long_branch,2]
  if (branch_node <= length(tree$tip.label)) {
    num <- 1
  } else {
    ind <- branch_node - length(tree$tip.label)
    num <- length(adephylo::listTips(tree)[[ind]])
  }
  return(num)
}
