#' Compute logmap
#' Compute the logmap centered at one tree for all other trees.
#'
#' @param cons_path The file path to the tree to center the log map.
#' @param tree_paths A list of file paths to all other trees.
#' @param other_cons_path An optional other consensus tree.
#'
#' @return A matrix with log map positions of all trees.
#'
#' @export
compute_logmap <- function(cons_path, tree_paths, other_cons_path = NULL) {
  res <- system2('java',
                 args = c('-jar', 'inst/java/logmap.jar',
                          cons_path,
                          cons_path),
                 stdout = T)
  eval(parse(text=res[length(res)]))
  n <- length(tree_paths) # number of trees
  logMap_dists <- matrix(nrow = n, ncol = length(logMap))
  if (!is.null(other_cons_path)) {logMap_dists <- matrix(nrow = n+1, ncol = length(logMap))}
  logMap_dists[1,] <- logMap

  for (i in 2:n) {
    res <- system2('java',
                   args = c('-jar', 'inst/java/logmap.jar',
                            cons_path,
                            tree_paths[i-1]),
                   stdout = T)
    eval(parse(text=res[length(res)]))
    logMap_dists[i,] <- logMap
  }
  if (!is.null(other_cons_path)) {
    res <- system2('java',
                   args = c('-jar', 'code/logmap.jar',
                            cons_path,
                            other_cons_path),
                   stdout = T)
    eval(parse(text=res[length(res)]))
    logMap_dists[dim(logMap_dists)[1],] <- logMap
  }
  return(logMap_dists)
}
