#' Compute logmap
#' Compute the logmap centered at one tree for all other trees.
#'
#' @param cons_path The file path to the tree to center the log map.
#' @param tree_paths A list of file paths to all other trees.
#' @param jar_path A file path to the jar file to compute the log map.
#' @param other_cons_path An optional other consensus tree.
#'
#' @return A matrix with log map positions of all trees.
#'
#' @export
compute_logmap <- function(cons_path, tree_paths, jar_path, other_cons_path = NULL) {
  res <- system2('java',
                 args = c('-jar', jar_path,
                          cons_path,
                          cons_path),
                 stdout = T)
  eval(parse(text=res[length(res)]))
  n <- length(tree_paths) # number of trees
  logMap_dists <- matrix(nrow = (n+1), ncol = length(logMap))
  if (!is.null(other_cons_path)) {logMap_dists <- matrix(nrow = n+2, ncol = length(logMap))}
  logMap_dists[1,] <- logMap

  for (i in 2:(n+1)) {
    res <- system2('java',
                   args = c('-jar', jar_path,
                            cons_path,
                            tree_paths[i-1]),
                   stdout = T)
    eval(parse(text=res[length(res)]))
    logMap_dists[i,] <- logMap
  }
  if (!is.null(other_cons_path)) {
    res <- system2('java',
                   args = c('-jar', jar_path,
                            cons_path,
                            other_cons_path),
                   stdout = T)
    eval(parse(text=res[length(res)]))
    logMap_dists[dim(logMap_dists)[1],] <- logMap
  }
  return(logMap_dists)
}
