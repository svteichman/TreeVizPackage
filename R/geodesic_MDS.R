#' Geodesic implementation
#' Calculates the geodesic distance between each tree
#'
#' @param tree_path The file path to a txt file containing phylogenetic trees in a multiPhylo object.
#' @param nf The number of axes in the MDS to keep.
#'
#' @return A list containing an object 'dist_mat' containing geodesic distances between each phylogenetic tree
#' in the multiPhylo object, and an object 'df' containing the desired number of axes in the MDS of the trees.
#' @export
geodesic_MDS <- function(tree_path, nf = 2, names = NULL) {
  dist_mat <- compute_geodesic(tree_path)
  pco <- ade4::dudi.pco(as.dist(dist_mat), nf = nf, scannf = "FALSE")
  df <- data.frame(matrix(data = NA, nrow = nrow(dist_mat), ncol = nf + 1))
  if (is.null(names)) {
    df[, 1] <- paste0("tree", 1:nrow(dist_mat))
  } else {
    df[, 1] <- names
  }
  for (i in 1:nf) {
    df[, i + 1] <- pco$tab[, paste0("A", i)]
  }
  return(list("dist_mat" = dist_mat, "df" = df))
}
