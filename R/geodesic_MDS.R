#' Geodesic implementation
#' Calculates the geodesic distance between each tree
#'
#' @param tree_path The file path to a txt file containing phylogenetic trees in a multiPhylo object.
#' @param nf The number of axes in the MDS to keep.
#'
#' @return A list containing an object 'dist_mat' containing geodesic distances between each phylogenetic tree
#' in the multiPhylo object, and an object 'df' containing the desired number of axes in the MDS of the trees.
#' @export
geodesic_MDS <- function(tree_path, nf = 2, names = NULL, consensus = "consensus") {
  dist_mat <- compute_geodesic(tree_path)
  pco <- ade4::dudi.pco(as.dist(dist_mat), nf = nf, scannf = "FALSE")
  df <- data.frame(matrix(data = NA, nrow = nrow(dist_mat), ncol = nf + 2))
  if (is.null(names)) {
    df[, 1] <- paste0("tree", 1:nrow(dist_mat))
  } else {
    df[, 1] <- names
  }
  df[, 2] <- rep("gene", nrow(dist_mat))
  if (!is.null(consensus)) {
    df[1, 2] <- consensus
  }
  for (i in 1:nf) {
    df[, i + 2] <- pco$tab[, paste0("A", i)]
  }
  names(df) <- c("tree_name", "tree_type", paste0("MDS",1:nf))
  return(list("dist_mat" = dist_mat, "df" = df))
}
