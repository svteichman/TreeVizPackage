#' Distance MDS of six difference tree distances
#' Performs distance MDS for a set of trees using six different distances with the SMACOF algorithm.
#'  Add names and references later.
#'
#' @param trees A list of phylogenetic trees. If list includes a consensus tree, it should be the
#' first tree.
#' @param tree_names An optional list of gene names to add to the dataframes.
#' @param consensus An optional argument specifying whether the first tree is a consensus tree, and
#' if so the name of the consensus tree.
#'
#' @return A list containing two dataframes. The first, of dimension \eqn{2 \cdot (n_tree\cdot n_dist)},
#'  contains the first two MDS coordinates for each tree and distance combination. The second, of dimension
#'  \eqn{(n_tree -1)\cdot n_dist} contains the distances from the first tree to each of the other trees for
#'  each distance.
#'
#' @export
perform_smacof <- function(trees, tree_names = NULL, consensus = NULL) {
  n <- length(trees)
  BHV <- treespace::treespace(trees, method = "BHV", nf = 2)
  BHV_MDS <- smacof::mds(BHV$D)
  BHV_MDS1 <- BHV_MDS$conf[,1]
  BHV_MDS2 <- BHV_MDS$conf[,2]
  KC <- treespace::treespace(trees, method = "treeVec", nf = 2)
  KC_MDS <- smacof::mds(KC$D)
  KC_MDS1 <- KC_MDS$conf[,1]
  KC_MDS2 <- KC_MDS$conf[,2]
  KF <- treespace::treespace(trees, method = "KF", nf = 2)
  KF_MDS <- smacof::mds(KF$D)
  KF_MDS1 <- KF_MDS$conf[,1]
  KF_MDS2 <- KF_MDS$conf[,2]
  nNodes <- treespace::treespace(trees, method = "nNodes", nf = 2)
  nNodes_MDS <- smacof::mds(nNodes$D)
  nNodes_MDS1 <- nNodes_MDS$conf[,1]
  nNodes_MDS2 <- nNodes_MDS$conf[,2]
  Patristic <- treespace::treespace(trees, method = "patristic", nf = 2)
  Patristic_MDS <- smacof::mds(Patristic$D)
  Patristic_MDS1 <- Patristic_MDS$conf[,1]
  Patristic_MDS2 <- Patristic_MDS$conf[,2]
  RF <- treespace::treespace(trees, method = "RF", nf = 2)
  RF_MDS <- smacof::mds(RF$D)
  RF_MDS1 <- RF_MDS$conf[,1]
  RF_MDS2 <- RF_MDS$conf[,2]
  plot_df <- data.frame(MDS1 = c(BHV_MDS1, KC_MDS1, KF_MDS1,
                                 nNodes_MDS1, Patristic_MDS1, RF_MDS1),
                        MDS2 = c(BHV_MDS2, KC_MDS2, KF_MDS2,
                                 nNodes_MDS2, Patristic_MDS2, RF_MDS2),
                        method = c(rep("BHV",n),rep("KC",n),rep("KF",n),
                                   rep("nNodes",n),rep("Patristic",n), rep("RF",n)),
                        tree_type = rep("gene", n*6))
  dist_df <- data.frame(BHV = as.matrix(BHV$D)[1,2:n],
                        KC = as.matrix(KC$D)[1,2:n],
                        KF = as.matrix(KF$D)[1,2:n],
                        nNodes = as.matrix(nNodes$D)[1,2:n],
                        Patristic = as.matrix(Patristic$D)[1,2:n],
                        RF = as.matrix(RF$D)[1,2:n])
  if (!is.null(tree_names)) {
    plot_df$tree_name <- tree_names
    dist_df$tree_name <- tree_names[2:n]
  }
  if (!is.null(consensus)) {
    plot_df$tree_type <- rep(c(consensus,rep("gene", (n-1))), 6)
  }
  return(list(plot_df, dist_df))
}
