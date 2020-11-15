#' Geodesic implementation
#' Calculates the geodesic distance between each tree
#'
#' @param tree_path The file path to a txt file containing phylogenetic trees in a multiPhylo object.
#'
#' @export
compute_geodesic <- function(tree_path) {
  output <- paste0("-o gtp_output.txt")
  #tree_path <- paste0("data/simple_bhv/example",seed,".txt")
  #code_path <- system.file("inst/java", "gtp.jar", package = "TreeVizPackage")
  #print(code_path)
  system2('java',
          args = c('-jar', "inst/java/gtp.jar", "-d", output,
                   tree_path),
          stdout = T)
}
