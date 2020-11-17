#' Plot logmap
#' Plot logmap coordinates using the first two principal components.
#'
#' @param vectors A matrix of logmap vectors for each tree.
#' @param cons_name The name of the consensus tree.
#' @param other_cons_name An optional other consensus tree name.
#' @param gene_names An optional list of gene names. If no names are given, numbers will be used
#' to identify trees.
#' @param col An optional list of gene attributes to color the points of the scatterplot by.
#'
#' @return A ggplot object.
#'
#'
#' @export
plot_logmap <- function(vectors, cons_name, other_cons_name = NULL, gene_names = NULL, col = NULL) {
  pca <- stats::prcomp(vectors, rank. = 2)
  if (is.null(gene_names)) {
    gene_names <- 1:(nrow(vectors)-1)
  }
  n <- length(gene_names) + 1
  pca_gene <- data.frame(dim1 = pca$x[2:n,1],
                         dim2 = pca$x[2:n,2],
                         name = gene_names)
  pca_consen <- data.frame(dim1 = pca$x[1,1],
                           dim2 = pca$x[1,2],
                           name = paste0(cons_name," Tree"))
  title <- paste0('Log Map of Gene Trees with Respect to ', cons_name, ' Tree')
  pca_plot <- ggplot(pca_gene, aes(x = dim1, y = dim2, Gene = name)) +
    geom_point(color = "black") +
    geom_point(data = pca_consen, color = "red") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
    xlab("First Principal Component") + ylab("Second Principal Component")
  if (!is.null(col)) {
    pca_gene$gradient <- col
    pca_plot <- ggplot(pca_gene, aes(x = dim1, y = dim2, Gene = name, color = gradient)) +
      geom_point() +
      geom_point(data = pca_consen, color = "red") +
      ggtitle(title) + labs(color = "Missingness")
      theme(plot.title = element_text(hjust = 0.5, size = 12)) +
      xlab("First Principal Component") + ylab("Second Principal Component")
    pca_consen$gradient <- NA
  }
  if (!is.null(other_cons_name)) {
    pca_consen1 <- data.frame(dim1 = pca$x[n+1,1],
                              dim2 = pca$x[n+1,2],
                              name = paste0(other_cons_name," Tree"))
    pca_plot <- ggplot(pca_gene, aes(x = dim1, y = dim2, Gene = name)) +
      geom_point(aes(color = "black")) +
      geom_point(data = pca_consen1, aes(color = "green")) +
      geom_point(data = pca_consen, aes(color = "red")) +
      ggtitle(title) +
      scale_colour_manual(name = 'Tree Type',
                          values =c('black'='black','green' = 'green','red'='red'),
                          labels = c('Gene Tree',other_cons_name, cons_name))
    theme(plot.title = element_text(hjust = 0.5, size = 12)) +
      xlab("First Principal Component") + ylab("Second Principal Component")
  }
  return(list(df = rbind(pca_gene, pca_consen), plot = pca_plot))
}
