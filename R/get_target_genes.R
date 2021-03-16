#' Lists genes without missingness
#' Takes a list of genes and returns the list with genes removed that are missing one or more tips.
#'
#' @param gene_names A list of gene names that correspond with alignments to examine
#' @param path The file path to the gene alignments, default is "
#' @param tail The extension to the gene alignments, default is ".fa
#'
#' @return A subset of the gene_names list only including names of genes without missingness.
#'
#'
#' @export
get_target_genes <- function(gene_names, path = "", tail = ".fa") {

  # initializing the vector we'll fill as we find the good ones
  target_genes <- vector()

  # iterating through genes
  for ( curr_gene in gene_names ) {

    # setting path to alignment fasta
    curr_fasta <- paste0(path, curr_gene, tail)

    # getting length of current gene's alignment
    curr_alignment_length <- nchar(readLines(curr_fasta, n = 2)[2])

    # adding gene to target gene list if there are no entries that are all gaps
    if ( ! any(readLines(curr_fasta) == paste0(rep("-", curr_alignment_length), collapse = "")) ) {
      target_genes <- c(target_genes, curr_gene)
    }

  }

  return(target_genes)
}
