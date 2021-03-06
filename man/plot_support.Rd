% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_support.R
\name{plot_support}
\alias{plot_support}
\title{Plot support
Plot a tree with support values on each internal branch.}
\usage{
plot_support(
  main_tree,
  n_trees,
  branch_support,
  rooted = TRUE,
  boot = FALSE,
  lab_size = 2,
  supp_size = 2,
  xlim_max = 0.5
)
}
\arguments{
\item{main_tree}{A tree to check support for.}

\item{n_trees}{The number of trees providing support for main_tree.}

\item{branch_support}{A list of branch support values for each internal branch of main_tree.}

\item{rooted}{A boolean, TRUE if the main tree is rooted}

\item{boot}{A logical value, TRUE is the support values are bootstrap support.}

\item{lab_size}{The size of the tip labels, default is 2.}

\item{supp_size}{The size of the support labels, default is 2.}

\item{xlim_max}{The length of the x axis.}
}
\value{
A ggtree object.
}
\description{
Plot support
Plot a tree with support values on each internal branch.
}
