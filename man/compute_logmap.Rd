% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_logmap.R
\name{compute_logmap}
\alias{compute_logmap}
\title{Compute logmap
Compute the logmap centered at one tree for all other trees.}
\usage{
compute_logmap(
  cons_path,
  tree_paths,
  jar_path = NULL,
  other_cons_path = NULL,
  add_pendant_branches = FALSE,
  cons_tree = NULL,
  trees_complete = NULL,
  cons_in_tree_paths = FALSE
)
}
\arguments{
\item{cons_path}{The file path to the tree to center the log map.}

\item{tree_paths}{A list of file paths to all other trees.}

\item{jar_path}{A optional file path to the jar file to compute the log map. Otherwise,
the logmap.jar file in the TreeVizPackage repo will be used.}

\item{other_cons_path}{An optional additional consensus tree to plot.}

\item{add_pendant_branches}{A boolean. When true, the pendant branch lengths will be added
to the log map coordinates.}

\item{cons_tree}{The consensus tree. This is required when \code{add_pendant_branches = TRUE}.}

\item{trees_complete}{Complete list of trees in a multiPhylo object including the base tree.
This is required when \code{add_pendant_branches = TRUE}.}

\item{cons_in_tree_paths}{A logical value, TRUE if the consensus tree is included in the tree
paths vector.}
}
\value{
A matrix with log map coordinates for all trees.
}
\description{
Compute logmap
Compute the logmap centered at one tree for all other trees.
}
